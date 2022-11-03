
# faster than [q,p]logis
logit = binomial()$linkfun
invlogit = binomial()$linkinv

sim_dat = function(N, a1, a2=NULL, a3=NULL){
  # m1 = LP = a1 + x1 + a2*x1^2 # non-lin
  # m2 = LP = a1 + x1 + x2 + x1*x2*a3 # interaction
  
  x1 = rnorm(N)
  if (is.null(a3)){
    # m1 
    LP = a1 + x1 + a2*x1^2
    y = rbinom(n = N, size = 1, prob = invlogit(LP))
    d = data.frame(x1, y)
  } else{
    # m2
    if (!is.null(a2)) warning("value of a3 is given so a2 will be ignored...")
    x2 = rnorm(N)
    LP = a1 + x1 + x2 + x1*x2*a3
    y = rbinom(n = N, size = 1, prob = invlogit(LP))
    d = data.frame(x1, x2, y)
  }
  return(d)
}

loess_cal = function(p, y){
  if (length(p) > 5000) warning("loess with N > 5000")
  
  fit = loess(y ~ p)
  
  p_c = predict(fit, newdata = p)
  
  return(p_c)
}

#plot(p, loessCal(p,y))

lowess_cal = function(p, y){
  
  fit = lowess(x = p, y = y, iter = 0)
  
  # adapted from rms:::calibrate.default
  p_c = approx(x = fit$x, y = fit$y, xout=p, ties=function(x)x[1])$y
  
  return(p_c)
}

#plot(p, lowessCal(p,y))

# fit_rcs = function(p, y){
#   tryCatch(
#     # try 4 then 3 knots
#     {
#       pp = Hmisc::rcspline.eval(p, nk = 4, inclx = T)
#       #rms::lrm.fit(x = pp, y = y)
#       f = glm(y ~ pp, family = "binomial")
#       if (f$converged & !f$boundary){
#         f
#       } else{
#         stop("error")
#       }
#     },
#     error= function(e){
#       tryCatch({
#         pp = Hmisc::rcspline.eval(p, nk = 4, inclx = T)
#         #rms::lrm.fit(x = pp, y = y)
#         f = glm(y ~ pp, family = "binomial")
#         if (f$converged & !f$boundary){
#           f
#         } else{
#           stop("error")
#         }
#       },
#       error = function(ee){
#         rms::lrm(y ~ p)
#       }
#       )
#     })
# }

fit_rcs = function(p, y){
  
  pp = Hmisc::rcspline.eval(p, nk = 4, inclx = T)
  f = glm(y ~ pp, family = "binomial")
  if (!f$converged | f$boundary){
    pp = Hmisc::rcspline.eval(p, nk = 3, inclx = T)
    f = glm(y ~ pp, family = "binomial")
  }
  
  return(f)
}

rcs_cal = function(p, y, logit_trans=T){

  if (logit_trans){
    p = logit(p)
  }
  d = data.frame('p'=p, 'y'=y)
  fit = fit_rcs(p, y)
  
  #p_c = invlogit(predict(fit, p))
  #p_c = invlogit(fit$linear.predictors)
  p_c = predict(fit, type = "response")
  
  nk = ifelse(fit$converged & !fit$boundary, length(coef(fit)), 0)
  
  return(list("p_c"=p_c, "nk" = nk))
}

# plot(p, rcsCal(p, y)[[1]])
# plot(p, rcsCal(p, y, F)[[1]])
calibration_metrics <- function(p, p_c){
  # code modified from the Austin and Steyerberg (2019, SiM) paper
  stopifnot(length(p) == length(p_c))
  
  if (any(is.na(p_c) | is.infinite(p_c))){
    d = NA
    message("some p_c's are missing or Inf")
  } else{
    d = abs(p - p_c)
  }
 
  m = c('ICI' = mean(d), 
        'E50' = median(d), 
        'E90' = if (any(is.na(d))) NA else unname(quantile(d, .9)), 
        'E_max' = max(d))
  
  return(m)
}

cal_all = function(p, y){
  loe = calibration_metrics(p = p, p_c = loess_cal(p = p, y = y))
  low = calibration_metrics(p = p, p_c = lowess_cal(p = p, y = y))
  
  rcs = rcs_cal(p = p, y = y, logit_trans = F)
  lrcs = rcs_cal(p = p, y = y, logit_trans = T)
  
  out = data.frame(
    "loess" = c(loe, "nk" = NA),
    "lowess" = c(low, "nk" = NA),
    "rcs" = c(calibration_metrics(p = p, p_c = rcs$p_c), "nk" = rcs$nk),
    "lrcs" = c(calibration_metrics(p = p, p_c = lrcs$p_c), "nk" = lrcs$nk)
  )
  
  # differences
  out$loessVlowess = out$loess - out$lowess
  out$loessVrcs = out$loess - out$rcs
  out$lowessVrcs = out$lowess - out$rcs
  out$loessVlrcs = out$loess - out$lrcs
  out$lowessVlrcs = out$lowess - out$lrcs
  out$rcsVlrcs = out$rcs - out$lrcs
  
  return(out)
}

run_sim = function(N, a1, a2=NULL, a3=NULL){
  if (is.null(a2) & is.null(a3)) stop("One of a2 or a3 must be specified")
  
  d = sim_dat(N = N*2, a1 = a1, a2 = a2, a3=a3)
  dd = d[1:N, ]; vd = d[(N+1):(N*2), ] # development & validation data
  if (is.null(a3)){
    #cor = glm(y ~ x1 + I(x1^2), data = dd, family = "binomial")
    cor = glm(y ~ poly(x1, 2), data = dd, family = "binomial")
    mis = glm(y ~ x1, data = dd, family = "binomial")
  } else{
    # m2
    if (!is.null(a2)) warning("value of a3 is given so a2 will be ignored...")
    cor = glm(y ~ x1*x2, data = dd, family = "binomial")
    mis = glm(y ~ x1 + x2, data = dd, family = "binomial")
  }
  y = vd$y
  # predictions in validation set
  p_cor = predict.glm(cor, newdata = vd, type = "response")
  p_mis = predict.glm(mis, newdata = vd, type = "response")
  
  metrics = list("cor" = cal_all(p = p_cor, y = y),
                 "mis" = cal_all(p = p_mis, y = y)
  )
  
  return(metrics)
}

#run_sim(N=100000, a1 = -3, a2 = NULL, a3= .5)

# library(future.apply)
# plan(multisession, workers = 4)
# 
# out = future_replicate(1000, run_sim(N=1000, a1 = -3, a2 = NULL, a3= .5))
# 
# any(unlist(lapply(out[1, ], \(x) x[5, 3])) < 4)
# any(unlist(lapply(out[1, ], \(x) x[5, 4])) < 4)
# 
# any(is.na(unlist(lapply(out[1, ], \(x) x[1, 1]))))

format_res = function(res){
  x = res$res[[1]]
  
  dat = lapply(seq(dim(x)[2]), function(a){
    cor = x[["cor",a]]
    mis = x[["mis",a]]
    dif = cor - mis # dif > 0 -> cor worse than mis
    cns = colnames(cor)
    
    cor$metric = rownames(cor)
    cor = tidyr::gather(cor, key = method, value = value, all_of(cns))
    cor$model = "Correct"
    
    mis$metric = rownames(mis)
    mis = tidyr::gather(mis, key = method, value = value, all_of(cns))
    mis$model = "Misspecified"
    
    dif$metric = rownames(dif)
    dif = tidyr::gather(dif, key = method, value = value, all_of(cns))
    dif$model = "Difference"
    
    out = rbind(cor, mis, dif)
    out$run = a
    return(out)
  })
  
  dat = data.table::rbindlist(dat)
  
  # summarize
  dat_sum = dat[, list("av_score"=mean(value, na.rm=T), 
                       "med_score"=median(value, na.rm = T),
                       "q25_score" = quantile(na.omit(value), probs=.25),
                       "q75_score" = quantile(na.omit(value), probs=.75),
                       "pg0" = mean(value > 0, na.rm=T),
                       "pl0" = mean(value < 0, na.rm=T),
                       "N_missing" = sum(is.na(value))
  ), by=c("metric", "method", "model")]
  
  dat_sum[, N := res[["N"]]]
  dat_sum[, a1 := res[["a1"]]]
  dat_sum[, (names(res[3])) := res[[3]]] # 3rd slot is either a1 or a2
  
  return(dat_sum)
}

#x = format_res(res)
