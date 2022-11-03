
library(future.apply)
library(data.table)
library(ggplot2)

source("sim/sim_functions.R")

## settings 
simulation = 1
wh = c(6.5, 6.5) # plot width/height

theme_set(theme_bw())

path = paste0("sim/sim", simulation, "-res")

res_files = list.files(path, full.names = T)

plan(multisession, workers = availableCores())
res = future_lapply(res_files, function(x){
  format_res(res = readRDS(x))
})

res = rbindlist(res)

res = subset(res, method %in% c("loess", "lowess", "rcs", "lrcs"))

res$metric = factor(res$metric, levels = c("ICI", "E50", "E90", "E_max", "nk"), labels = c("ICI", "E50", "E90", "E max", "nk"))
res$method2 = factor(res$method, levels = c('loess', 'lowess', 'rcs', 'lrcs'), 
                     labels = c("loess", "lowess", "RCS", "RCS + logit(p)"))

if (simulation == 1){
  setnames(res, old = "a2", new = "a")
  xlab = bquote(a[2]~(quadratic~coefficient))
} else if (simulation == 2){
  setnames(res, old = "a3", new = "a")
  xlab = bquote(a[3]~(interaction~coefficient))
} else{
  stop("simulation should be 1 or 2")
}

# all methods (incl lowess and loess)
for (m in c("Correct", "Misspecified", "Difference")){
  # separate plots for each N
  for (n in c(1000, 2000, 5000, 10000)){
    plot = subset(res, metric %in% c("ICI", "E50", "E90", "E max") & 
                    method %in% c("loess", "lowess", "rcs", "lrcs") &
                    N == n &
                    model == m) |>
      ggplot(aes(x=a, y=av_score, shape=method2, color=method2)) + 
      geom_point() + geom_line() +
      facet_grid(metric ~ a1 , scales = "free_y", switch = 'y', 
                 labeller = label_bquote(cols = a[1]~"="~.(a1))) +
      theme(strip.background = element_blank(), strip.placement = "outside", 
            strip.text.y.left = element_text(angle = 0), axis.title.y = element_blank(),
            legend.position = "bottom") + 
      labs(shape="Method", color="Method", y="", x=xlab) + 
      scale_x_continuous(breaks = seq(.2, 1, .2), minor_breaks = seq(.1,1,.1)) + expand_limits(y=0)
    
    plot 
    
    ggsave(plot = plot, filename = paste0("sim/plots/sim", simulation, "-all-N", n, "-", m, ".pdf"), 
           width = wh[1], height = wh[2])
  }
}


# just the rcs methods
for (m in c("Correct", "Misspecified", "Difference")){
  plot = subset(res, metric %in% c("ICI", "E50", "E90", "E max") & 
                  method %in% c("rcs", "lrcs") & 
                  model == m) |>
    ggplot(aes(x=a, y=av_score, shape=method2, color=as.factor(N))) + 
    geom_point() + geom_line() +
    facet_grid(metric ~ a1, scales = "free_y", switch = 'y', 
               labeller = label_bquote(cols = a[1]~"="~.(a1))) +
    theme(strip.background = element_blank(), strip.placement = "outside", 
          strip.text.y.left = element_text(angle = 0), axis.title.y = element_blank(),
          legend.position = "bottom") + 
    labs(color="N", shape="Method", y="", x=xlab) + 
    scale_x_continuous(breaks = seq(.2, 1, .2), minor_breaks = seq(.1,1,.1)) + expand_limits(y=0)
  
  plot 
  
  ggsave(plot = plot, filename = paste0("sim/plots/sim", simulation, "-rcs-", m, ".pdf"), 
         width = wh[1], height = wh[2])
}

## average
av_res = res[metric %in% c("ICI", "E50", "E90", "E max") & method %in% c("loess", "lowess", "rcs", "lrcs"), list("avav"=mean(av_score)), by=c("method2", "model", 'metric')]

for (m in c("Correct", "Misspecified", "Difference")){
  plot = ggplot(subset(av_res, model == m), aes(x = method2, y = avav, fill=method2)) + 
    geom_bar(stat="identity") + facet_wrap(~metric, scales = "free_y") + 
    theme(strip.background = element_blank(), strip.placement = "outside", 
          legend.position = "none") + labs(y="Average across all simulations", x="")
  
  plot
  
  ggsave(plot = plot, filename = paste0("sim/plots/sim", simulation, "-average-", m, ".pdf"))
  
}

# probability of favoring the correct model

plot = subset(res, metric %in% c("ICI", "E50", "E90", "E max") & 
         method %in% c("rcs", "lrcs") & 
         model == "Difference") |>
  ggplot(aes(x=a, y=pl0, shape=method2, color=as.factor(N))) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ a1, scales = "free_y", switch = 'y', 
             labeller = label_bquote(cols = a[1]~"="~.(a1))) +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        strip.text.y.left = element_text(angle = 0), #axis.title.y = element_blank(),
        legend.position = "bottom") + 
  labs(color="N", shape="Method", y="p(correct < miscalibrated)", x=xlab) + 
  scale_x_continuous(breaks = seq(.2, 1, .2), minor_breaks = seq(.1,1,.1)) #+ expand_limits(y=0)

ggsave(plot = plot, filename = paste0("sim/plots/sim", simulation, "-pcorrect.pdf"), 
       width = wh[1]*1.1, height = wh[2])


## recreate plots from Austin & Steyerberg
AS_plots = list()
for (m in c("Correct", "Misspecified", "Difference")){
  AS_plots[[m]] = subset(res, metric %in% c("ICI", "E50", "E90", "E max") & 
                           method %in% c("loess", "lowess", "rcs") & 
                           model == m & N %in% c(1000, 2000, 5000) &
                           a1 == 0) |>
    ggplot(aes(x=a, y=av_score, color=as.factor(N), shape=method2)) + 
    geom_point() + geom_line() +
    facet_wrap(~metric, scales = "free_y", strip.position = "left") + 
    theme(strip.background = element_blank(), strip.placement = "outside", 
          strip.text.y.left = element_text(angle = 0), axis.title.y = element_blank(),
          legend.position = "bottom") + 
    labs(color="N", shape="Method", y="", x=xlab)
  
}

AS_plots$Correct
AS_plots$Misspecified
AS_plots$Difference
