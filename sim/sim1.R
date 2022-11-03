
rm(list=ls())

library(future.apply)
library(data.table)
library(ggplot2)

source("sim/sim_functions.R")

# Settings ----
nrep = 1000
Ns = c(1000, 2000, 5000, 10000)
a1s = c(-3, -1.5, 0)
a2s = seq(.1, 1, .1)

# Run sim ----
  
sim1_all = expand.grid('N' = Ns, 'a1' = a1s, 'a2' = a2s, 'res' = NA)

sim1_all$file = with(sim1_all, paste0("sim/sim1-res/n", N, "_a1", a1, "_a2", a2, ".rds"))

done = list.files("sim/sim1-res", full.names = T)

# remove the done ones
sim1 = subset(sim1_all, !file %in% done)

plan(multisession, workers = availableCores())
for (i in seq_len(nrow(sim1))){
  res = list(future_replicate(nrep, run_sim(N  = sim1$N[i], 
                                                    a1 = sim1$a1[i], 
                                                    a2 = sim1$a2[i], 
                                                    a3 = NULL)
  )
  )
  
  saveRDS(object = list(
    'N' = sim1$N[i],
    'a1' = sim1$a1[i],
    'a2' = sim1$a2[i],
    'res' = res
  ), file = sim1$file[i])
  
  cat("Simulation", i, "of", nrow(sim1), "done...\n")
  rm(res)
}

if (!all(sim1_all$files %in% list.files("sim/sim1-res", full.names = T))){
  warning("Not done yet")
}

# Calculate means etc ----

res_files = list.files("sim/sim1-res", full.names = T)

res = future_lapply(res_files, function(x){
  format_res(res = readRDS(x))
  })

res = rbindlist(res)

res$metric = factor(res$metric, levels = c("ICI", "E50", "E90", "E_max", "nk"))

theme_set(theme_bw())

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("loess", "lowess", "rcs", "lrcs") & model == "Correct") |>
  ggplot(aes(x=a2, y=av_score, color=method)) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ a1 + N, scales = "free_y")

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("loess", "lowess", "rcs", "lrcs") & model == "Misspecified") |>
  ggplot(aes(x=a2, y=av_score, color=method)) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ a1 + N, scales = "free_y")

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("loess", "lowess", "rcs", "lrcs") & model == "Difference") |>
  ggplot(aes(x=a2, y=av_score, color=method)) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ a1 + N, scales = "free_y")

# just the rcs methods

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("rcs", "lrcs") & 
         model == "Correct" & a1 == 0) |>
  ggplot(aes(x=a2, y=av_score, color=method)) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ N, scales = "free_y")

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("rcs", "lrcs") & 
         model == "Misspecified" & a1 == 0) |>
  ggplot(aes(x=a2, y=av_score, color=method)) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ N, scales = "free_y")

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("rcs", "lrcs") & 
         model == "Difference" & a1 == 0) |>
  ggplot(aes(x=a2, y=av_score, color=method)) + 
  geom_point() + geom_line() +
  facet_grid(metric ~ N, scales = "free_y")



## recreate plots from Austin & Steyerberg
# figures 4,5,6
subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("loess", "lowess", "rcs") & 
         model == "Correct" & N %in% c(1000, 2000, 5000) &
         a1 == 0) |>
  ggplot(aes(x=a2, y=av_score, color=as.factor(N), shape=method)) + 
  geom_point() + geom_line() +
  facet_wrap(~metric, scales = "free_y")

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("loess", "lowess", "rcs") & 
         model == "Misspecified" & N %in% c(1000, 2000, 5000) &
         a1 == 0) |>
  ggplot(aes(x=a2, y=av_score, color=as.factor(N), shape=method)) + 
  geom_point() + geom_line() +
  facet_wrap(~metric, scales = "free_y")

subset(res, metric %in% c("ICI", "E50", "E90", "E_max") & 
         method %in% c("loess", "lowess", "rcs") & 
         model == "Difference" & N %in% c(1000, 2000, 5000) &
         a1 == 0) |>
  ggplot(aes(x=a2, y=av_score, color=as.factor(N), shape=method)) + 
  geom_point() + geom_line() +
  facet_wrap(~metric, scales = "free_y")
