
rm(list=ls())

library(future.apply)
library(data.table)
library(ggplot2)

source("sim/sim_functions.R")

# Settings ----
nrep = 1000
Ns = c(1000, 2000, 5000, 10000)
a1s = c(-3, -1.5, 0)
a3s = seq(.1, 1, .1)

# Run sim ----

sim2_all = expand.grid('N' = Ns, 'a1' = a1s, 'a3' = a3s, 'res' = NA)

sim2_all$file = with(sim2_all, paste0("sim/sim2-res/n", N, "_a1", a1, "_a3", a3, ".rds"))

done = list.files("sim/sim2-res", full.names = T)

# remove the done ones
sim2 = subset(sim2_all, !file %in% done)

plan(multisession, workers = availableCores())
for (i in seq_len(nrow(sim2))){
  res = list(future_replicate(nrep, run_sim(N  = sim2$N[i], 
                                            a1 = sim2$a1[i], 
                                            a3 = sim2$a3[i], 
                                            a2 = NULL)
  )
  )
  
  saveRDS(object = list(
    'N' = sim2$N[i],
    'a1' = sim2$a1[i],
    'a3' = sim2$a3[i],
    'res' = res
  ), file = sim2$file[i])
  
  cat("Simulation", i, "of", nrow(sim2), "done...\n")
  rm(res)
}

if (!all(sim2_all$files %in% list.files("sim/sim2-res", full.names = T))){
  warning("Not done yet")
}
