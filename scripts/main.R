
# PSA
# duplicating Excel model for comparison
# calculate total cost for randomly sampled
# counts


library(dplyr)
library(purrr)

source("scripts/model_data.R")

sample_inc <- write.csv(sample_ltbi, file = here::here("data", "sample_inc_mean.csv"))
sample_id <- write.csv(sample_ltbi, file = here::here("data", "sample_id_mean.csv"))
psample_screen <- write.csv(sample_ltbi, file = here::here("data", "sample_screen_mean.csv"))
psample_ltbi <- write.csv(sample_ltbi, file = here::here("data", "sample_ltbi_mean.csv"))

num_settings <- 5
out <- vector(mode = "list",
              length = num_settings)

for (j in seq_len(num_settings)){
  
  out[[j]][1] <- NA

  for (i in 2:1000){
    
  out[[j]][i] <-
    total_year_cost(
      inc_sample = sample_inc[j, i],
      id_per_inc = sample_id[j, i],
      screen_per_inc = sample_id[j, i]*psample_screen[j, i],
      ltbi_per_inc = sample_id[j, i]*psample_screen[j, i]*psample_ltbi[j, i])
  }
}

saveRDS(out, file = here::here("data", "cost_boot_setting.Rds"))

par(mfrow = c(2,3))
map(out, hist, breaks = 20)
map(out, summary)
