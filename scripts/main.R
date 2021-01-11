
# PSA
# duplicating Excel model for comparison
# calculate total cost for randomly sampled
# counts


library(dplyr)
library(purrr)

source("functions.R")
source("VBA_converted.R")
source("model_data.R")


out <- vector(mode = "list",
              length = 5)

for (j in 1:5){
  
  out[[j]][1] <- NA

  for (i in 2:1000){
    
  out[[j]][i] <-
    total_year_cost(
      inc_sample = sample_inc[j,i],
      id_per_inc = sample_id[j,i],
      screen_per_inc = sample_id[j,i]*sample_screen[j,i],
      ltbi_per_inc = sample_id[j,i]*sample_screen[j,i]*sample_ltbi[j,i])
  }
}

saveRDS(out, file = here::here("data", "cost_boot_setting.Rds"))

par(mfrow = c(2,3))
map(out, hist, breaks = 20)
map(out, summary)
