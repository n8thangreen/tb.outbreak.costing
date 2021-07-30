
# PSA_with_BUGS.R:
# calculate total cost using BUGS output
# for probabilities and counts
# duplicating Excel model for comparison


library(dplyr)
library(purrr)

# load parameter values
source("scripts/model_data.R")
load("data/BUGS_output.RData")

# output list by setting
num_settings <- 5
out <- vector(mode = "list",
              length = num_settings)
mcmc_dat <- res_bugs$BUGSoutput$sims.list

n_samples <- res_bugs$BUGSoutput$n.sims

for (s in seq_len(num_settings)) {
  for (i in 1:n_samples) {
    
  out[[s]][i] <-
    total_year_cost(
      inc_sample = mcmc_dat$srate_inc[i,s],
      id_per_inc = mcmc_dat$srate_id[i,s],
      screen_per_inc = mcmc_dat$pred_n_screen[i,s],
      ltbi_per_inc = mcmc_dat$pred_n_ltbi[i,s])
  }
}

dat <- read.csv("data/cleaned_data.csv", check.names = FALSE)

names(out) <- levels(as.factor(dat$setting))

saveRDS(out, file = here::here("data", "cost_BUGS_setting.Rds"))


##########
# output #
##########

library(ggplot2)

c_samples_by_setting <-
  do.call(cbind.data.frame, out) %>%
  melt(value.name = "cost",
       variable.name = "setting")

ggplot(c_samples_by_setting, aes(x = cost)) +
  facet_wrap(~setting, scales="free") +
  geom_histogram(color="black", fill="white", binwidth = 6e3) +
  xlim(0,2e5)

ggsave(filename = "plots/posterior_setting_cost_hist.png",
       width = 20, height = 20, units = "cm")

