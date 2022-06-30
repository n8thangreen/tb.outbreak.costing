
# PSA_with_BUGS.R:
# probabilistic sensitivity analysis
# calculate total cost using BUGS output
# for probabilities and counts
# duplicating Excel model for comparison


library(dplyr)
library(purrr)
library(ggplot2)
library(reshape2)


# load parameter values
source(here::here("scripts/model_data.R"))
load(here::here("input_data/BUGS_output.RData"))

dat <-
  read.csv(
    here::here("input_data/cleaned_data.csv"), check.names = FALSE)

# output list by setting
num_settings <- 5
mcmc_dat <- res_bugs$BUGSoutput$sims.list
n_samples <- res_bugs$BUGSoutput$n.sims

out_total <- vector(mode = "list",
                    length = num_settings)
out_per_inc <- vector(mode = "list",
                      length = num_settings)
cost_per_ltbi <- vector(mode = "list",
                        length = num_settings)

for (s in seq_len(num_settings)) {
  for (i in 1:n_samples) {
    
    ## year totals
    out_total[[s]][i] <-
      total_year_cost(
        inc_sample = mcmc_dat$srate_inc[i,s],
        id_per_inc = mcmc_dat$srate_id[i,s],
        screen_per_inc = mcmc_dat$pred_n_screen[i,s],
        ltbi_per_inc = mcmc_dat$pred_n_ltbi[i,s])
    
    ## per incident
    out_per_inc[[s]][i] <-
      total_year_cost(
        inc_sample = 1,
        id_per_inc = mcmc_dat$srate_id[i,s],
        screen_per_inc = mcmc_dat$pred_n_screen[i,s],
        ltbi_per_inc = mcmc_dat$pred_n_ltbi[i,s])
    
    cost_per_ltbi[[s]][i] <-
      out_per_inc[[s]][i]/mcmc_dat$pred_n_ltbi[i,s]
  }
}
names(out_total) <- levels(as.factor(dat$setting))
names(out_per_inc) <- levels(as.factor(dat$setting))
names(cost_per_ltbi) <- levels(as.factor(dat$setting))

saveRDS(out_total, file = here::here("input_data", "cost_BUGS_setting.Rds"))
saveRDS(out_per_inc, file = here::here("input_data", "cost_BUGS_setting_per_inc.Rds"))


##########
# output #
##########
# out <- load(here::here("input_data", "cost_BUGS_setting.Rds"))

# long format for plotting
c_samples_by_setting <-
  do.call(cbind.data.frame, out_total) %>%
  melt(value.name = "cost",
       variable.name = "setting")

# histogram total cost per setting
ggplot(c_samples_by_setting, aes(x = cost)) +
  facet_wrap(~setting, scales = "free") +
  # geom_histogram(color = "black", fill = "white", binwidth = 6e3) +
  geom_histogram(aes(y = ..density..),  colour = 1, fill = "white") +
  geom_density() #c(0.7, 0.8))

ggsave(filename = here::here("plots/posterior_setting_cost_hist_per_ltbi.png"),
       width = 20, height = 20, units = "cm")


#########################################
# cost-effectiveness planes

library(BCEA)

bcea_ltbi <- 
  bcea(as.matrix(cbind(0, -mcmc_dat$pred_n_ltbi)),
       as.matrix(cbind(0, -do.call(cbind.data.frame, out_total))),
       interventions = c("", names(out_total)))

ceplane.plot(bcea_ltbi, graph = "ggplot2",
             xlim = c(0, 10), ylim = c(0, 100000))

bcea_per_inc <- 
  bcea(as.matrix(cbind(0, -mcmc_dat$pred_n_ltbi)),
       as.matrix(cbind(0, -do.call(cbind.data.frame, out_per_inc))),
       interventions = c("", names(out_total)))

ceplane.plot(bcea_per_inc, graph = "ggplot2",
             xlim = c(0, 10), ylim = c(0, 15000))
