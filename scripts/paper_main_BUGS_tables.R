
# tables for paper using BUGS output

library(MCMCvis)
library(dplyr)

load(here::here("input_data/BUGS_output.RData"))

num_settings <- 5
mcmc_dat <- res_bugs$BUGSoutput$sims.list


############################################
# population counts per incident
# for each subgroup

tab_pop_inc <- 
  cbind(
    transmute(MCMCsummary(
      res_bugs,
      params = "srate_id",
      round = 2)[, c("mean", "2.5%", "97.5%")],
      n_identify = paste0(mean, " (", `2.5%`, "-", `97.5%`, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "pred_n_screen",
      round = 2)[, c("mean", "2.5%", "97.5%")],
      n_screen = paste0(mean, " (", `2.5%`, "-", `97.5%`, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "sp_screen",
      round = 2)[, c("mean", "2.5%", "97.5%")],
      p_screen = paste0(mean, " (", `2.5%`, "-", `97.5%`, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "pred_n_ltbi",
      round = 2)[, c("mean", "2.5%", "97.5%")],
      n_ltbi = paste0(mean, " (", `2.5%`, "-", `97.5%`, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "sp_ltbi",
      round = 2)[, c("mean", "2.5%", "97.5%")],
      p_ltbi = paste0(mean, " (", `2.5%`, "-", `97.5%`, ")")))

write.csv(tab_pop_inc, file = here::here("output_data/pop_summary_table_per_inc.csv"))


############################################
# population counts per year
# for each subgroup

##TODO: not strictly correct because should sample srate_inc number of number id
##      at the moment we use the same number id

mean_id <- apply(mcmc_dat$srate_inc * mcmc_dat$srate_id, 2, mean)
sd_id <- apply(mcmc_dat$srate_inc * mcmc_dat$srate_id, 2, sd)
quantiles_id <- apply(mcmc_dat$srate_inc * mcmc_dat$srate_id, 2, quantile, probs = c(0.025, 0.975))

mean_screen <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_screen, 2, mean)
sd_screen <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_screen, 2, sd)
quantiles_screen <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_screen, 2, quantile, probs = c(0.025, 0.975))

mean_ltbi <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_ltbi, 2, mean)
sd_ltbi <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_ltbi, 2, sd)
quantiles_ltbi <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_ltbi, 2, quantile, probs = c(0.025, 0.975))

tab_pop <- 
  cbind(
    transmute(MCMCsummary(
      res_bugs,
      params = "srate_inc", round = 2)[, c("mean", "sd")],
      n_incidents = paste0(mean, " (", round(mean - 1.96*sd,2), "-", round(mean + 1.96*sd,2), ")")),
    n_identify = paste0(round(mean_id,2), " (", round(quantiles_id[1,],2), "-", round(quantiles_id[2,],2), ")"),
    n_screen = paste0(round(mean_screen,2), " (", round(quantiles_screen[1,],2), "-", round(quantiles_screen[2,],2), ")"),
    n_ltbi = paste0(round(mean_ltbi,2), " (", round(quantiles_ltbi[1,],2), "-", round(quantiles_ltbi[2,],2), ")")
    )

write.csv(tab_pop, file = here::here("output_data/pop_summary_table.csv"))


###################
# costs

out_total <- readRDS(here::here("input_data", "cost_BUGS_setting.Rds"))
out_per_inc <- readRDS(here::here("input_data", "cost_BUGS_setting_per_inc.Rds"))

c_per_inc <- do.call(cbind, out_per_inc)
c_total <- do.call(cbind, out_total)

tab_cost <- 
  data.frame(
    cost_total = paste0(round(apply(c_total, 2, mean),0), " (",
                        round(apply(c_total, 2, sd),0), ")"),
    cost_per_inc = paste0(round(apply(c_per_inc, 2, mean),0), " (",
                          round(apply(c_per_inc, 2, sd),0), ")"),
    pp_id = paste0(round(apply(c_per_inc/mcmc_dat$srate_id, 2, mean),0), " (",
                   round(apply(c_per_inc/mcmc_dat$srate_id, 2, sd),0), ")"),
    pp_screen = paste0(round(apply(c_per_inc/mcmc_dat$pred_n_screen, 2, mean),0), " (",
                       round(apply(c_per_inc/mcmc_dat$pred_n_screen, 2, sd),0), ")"),
    pp_ltbi = paste0(round(apply(c_per_inc/mcmc_dat$pred_n_ltbi, 2, mean),0), " (",
                     round(apply(c_per_inc/mcmc_dat$pred_n_ltbi, 2, sd),0), ")"))

tab_cost

write.csv(tab_cost, file = here::here("output_data", "cost_summary_table.csv"))

