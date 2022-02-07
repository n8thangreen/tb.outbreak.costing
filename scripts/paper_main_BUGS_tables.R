
# tables for paper


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
      round = 3)[, c("mean", "sd")],
      n_identify = paste0(mean, " (", sd, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "pred_n_screen",
      round = 3)[, c("mean", "sd")],
      n_screen = paste0(mean, " (", sd, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "sp_screen",
      round = 3)[, c("mean", "sd")],
      p_screen = paste0(mean, " (", sd, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "pred_n_ltbi",
      round = 3)[, c("mean", "sd")],
      n_ltbi = paste0(mean, " (", sd, ")")),
    transmute(MCMCsummary(
      res_bugs,
      params = "sp_ltbi",
      round = 3)[, c("mean", "sd")],
      p_ltbi = paste0(mean, " (", sd, ")")))

write.csv(tab_pop_inc, file = here::here("output_data/pop_summary_table_per_inc.csv"))


############################################
# population counts per year
# for each subgroup

##TODO: not strictly correct because should sample srate_inc number of number id
##      at the moment we use the same number id

mean_id <- apply(mcmc_dat$srate_inc * mcmc_dat$srate_id, 2, mean)
sd_id <- apply(mcmc_dat$srate_inc * mcmc_dat$srate_id, 2, sd)
mean_screen <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_screen, 2, mean)
sd_screen <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_screen, 2, sd)
mean_ltbi <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_ltbi, 2, mean)
sd_ltbi <- apply(mcmc_dat$srate_inc * mcmc_dat$pred_n_ltbi, 2, sd)

tab_pop <- 
  cbind(
    transmute(MCMCsummary(
      res_bugs,
      params = "srate_inc",
      round = 3)[, c("mean", "sd")],
      n_incidents = paste0(mean, " (", sd, ")")),
    n_identify = paste0(round(mean_id,3), " (", round(sd_id,3), ")"),
    n_screen = paste0(round(mean_screen,3), " (", round(sd_screen,3), ")"),
    n_screen = paste0(round(mean_ltbi,3), " (", round(sd_ltbi,3), ")")
    )

write.csv(tab_pop, file = here::here("output_data/pop_summary_table.csv"))


###################
# costs

c_dat <- do.call(cbind, out)

tab_cost <- 
  data.frame(
    total = paste0(round(apply(c_dat, 2, mean),0), " (",
                   round(apply(c_dat, 2, sd),0), ")"),
    pp_id = paste0(round(apply(c_dat/mcmc_dat$srate_id, 2, mean),0), " (",
                   round(apply(c_dat/mcmc_dat$srate_id, 2, sd),0), ")"),
    pp_screen = paste0(round(apply(c_dat/mcmc_dat$pred_n_screen, 2, mean),0), " (",
                       round(apply(c_dat/mcmc_dat$pred_n_screen, 2, sd),0), ")"),
    pp_ltbi = paste0(round(apply(c_dat/mcmc_dat$pred_n_ltbi, 2, mean),0), " (",
                     round(apply(c_dat/mcmc_dat$pred_n_ltbi, 2, sd),0), ")"))

write.csv(tab_cost, file = here::here("output_data/cost_summary_table.csv"))

