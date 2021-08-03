
# tables


library(MCMCvis)

load("data/BUGS_output.RData")

num_settings <- 5
mcmc_dat <- res_bugs$BUGSoutput$sims.list

######################
# population

tab_pop <- 
  cbind(
    transmute(MCMCsummary(
      res_bugs,
      params = "srate_inc",
      round = 3)[, c("mean", "sd")],
      n_incidents = paste0(mean, " (", sd, ")")),
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

# MCMCtrace(res_bugs, 
#           ISB = FALSE, 
#           exact = TRUE,
#           pdf = FALSE)

write.csv(tab_pop, file = "output_data/pop_summary_table.csv")

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

write.csv(tab_cost, file = "output_data/cost_summary_table.csv")

