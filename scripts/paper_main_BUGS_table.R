
#


load("data/BUGS_output.RData")

num_settings <- 5
mcmc_dat <- res_bugs$BUGSoutput$sims.list

x <- print(res_bugs, )


library(MCMCvis)

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



