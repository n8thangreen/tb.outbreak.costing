
#############################
# posterior forest plots


library(ggplot2)
library(grid)
library(gridExtra)


dat <- read.csv(here::here("input_data/cleaned_data.csv"), check.names = FALSE)
load(file = here::here("input_data/BUGS_output.RData"))

R2WinBUGS::attach.bugs(res_bugs$BUGSoutput)

grid <- 
  expand.grid(
    levels(as.factor(dat$year)),
    levels(as.factor(dat$setting)))
grid[,"names"] <- paste(grid[,"Var1"], grid[,"Var2"])

mcmcplots::caterplot(res_bugs, parms = c("p_ltbi"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")
mcmcplots::caterplot(res_bugs, parms = c("p_screen"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")
mcmcplots::caterplot(res_bugs, parms = c("rate_id"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")
mcmcplots::caterplot(res_bugs, parms = c("rate_inc"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")

# setting only
par(mfrow = c(3,2))
mcmcplots::caterplot(res_bugs, parms = c("sp_ltbi"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-0.1,0.3))
title("prob ltbi")
mcmcplots::caterplot(res_bugs, parms = c("sp_screen"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(0.5,1.1))
title("prob screen")
mcmcplots::caterplot(res_bugs, parms = c("srate_id"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-100,400))
title("n identify")
mcmcplots::caterplot(res_bugs, parms = c("srate_inc"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-0.1,12))
title("n incident")
mcmcplots::caterplot(res_bugs, parms = c("pred_n_screen"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-0.10,300))
title("n screen")
mcmcplots::caterplot(res_bugs, parms = c("pred_n_ltbi"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-4,20))
title("n ltbi")


###########################
# bespoke clearer plots

stan_forest_plot(res_bugs, param = "p_ltbi")
ggsave("plots/p_ltbi_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "p_screen")
ggsave("plots/p_screen_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "rate_id") + xlab("Number identified")
ggsave("plots/rate_id_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "rate_inc") + xlab("Number of incidents")
ggsave("plots/rate_inc_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)


## setting only
fp_ltbi <- stan_forest_plot_setting(res_bugs, param = "sp_ltbi", title = "Probability LTBI")
fp_screen <- stan_forest_plot_setting(res_bugs, param = "sp_screen", title = "Probability Screened")
fp_id <- stan_forest_plot_setting(res_bugs, param = "srate_id", title = "Identification rate")
fp_inc <- stan_forest_plot_setting(res_bugs, param = "srate_inc", title = "Incident rate")
fp_nscreen <- stan_forest_plot_setting(res_bugs, param = "pred_n_screen", title = "Number screened")
fp_nltbi <- stan_forest_plot_setting(res_bugs, param = "pred_n_ltbi", title = "Number LTBI")


fp <- grid_arrange_shared_legend(fp_ltbi, fp_screen, fp_id, fp_inc, fp_nscreen, fp_nltbi, nrow = 2, ncol = 3)

ggsave(plot = fp, here::here("plots/forest_plot_setting.png"), width = 40, height = 30, units = "cm", dpi = 640)

