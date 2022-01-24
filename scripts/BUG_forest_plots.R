
#########
# plots #
#########


##TODO: improve these plots
##      use the ggplot functions from mcmc stan work

library(ggplot2)

dat <- read.csv("data/cleaned_data.csv", check.names = FALSE)
load(file = "data/BUGS_output.RData")

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

#######################

stan_forest_plot(res_bugs, param = "p_ltbi")
ggsave("plots/p_ltbi_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "p_screen")
ggsave("plots/p_screen_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "rate_id") + xlab("Number identified")
ggsave("plots/rate_id_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "rate_inc") + xlab("Number of incidents")
ggsave("plots/rate_inc_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

