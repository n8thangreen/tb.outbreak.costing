
#########
# plots #
#########


##TODO: improve these plots
##      use the ggplot functions from mcmc stan work

library(ggplot2)

load(file = "data/BUGS_output.RData")

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

stan_forest_plot(res_bugs, parms = "p_ltbi")
stan_forest_plot(res_bugs, parms = "p_screen")
stan_forest_plot(res_bugs, parms = "rate_id")
stan_forest_plot(res_bugs, parms = "rate_inc")

