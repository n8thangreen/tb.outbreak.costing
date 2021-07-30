
# run BUGS model script


library(R2jags)

dat <- read.csv("data/cleaned_data.csv", check.names = FALSE)

inc <-
  dcast(dat, year ~ setting, value.var = "Latent") %>% 
  select(-year)

dataJags <-
  list(inc = inc,
       id = dat$`Total No identified`,
       screen = dat$`Total No Screened`,
       ltbi = dat$Latent,
       yr = as.numeric(as.factor(dat$year)),
       set = as.numeric(as.factor(dat$setting)),
       n_yr = length(unique(dat$year)),
       n_set = length(unique(dat$setting)),
       n_inc = nrow(dat))

inits <-
  list(list())

filein <- "BUGS/model.txt"
params <- c("rate_inc", "rate_id", "p_screen", "p_ltbi",
            "srate_inc", "srate_id", "sp_screen", "sp_ltbi",
            "pred_n_screen", "pred_n_ltbi")

n.iter <- 20000
n.burnin <- 1000
n.thin <- floor((n.iter - n.burnin)/500)

res_bugs <-
  jags(data = dataJags,
       inits = NULL,
       parameters.to.save = params,
       model.file = filein,
       n.chains = 1,
       n.iter,
       n.burnin,
       n.thin,
       DIC = TRUE)

R2WinBUGS::attach.bugs(res_bugs$BUGSoutput)
out <- res_bugs$BUGSoutput

save(res_bugs, file = "data/BUGS_output.RData")


############################
# plots

##TODO: improve these plots
##      use the ggplot functions from mcmc stan work

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


