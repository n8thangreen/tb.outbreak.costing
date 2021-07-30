
# run BUGS model script


library(R2jags)

dat <- read.csv("data/cleaned_data.csv", check.names = FALSE)

##TODO: confirm what this should be and move to cleaning script
# row 132 too many latent. should be 1?

dat <-
  mutate(dat, Latent = pmin(`Total No Screened`, Latent))

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
params <- c("rate_inc", "rate_id", "p_screen", "p_ltbi")

n.iter <- 10000
n.burnin <- 500
n.thin <- floor((n.iter - n.burnin)/500)

mm1 <-
  jags(data = dataJags,
       inits = inits,
       parameters.to.save = params,
       model.file = filein,
       n.chains = 1,
       n.iter,
       n.burnin,
       n.thin,
       DIC = TRUE)

R2WinBUGS::attach.bugs(mm1$BUGSoutput)

out <- mm1$BUGSoutput


###########################
# output

##TODO: improve these plots
##      use the ggplot functions from mcmc stan work

grid <- 
  expand.grid(
    levels(as.factor(dat$year)),
    levels(as.factor(dat$setting)))
grid[,"names"] <- paste(grid[,"Var1"], grid[,"Var2"])

mcmcplots::caterplot(mm1, parms = c("p_ltbi"), reorder = FALSE, labels = grid$names, labels.loc = "above")
mcmcplots::caterplot(mm1, parms = c("p_screen"), reorder = FALSE, labels = grid$names, labels.loc = "above")
mcmcplots::caterplot(mm1, parms = c("rate_id"), reorder = FALSE, labels = grid$names, labels.loc = "above")
mcmcplots::caterplot(mm1, parms = c("rate_inc"), reorder = FALSE, labels = grid$names, labels.loc = "above")

save(mm1, file = "data/jags_output.RData")

