
# run BUGS model script
# and forest plots


library(R2jags)
library(dplyr)
library(reshape2)

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

