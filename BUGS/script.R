
# run BUGS model script


library(R2jags)

data()


dat <-
  dat %>% 
  ungroup() %>% 
  mutate(n = 1:n())

inc <- dcast(dat, year ~ setting, value.var = "Latent")

n_S <- nrow(data_obs$ICD)  # number of states

r.1 <-
  rbind(
    cbind(data_risk6$ICD[, 1:n_S], empty_mat),
    cbind(empty_mat, data_risk6$low_risk[, 1:n_S]))

n.1 <- unname(rowSums(r.1))

scale <- 1                                 # level of informativeness for
alpha.0 <- c(rep(scale, n_S), rep(0, n_S)) # the Dirichlet prior
alpha.1 <- c(rep(0, n_S), rep(scale, n_S)) # with structural zeros

dataJags <-
  list(n.0 = n.0,
       n.1 = n.1,
       r.0 = r.0,
       r.1 = r.1,
       from_shock = c(1,0,0,0,0,0),
       from_ICD_death = c(0,0,1,0,0,0),
       alpha.0 = alpha.0,
       alpha.1 = alpha.1)

filein <- "BUGS/model.txt"
params <- c("lambda.0", "lambda.1") #probabilities

n.iter <- 10000
n.burnin <- 5000
n.thin <- floor((n.iter - n.burnin)/500)

mm1 <-
  jags(data = dataJags,
       inits = inits,
       parameters.to.save = params,
       model.file = filein,
       n.chains = 2,
       n.iter,
       n.burnin,
       n.thin,
       DIC = TRUE)

R2WinBUGS::attach.bugs(mm1$BUGSoutput)

save(mm1, file = "data/jags_output.RData")
saveRDS(lambda.1, file = "data/lambda1.Rds")

# print(mm1, digits = 3, intervals = c(0.025, 0.975))

