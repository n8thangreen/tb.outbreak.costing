# ============================================
# Poisson correlated TB intensities
# ============================================

library(R2jags)
library(dplyr)
library(reshape2)
library(rstan)
library(here)
library(readr)

# dat <- read.csv("C:/Users/john1/Downloads/cleaned_data.csv")  # dont keep data here
dat <- read_csv(here("data", "cleaned_data.csv"))

# model code with no random effects on the probs
model_code <- "
data {
  int<lower=1> M;                      // Total number of observations
  int<lower=1> L;                      // Total number of locations
  int<lower=1> J;                      // Total number of settings
  
  array[M] int<lower=0> Y;             // Observed counts
  array[M] int<lower=1, upper=J> S;    // Setting index for each observation m
  array[M] int<lower=1, upper=L> Z;    // Location index for each observation m
}

parameters {
  vector[M] log_lambda;           // Total intensity per setting j
  // top level: setting means
  vector[J] log_lambda_s;

  // middle level: setting-year means
  array[J, L] real log_lambda_st;
  
   // standard deviations for the Normal hierarchy
  vector<lower=0>[J] sigma_st;
  array[J, L] real<lower=0> sigma_obs;
}

model {
  // Priors
  // top level prior
  log_lambda_s ~ normal(0, 3);
  for (j in 1:J){
    for (l in 1:L){
      log_lambda_st[j, l] ~ normal(log_lambda_s[j], sigma_st[j]);
      sigma_obs[j,l] ~ normal(0, 3);
    }
  }
  // bottom level: Normal around lambda_st
  for (m in 1:M){
    log_lambda[m] ~ normal(log_lambda_st[S[m], Z[m]], sigma_obs[S[m], Z[m]]);
  }
  // Likelihood
  Y ~ poisson(exp(log_lambda));
  
  // priors on SDs
  sigma_st  ~ normal(0, 3);
}
"


# Enable parallel processing for Stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

Y <- dat$Total.No.identified
J <- length(unique(dat$setting))
L <- length(unique(dat$year))
M <- length(Y)
dat$setting_num <- as.numeric(factor(dat$setting))
dat$year_num <- as.numeric(factor(dat$year))

S = dat$setting_num
Z = dat$year_num
SCR <- dat$Total.No.Screened
LTBI <- dat$Latent

stan_data <- list(
  Y = Y,
  J = J,
  L = L,
  M = M,
  S = S,
  Z = Z
)

fit <- stan(
  model_code = model_code,
  data = stan_data,
  iter = 6000,
  warmup = 5000,
  chains = 2,
  seed = 42
)

posterior = extract(fit)

true_y = c()
RMSE_y = c()

for(i in 1:dim(posterior$lambda)[2]){
  RMSE_y[i] = sqrt(mean((posterior$lambda[,i] - Y[i])^2))
  x_1 = quantile(posterior$lambda[,i],0.025)
  x_2 = quantile(posterior$lambda[,i],0.975)
  true_y[i] = x_1< Y[i] & x_2>Y[i]
}

plot(density(apply(posterior$lambda[,stan_data$S == 5 & stan_data$Z == 7],1,sum)
))
abline(v = sum(dat$Total.No.identified[dat$year == 2019 & dat$setting == "other"]))

plot(density((posterior$lambda[,stan_data$S == 5])
))
lines(density(dat$Total.No.identified[dat$setting == "other"]))


# model code with no random effects on the probs
model_code <- "
data {
  int<lower=1> M;                      // Total number of observations
  int<lower=1> L;                      // Total number of locations
  int<lower=1> J;                      // Total number of settings
  
  array[M] int<lower=0> Y;             // Observed counts
  array[M] int<lower=1, upper=J> S;    // Setting index for each observation m
  array[M] int<lower=1, upper=L> Z;    // Location index for each observation m
}

parameters {
  vector[M] log_lambda_raw;           // Total intensity per setting j
  // top level: setting means
  vector[J] log_lambda_s_raw;

  // middle level: setting-year means
  array[J,L] real log_lambda_st_raw;
  
   // standard deviations for the Normal hierarchy
  vector<lower=0>[J] sigma_st;
  array[J, L] real<lower=0> sigma_obs;
}

transformed parameters {
  vector[J] log_lambda_s;
  array[J,L] real log_lambda_st;
  vector[M] log_lambda;
  
  log_lambda_s = log_lambda_s_raw * 3;
 
  // Middle level: log_lambda_st[j,l] = log_lambda_s[j] + σ_st[j,l] * raw
  for (j in 1:J){
    for (l in 1:L){
      log_lambda_st[j,l] = log_lambda_s[j] + log_lambda_st_raw[j,l] * sigma_st[j];
    }
  }
  
  for (m in 1:M){
    log_lambda[m] = log_lambda_st[S[m], Z[m]] + sigma_obs[S[m], Z[m]] * log_lambda_raw[m];
  }
}

model {
  // Priors
  // top level prior
  log_lambda_s_raw ~ normal(0, 1);
  
  for (j in 1:J){
    for (l in 1:L){
      log_lambda_st_raw[j,l] ~ normal(0, 1);
      sigma_obs[j,l] ~ normal(0, 3);

    }
  }
  log_lambda_raw ~ normal(0,1);
  
  // Likelihood
  Y ~ poisson(exp(log_lambda));
  
  // priors on SDs
  sigma_st  ~ normal(0, 3);
}
"

posterior = extract(fit)

posterior$lambda = exp(posterior$log_lambda)
true_y = c()
RMSE_y = c()

for(i in 1:dim(posterior$lambda)[2]){
  RMSE_y[i] = sqrt(mean((posterior$lambda[,i] - Y[i])^2))
  x_1 = quantile(posterior$lambda[,i],0.025)
  x_2 = quantile(posterior$lambda[,i],0.975)
  true_y[i] = x_1< Y[i] & x_2>Y[i]
}

#
posterior$lambda[,stan_data$S == 5 & stan_data$Z == 7] |> 
  apply(1, sum) |> 
  density() |> 
  plot()

abline(v = sum(dat$Total.No.identified[dat$year == 2019 & dat$setting == "other"]))

#
posterior$lambda[, stan_data$S == 5] |> 
  density() |> 
  plot()

lines(density(dat$Total.No.identified[dat$setting == "other"]))


