# ============================================
# Poisson correlated TB intensities
# ============================================

library(R2jags)
library(dplyr)
library(reshape2)
library(rstan)
library(here)
library(readr)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2)


# dat <- read.csv("C:/Users/john1/Downloads/cleaned_data.csv")  # dont keep data here
dat <- read_csv(here("input_data", "cleaned_data.csv"))

# model code with no random effects on the probs
model_code <- "
data {
  int<lower=1> M;                      // Total number of observations  //NG: incident?
  int<lower=1> L;                      // Total number of locations     //NG: year?
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
  // Priors //
  
  // top level prior
  log_lambda_s ~ normal(0, 3);
  
  for (j in 1:J){ // setting
    for (l in 1:L){ //  location
      log_lambda_st[j, l] ~ normal(log_lambda_s[j], sigma_st[j]);
      sigma_obs[j,l] ~ normal(0, 3);
    }
  }
  
  // bottom level: Normal around lambda_st
  for (m in 1:M){ // observation
    log_lambda[m] ~ normal(log_lambda_st[S[m], Z[m]], sigma_obs[S[m], Z[m]]);
  }
  
  // Likelihood //
  
  Y ~ poisson(exp(log_lambda));
  
  // priors on SDs
  sigma_st  ~ normal(0, 3);
}
"


# Enable parallel processing for Stan
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

Y <- dat$`Total No identified`
J <- length(unique(dat$setting))
L <- length(unique(dat$year))
M <- length(Y)

dat$setting_num <- as.numeric(factor(dat$setting))
dat$year_num <- as.numeric(factor(dat$year))

S = dat$setting_num
Z = dat$year_num
SCR <- dat$`Total No Screened`
LTBI <- dat$Latent

stan_data <- list(
  Y = Y,
  J = J,
  L = L,
  M = M,
  S = S,
  Z = Z
)

fit <- rstan::stan(
  model_code = model_code,
  data = stan_data,
  iter = 1000,
  warmup = 1000,
  chains = 2,
  seed = 42,
  verbose = TRUE
)

posterior = rstan::extract(fit)

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


# -----------------
# cmdstanr version

# temporary .stan file
stan_file <- write_stan_file(model_code)

# compile model
mod_nested <- cmdstan_model(stan_file)

fit <- mod_nested$sample(
  data = stan_data,
  seed = 42,
  chains = 2,
  parallel_chains = 2, 
  iter_warmup = 1000,
  iter_sampling = 1000
)

# fit$summary()

# #  convert to rstan format
# stanfit_object <- rstan::read_stan_csv(fit$output_files())


# 1. Extract log_lambda from the old model fit as a matrix
# Rows = MCMC iterations, Columns = M observations
log_lambda_mat <- fit$draws(variables = "log_lambda", format = "matrix")

# 2. Exponentiate to convert from log-intensity to the expected count (lambda)
lambda_mat <- exp(log_lambda_mat)

# 3. Setup the plotting window for two plots side-by-side
par(mfrow = c(1, 2))

# ---------------------------------------------------------
# Plot 1: Sum for Setting 5, Year (Location) 7
# ---------------------------------------------------------
# Find the columns corresponding to Setting 5 and Year 7
idx_5_7 <- which(stan_data$S == 5 & stan_data$Z == 7)

# Sum the lambda values across those specific observations for every iteration
sum_lambda_5_7 <- rowSums(lambda_mat[, idx_5_7, drop = FALSE])

plot(density(sum_lambda_5_7), 
     main = "Posterior Sum of lambda\n(Setting 5, Year 7)", 
     xlab = "Expected Count", 
     lwd = 2)
     # xlim = c(0, 150))

abline(v = sum(dat$`Total No identified`[dat$year == 2019 & dat$setting == "other"]), 
       col = "red", lwd = 2, lty = 2)

# ---------------------------------------------------------
# Plot 2: Density of all observations in Setting 5 vs Data
# ---------------------------------------------------------
# Find all columns corresponding to Setting 5 across all years
idx_5 <- which(stan_data$S == 5)

# Flatten the matrix subset into a single vector to plot the overall density
plot(density(as.numeric(lambda_mat[, idx_5])), 
     main = "Density: Model vs Observed\n(Setting = 'other')", 
     xlab = "Expected Count", 
     lwd = 2, col = "darkgreen")

lines(density(dat$`Total No identified`[dat$setting == "other"]), 
      col = "red", lwd = 2, lty = 2)

# Reset plotting window
par(mfrow = c(1, 1))

##########################
# new plots

color_scheme_set("brightblue")

# 1. Ensure we have the lambda matrix from the old model
log_lambda_mat <- fit$draws(variables = "log_lambda", format = "matrix")
lambda_mat <- exp(log_lambda_mat)

# 2. Simulate Y_rep (replicated datasets) using the Poisson distribution
# This creates a matrix of identical dimensions (Iterations x Observations)
set.seed(123) 
Y_rep_old <- matrix(rpois(length(lambda_mat), lambda_mat), 
                    nrow = nrow(lambda_mat), 
                    ncol = ncol(lambda_mat))

# 3. Extract the observed counts for the plotting functions
Y <- stan_data$Y

# ---------------------------------------------------------
# Posterior Predictive Check Plots
# ---------------------------------------------------------

# A. Density Overlay
# Checks if the overall shape of the simulated counts matches the observed data.
# We plot the true Y against the first 100 simulated datasets to avoid over-cluttering.
ppc_dens_overlay(y = Y, yrep = Y_rep_old[1:100, ]) + 
  ggtitle("Old Model: Distribution of Counts")

# B. Proportion of Zeros Check
# Clinical/epidemiological data often has more zeros than a Poisson expects. 
# This checks if your model generates the correct proportion of zero counts.
prop_zero <- function(x) mean(x == 0)
ppc_stat(y = Y, yrep = Y_rep_old, stat = "prop_zero") + 
  ggtitle("Old Model: Proportion of Zeros")

# C. Maximum Value Check
# Checks if the model is capturing the extreme high counts (heavy tails).
ppc_stat(y = Y, yrep = Y_rep_old, stat = "max") + 
  ggtitle("Old Model: Maximum Count")

# D. Rootogram
# A specialized plot for count data showing the exact deviation between 
# observed and expected counts per bin. Hanging bars below the zero line 
# mean the model underpredicts that specific count.
ppc_rootogram(y = Y, yrep = Y_rep_old) +
  ggtitle("Old Model: Rootogram")


# If you want the plots labeled with the actual setting names (e.g., "other")
ppc_dens_overlay_grouped(
  y = Y, 
  yrep = Y_rep_old[1:100, ], 
  group = dat$setting 
) + 
  ggtitle("Old Model: Distribution of Counts by Setting") +
  xlim(0, 500)

# Check the proportion of zeros broken down by each setting
ppc_stat_grouped(
  y = Y, 
  yrep = Y_rep_old, 
  group = dat$setting, 
  stat = "prop_zero"
) + 
  ggtitle("Old Model: Proportion of Zeros by Setting")

# Check the maximum predicted count broken down by each setting
ppc_stat_grouped(
  y = Y, 
  yrep = Y_rep_old, 
  group = dat$setting, 
  stat = "max"
) + 
  ggtitle("Old Model: Maximum Count by Setting")














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


#########################
# cross design with OLRE

cross_olre_model <- "
data {
  int<lower=1> M;                      // Total number of observations
  int<lower=1> L;                      // Total number of years
  int<lower=1> J;                      // Total number of settings
  
  array[M] int<lower=0> Y;             // Observed counts
  array[M] int<lower=1, upper=J> S;    // Setting index for each observation m
  array[M] int<lower=1, upper=L> Z;    // Year index for each observation m
}

parameters {
  real mu;                             // Global intercept
  
  // Raw parameters for non-centered parameterization
  vector[L] a_raw;                     // Year effects (raw)
  vector[J] b_raw;                     // Setting effects (raw)
  vector[M] epsilon_raw;               // Observation-level effects (raw)
  
  // Standard deviations for the hierarchies
  real<lower=0> sigma_a;               // SD for year effects
  real<lower=0> sigma_b;               // SD for setting effects
  real<lower=0> sigma_obs;             // SD for observation-level error (OLRE)
}

transformed parameters {
  vector[L] a;                         // Year effects
  vector[J] b;                         // Setting effects
  vector[M] log_lambda;                // Log-intensity for each observation
  
  // Non-centered transformations (scaling raw standard normals by their SD)
  a = a_raw * sigma_a;
  b = b_raw * sigma_b;
  
  // Crossed structure + OLRE
  for (m in 1:M) {
    log_lambda[m] = mu + a[Z[m]] + b[S[m]] + (epsilon_raw[m] * sigma_obs);
  }
}

model {
  // Priors
  mu ~ normal(0, 5);                   // Weakly informative prior on global mean
  
  // Standard normal priors for the raw parameters (Matt trick)
  a_raw ~ std_normal();
  b_raw ~ std_normal();
  epsilon_raw ~ std_normal();
  
  // Half-normal priors on standard deviations (implicitly truncated at 0)
  sigma_a ~ normal(0, 3);
  sigma_b ~ normal(0, 3);
  sigma_obs ~ normal(0, 3);
  
  // Likelihood
  Y ~ poisson_log(log_lambda);         
}
"

# temporary .stan file
stan_file <- write_stan_file(cross_olre_model)

# compile model
mod_cross_olre <- cmdstan_model(stan_file)

fit <- mod_cross_olre$sample(
  data = stan_data,
  seed = 42,
  chains = 2,
  parallel_chains = 2, 
  iter_warmup = 1000,
  iter_sampling = 1000
)

# fit$summary()

# 1. Ensure we have the lambda matrix from the old model
log_lambda_mat <- fit$draws(variables = "log_lambda", format = "matrix")
lambda_mat <- exp(log_lambda_mat)

# 2. Simulate Y_rep (replicated datasets) using the Poisson distribution
# This creates a matrix of identical dimensions (Iterations x Observations)
set.seed(123) 
Y_rep_old <- matrix(rpois(length(lambda_mat), lambda_mat), 
                    nrow = nrow(lambda_mat), 
                    ncol = ncol(lambda_mat))

# 3. Extract the observed counts for the plotting functions
Y <- stan_data$Y

# ---------------------------------------------------------
# Posterior Predictive Check Plots
# ---------------------------------------------------------

# A. Density Overlay
# Checks if the overall shape of the simulated counts matches the observed data.
# We plot the true Y against the first 100 simulated datasets to avoid over-cluttering.
ppc_dens_overlay(y = Y, yrep = Y_rep_old[1:100, ]) + 
  ggtitle("Old Model: Distribution of Counts")



##################
# mixture model

mix_model <- "
data {
  int<lower=1> M;                      
  int<lower=1> L;                      
  int<lower=1> J;                      
  
  array[M] int<lower=0> Y;             
  array[M] int<lower=1, upper=J> S;    
  array[M] int<lower=1, upper=L> Z;    
}

parameters {
  vector<lower=0, upper=1>[J] theta; 
  
  // Small events (Poisson-lognormal + OLRE)
  vector[J] log_lambda_s;
  array[J, L] real log_lambda_st_raw;  
  vector<lower=0>[J] sigma_st;
  vector[M] log_lambda_raw;            
  vector<lower=0>[J] sigma_obs;
  
  // Large events: Simple, direct setting-specific means with strong priors
  vector<lower=0, upper=1000>[J] mu_large;   
  real<lower=5> phi_large;       
}

transformed parameters {
  array[J, L] real log_lambda_st;
  vector[M] log_lambda_small;
  
  for (j in 1:J) {
    for (l in 1:L) {
      log_lambda_st[j, l] = log_lambda_s[j] + log_lambda_st_raw[j, l] * sigma_st[j];
    }
  }
  
  for (m in 1:M) {
    log_lambda_small[m] = log_lambda_st[S[m], Z[m]] + sigma_obs[S[m]] * log_lambda_raw[m];
  }
}

model {
  theta ~ beta(1, 9); 
  
  // Strong, explicit setting-specific priors based on your rules:
  // Assuming alphabetical setting order: 
  // 1: commercial, 2: education, 3: factory/workplace, 4: hospital, 5: other
  mu_large[1] ~ normal(150, 10);  // commercial (~100-200)
  mu_large[2] ~ normal(400, 20);  // education (~400)
  mu_large[3] ~ normal(350, 10);  // factory/workplace (~400)
  mu_large[4] ~ normal(150, 10);  // hospital (~100-200)
  mu_large[5] ~ normal(150, 20);  // other (~100-200)
  
  phi_large ~ gamma(100, 1);           
  
  log_lambda_s ~ normal(0, 2.5);
  sigma_st ~ normal(0, 2);
  
  for (j in 1:J) {
    for (l in 1:L) {
      log_lambda_st_raw[j, l] ~ std_normal();
    }
    sigma_obs[j] ~ normal(0, 0.8); 
  }
  
  log_lambda_raw ~ std_normal();
  
  for (m in 1:M) {
    target += log_mix(
      theta[S[m]], 
      neg_binomial_2_lpmf(Y[m] | mu_large[S[m]], phi_large), 
      poisson_log_lpmf(Y[m] | log_lambda_small[m])          
    );
  }
}
"

# temporary .stan file
stan_file <- write_stan_file(mix_model)

# compile model
mod <- cmdstan_model(stan_file)

fit_mix <- mod$sample(
  data = stan_data,
  seed = 42,
  chains = 2,
  parallel_chains = 2, 
  iter_warmup = 1000,
  iter_sampling = 1000
)

log_lambda_small_mat <- fit_mix$draws(variables = "log_lambda_small", format = "matrix")
lambda_small_mat     <- exp(log_lambda_small_mat)

theta_mat     <- fit_mix$draws(variables = "theta", format = "matrix") 
mu_large_mat  <- fit_mix$draws(variables = "mu_large", format = "matrix") 
phi_large_vec <- as.numeric(fit_mix$draws(variables = "phi_large", format = "matrix"))

n_iter <- nrow(lambda_small_mat)
n_obs  <- ncol(lambda_small_mat)

set.seed(123) 

is_large_event <- matrix(FALSE, nrow = n_iter, ncol = n_obs)
for(m in 1:n_obs) {
  setting_idx <- stan_data$S[m]
  is_large_event[, m] <- runif(n_iter) < theta_mat[, setting_idx]
}

Y_rep_small <- matrix(rpois(n_iter * n_obs, lambda_small_mat), 
                      nrow = n_iter, ncol = n_obs)

mu_large_obs <- matrix(NA, nrow = n_iter, ncol = n_obs)
for(m in 1:n_obs) {
  mu_large_obs[, m] <- mu_large_mat[, stan_data$S[m]]
}

# Generate the large event draws
Y_rep_large <- matrix(rnbinom(n_iter * n_obs, 
                              size = rep(phi_large_vec, times = n_obs), 
                              mu = as.vector(mu_large_obs)), 
                      nrow = n_iter, ncol = n_obs)

# Hard-cap the extreme outliers so they don't distort the x-axis scale
Y_rep_large[Y_rep_large > 600] <- 600

# Combine
Y_rep_mix <- ifelse(is_large_event, Y_rep_large, Y_rep_small)

ppc_dens_overlay_grouped(
  y = stan_data$Y, 
  yrep = Y_rep_mix[1:100, ], 
  group = dat$setting 
) + 
  ggtitle("Simple Mixture Model: Counts by Setting") + xlim(0,750)
