library(R2jags)
library(dplyr)
library(reshape2)

#dat <- read.csv("data/cleaned_data.csv", check.names = FALSE)
dat <- read.csv("C:/Users/john1/Downloads/cleaned_data.csv")

library(rstan)

#model code with no random effects on the probs
model_code <- "
data {
  int<lower=1> M;                      // Total number of observations
  int<lower=1> L;                      // Total number of locations
  int<lower=1> J;                      // Total number of settings
  
  array[M] int<lower=0> Y;             // Observed counts
  array[M] int<lower=0> SCR;           // Sub-counts (must be <= Y[m])
  array[M] int<lower=0> LTBI;           // Sub-counts (must be <= Y[m])
  array[M] int<lower=1, upper=J> S;    // Setting index for each observation m
  array[M] int<lower=1, upper=L> Z;    // Location index for each observation m
}

parameters {
  vector<lower=0>[J] lambda;           // Total intensity per setting j
  array[J] simplex[L] theta;           // Location proportions per setting j
  
  // Setting-specific hyper-parameters for observation weights
  vector<lower=0>[J] alpha_w;          // Gamma shape parameter per setting j

  vector<lower=0>[M] w;                // Unnormalized observation weights
  
  // Binomial success probabilities per (setting j, location i)
  vector<lower=0, upper=1>[M] q; 
  
  // Binomial success probabilities per (setting j, location i)
  vector<lower=0, upper=1>[M] d; 
}

transformed parameters {
  vector<lower=0,upper=1>[M] p;                // Normalized observation probabilities
  vector<lower=0>[M] mu;               // Poisson rate for observation m

  matrix[J, L] sum_w = rep_matrix(0.0, J, L);

  for (m in 1:M) {
    sum_w[S[m], Z[m]] += w[m];
  }

  for (m in 1:M) {
    p[m] = w[m] / sum_w[S[m], Z[m]];
    mu[m] = lambda[S[m]] * theta[S[m]][Z[m]] * p[m];
  }
}

model {
  // Priors
  // Priors on intensity and location proportions
  lambda ~ gamma(0.001, 0.001);          
  for (j in 1:J) {
    theta[j] ~ dirichlet(rep_vector(1.0, L));
  }
  
  // Hyperpriors for setting-specific weight parameters
  alpha_w ~ gamma(0.001, 0.001);

  // Setting-specific prior for each observation weight w_m
  for (m in 1:M) {
    w[m] ~ gamma(alpha_w[S[m]], 1);
  }

  // 2. Prior on binomial detection/success probabilities q
  for(m in 1:M){
   q[m] ~ beta(1, 1);
  }
  
  // 2. Prior on binomial detection/success probabilities d
  for(m in 1:M){
   d[m] ~ beta(1, 1);
  }

  // Likelihood
  Y ~ poisson(mu);
  
    for (m in 1:M) {                     // Binomial thinning process
    SCR[m] ~ binomial(Y[m], q[m]);
    }
  for (m in 1:M) {                     // Binomial thinning process
    LTBI[m] ~ binomial(SCR[m], d[m]);
  }
  
  
}

"


library(rstan)
#model code with random effects on the probs
model_code <- "
data {
  int<lower=1> M;                      // Total number of observations
  int<lower=1> L;                      // Total number of locations
  int<lower=1> J;                      // Total number of settings
  
  array[M] int<lower=0> Y;             // Observed counts
  array[M] int<lower=0> SCR;           // Sub-counts (must be <= Y[m])
  array[M] int<lower=0> LTBI;           // Sub-counts (must be <= Y[m])
  array[M] int<lower=1, upper=J> S;    // Setting index for each observation m
  array[M] int<lower=1, upper=L> Z;    // Location index for each observation m
}

parameters {
  vector<lower=0>[J] lambda;           // Total intensity per setting j
  array[J] simplex[L] theta;           // Location proportions per setting j
  
  // Setting-specific hyper-parameters for observation weights
  vector<lower=0>[J] alpha_w;          // Gamma shape parameter per setting j

  vector<lower=0>[M] w;                // Unnormalized observation weights
  
  // Linear predictor parameters
  vector[J] v_q;                       // Setting effects for q
  vector[L] e_q;                       // Location effects for q
  
  vector[J] v_d;                       // Setting effects for d
  vector[L] e_d;                       // Location effects for d
  

}

transformed parameters {

  vector[M] logit_q;
  vector[M] logit_d;
  
  vector<lower=0,upper=1>[M] p;                // Normalized observation probabilities
  vector<lower=0>[M] mu;               // Poisson rate for observation m

  matrix[J, L] sum_w = rep_matrix(0.0, J, L);

  for (m in 1:M) {
    sum_w[S[m], Z[m]] += w[m];
  }

  for (m in 1:M) {
    
    p[m] = w[m] / sum_w[S[m], Z[m]];
    mu[m] = lambda[S[m]] * theta[S[m]][Z[m]] * p[m];
    
    logit_q[m] = v_q[S[m]] + e_q[Z[m]];
    logit_d[m] = v_d[S[m]] + e_d[Z[m]];

  }
}

model {
  // Priors on intensity and location proportions
  lambda ~ gamma(0.001, 0.001);          
  for (j in 1:J) {
    theta[j] ~ dirichlet(rep_vector(1.0, L));
  }
  
  // Hyperpriors for setting-specific weight parameters
  alpha_w ~ gamma(0.001, 0.001);

  // Setting-specific prior for each observation weight w_m
  for (m in 1:M) {
    w[m] ~ gamma(alpha_w[S[m]], 1);
  }

  // 2. Prior on binomial detection/success probabilities q
  v_q ~ normal(0, 1.81);
  e_q ~ normal(0, 1.81);  
  
  v_d ~ normal(0, 1.81);
  e_d ~ normal(0, 1.81);  
  

  // Likelihood
  Y ~ poisson(mu);
  
  for (m in 1:M) {                     // Binomial thinning process
    SCR[m] ~ binomial_logit(Y[m], logit_q[m]);
  }
  for (m in 1:M) {                     // Binomial thinning process
    LTBI[m] ~ binomial_logit(SCR[m], logit_d[m]);
  }
  
  
}

"


library(rstan)

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
  Z = Z,
  SCR = SCR,
  LTBI = LTBI
)


fit <- stan(
  model_code = model_code,
  data = stan_data,
  iter = 4000,
  warmup = 2000,
  chains = 4,
  seed = 42
)

# 1. Extract posterior draws from Stan fit object
posterior <- extract(fit)

# Extract dimensions
S_draws <- dim(posterior$lambda)[1] # Total number of posterior samples (e.g., 12000)
J       <- dim(posterior$lambda)[2] # Number of settings
L       <- dim(posterior$theta)[3]  # Number of locations
M       <- dim(posterior$mu)[2]     # Number of observations
# ==============================================================================
# A. Observation-Level Predictive Counts: Y_sim (Matrix: S_draws x M)
# ==============================================================================
# Vectorized Poisson sampling using posterior mu
Y_sim <- matrix(
  rpois(n = S_draws * M, lambda = posterior$mu),
  nrow = S_draws,
  ncol = M
)

# ==============================================================================
# B. Setting-Level Total Counts: N_sim (Matrix: S_draws x J)
# ==============================================================================
# Vectorized Poisson sampling using posterior lambda
N_sim <- matrix(
  rpois(n = S_draws * J, lambda = posterior$lambda),
  nrow = S_draws,
  ncol = J
)

# ==============================================================================
# C. Location-Level Counts per Setting: n_sim (Array: S_draws x J x L)
# ==============================================================================
n_sim <- array(0, dim = c(S_draws, J, L))

for (s in 1:S_draws) {
  for (j in 1:J) {
    N_val <- N_sim[s, j]
    
    if (N_val > 0) {
      theta_vec <- posterior$theta[s, j, ]
      # rmultinom returns an (L x 1) matrix, convert to vector
      n_sim[s, j, ] <- rmultinom(n = 1, size = N_val, prob = theta_vec)[, 1]
    }
  }
}

# ==============================================================================
# D. Sub-Count Predictive Realizations: SCR_sim (Matrix: S_draws x M)
# ==============================================================================

# 1. Map posterior$q (S_draws x J x L) to each observation m (S_draws x M)
library(boot)
q_obs <- inv.logit(posterior$logit_q)
#q_obs = posterior$q
# 2a. Full Generative Predictions: SCR_sim ~ Binomial(Y_sim, q)
# Uses simulated counts Y_sim as trial sizes
SCR_sim = matrix(NA, nrow = S_draws, ncol = M)
for(m in 1:M){
  for(j in 1:S_draws){
    SCR_sim[j, m] <- rbinom(1, size = Y_sim[j, m], prob = q_obs[j,m])
  }
}

# 2b. Conditional Predictive Draws: SCR_sim_given_Y ~ Binomial(Y_observed, q)
# Uses actual observed counts Y as trial sizes
Y_mat <- matrix(rep(stan_data$Y, each = S_draws), nrow = S_draws, ncol = M)

SCR_sim_given_Y <- matrix(
  rbinom(n = S_draws * M, size = Y_mat, prob = q_obs),
  nrow = S_draws,
  ncol = M
)


d_obs <- inv.logit(posterior$logit_d)
#d_obs = posterior$d
LTBI_sim = matrix(NA, nrow = S_draws, ncol = M)
for(m in 1:M){
  for(j in 1:S_draws){
    LTBI_sim[j, m] <- rbinom(1, size = SCR_sim[j, m], prob = d_obs[j,m])
  }
}


par(mfrow=c(6,6))
plot(density(SCR_sim[,1]))
abline(v=quantile(SCR_sim[,1],0.025))
abline(v=quantile(SCR_sim[,1],0.975))
abline(v = dat$Total.No.Screened[1],col='red')
for(i in 2:36){
  plot(density(SCR_sim[,i]))
  abline(v = dat$Total.No.Screened[i],col='red')
  abline(v=quantile(SCR_sim[,i],0.025))
  abline(v=quantile(SCR_sim[,i],0.975))
}


par(mfrow=c(6,6))
plot(density(LTBI_sim[,37]))
abline(v=quantile(LTBI_sim[,37],0.025))
abline(v=quantile(LTBI_sim[,37],0.975))
abline(v = dat$Latent[37],col='red')
for(i in 38:72){
  plot(density(LTBI_sim[,i]))
  abline(v = dat$Latent[i],col='red')
  abline(v=quantile(LTBI_sim[,i],0.025))
  abline(v=quantile(LTBI_sim[,i],0.975))
}


