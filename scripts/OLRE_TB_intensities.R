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
library(patchwork)

dat <- read_csv(here("input_data", "cleaned_data.csv"))

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
  Z = Z,
  SCR = SCR,
  LTBI = LTBI
)


#########################
# cross design with OLRE

cross_olre_model <- "
data {
  int<lower=1> M;                      // Total number of observations
  int<lower=1> L;                      // Total number of years
  int<lower=1> J;                      // Total number of settings
  
  array[M] int<lower=0> LTBI;           // Sub-counts (must be <= Y[m])
  array[M] int<lower=0> SCR;           // Sub-counts (must be <= Y[m])
  array[M] int<lower=0> Y;             // Observed counts
  array[M] int<lower=1, upper=J> S;    // Setting index for each observation m
  array[M] int<lower=1, upper=L> Z;    // Year index for each observation m
}

parameters {
  //Identification//
  real mu;                             // Global intercept
  
  // Raw parameters for non-centered parameterization
  vector[L] a_raw;                     // Year effects (raw)
  vector[J] b_raw;                     // Setting effects (raw)
  vector[M] epsilon_raw;               // Observation-level effects (raw)
  
  // Standard deviations for the hierarchies
  real sigma_a_raw;
  real sigma_b_raw;
  real sigma_obs_raw;
  
  //Screening//
  real mu_q;                             // Global intercept
  
  // Raw parameters for non-centered parameterization
  vector[L] a_q_raw;                     // Year effects (raw)
  vector[J] b_q_raw;                     // Setting effects (raw)
  vector[M] epsilon_q_raw;               // Observation-level effects (raw)
  
  // Standard deviations for the hierarchies
  real sigma_a_q_raw;
  real sigma_b_q_raw;
  real sigma_obs_q_raw;
  
  //LTBI//
  //Screening//
  real mu_d;                             // Global intercept
  
  // Raw parameters for non-centered parameterization
  vector[L] a_d_raw;                     // Year effects (raw)
  vector[J] b_d_raw;                     // Setting effects (raw)
  vector[M] epsilon_d_raw;               // Observation-level effects (raw)
  
  // Standard deviations for the hierarchies
  real sigma_a_d_raw;
  real sigma_b_d_raw;
  real sigma_obs_d_raw;
}

transformed parameters {
  //Identification//
  vector[L] a;                         // Year effects
  vector[J] b;                         // Setting effects
  vector[M] log_lambda;                // Log-intensity for each observation
  
  real<lower=0> sigma_a = exp(sigma_a_raw);               // SD for year effects
  real<lower=0> sigma_b = exp(sigma_b_raw);               // SD for year effects
  real<lower=0> sigma_obs = exp(sigma_obs_raw);               // SD for year effects
  
  // Non-centered transformations (scaling raw standard normals by their SD)
  a = a_raw * sigma_a;
  b = b_raw * sigma_b;
  
  // Crossed structure + OLRE
  for (m in 1:M) {
    log_lambda[m] = mu + a[Z[m]] + b[S[m]] + (epsilon_raw[m] * sigma_obs);
  }
  
  //Screening//
  vector[L] a_q;                         // Year effects
  vector[J] b_q;                         // Setting effects
  vector[M] logit_q;                // Log-intensity for each observation
  
  real<lower=0> sigma_a_q = exp(sigma_a_q_raw);               // SD for year effects
  real<lower=0> sigma_b_q = exp(sigma_b_q_raw);               // SD for year effects
  real<lower=0> sigma_obs_q = exp(sigma_obs_q_raw);               // SD for year effects
  
  // Non-centered transformations (scaling raw standard normals by their SD)
  a_q = a_q_raw * sigma_a_q;
  b_q = b_q_raw * sigma_b_q;
  
  // Crossed structure + OLRE
  for (m in 1:M) {
    logit_q[m] = mu_q + a_q[Z[m]] + b_q[S[m]] + (epsilon_q_raw[m] * sigma_obs_q);
  }
  
  //LTBI//
   //Screening//
  vector[L] a_d;                         // Year effects
  vector[J] b_d;                         // Setting effects
  vector[M] logit_d;                // Log-intensity for each observation
  
  real<lower=0> sigma_a_d = exp(sigma_a_d_raw);               // SD for year effects
  real<lower=0> sigma_b_d = exp(sigma_b_d_raw);               // SD for year effects
  real<lower=0> sigma_obs_d = exp(sigma_obs_d_raw);               // SD for year effects
  
  // Non-centered transformations (scaling raw standard normals by their SD)
  a_d = a_d_raw * sigma_a_d;
  b_d = b_d_raw * sigma_b_d;
  
  // Crossed structure + OLRE
  for (m in 1:M) {
    logit_d[m] = mu_d + a_d[Z[m]] + b_d[S[m]] + (epsilon_d_raw[m] * sigma_obs_d);
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
  sigma_a_raw ~ normal(0, 1);
  sigma_b_raw ~ normal(0, 1);
  sigma_obs_raw ~ normal(0, 1);
  
  //mu_q ~ normal(0, 1.87);                   // Weakly informative prior on global mean
  mu_q ~ normal(0.94, 0.3);                   //informative prior on global mean
  
  // Standard normal priors for the raw parameters (Matt trick)
  a_q_raw ~ std_normal();
  b_q_raw ~ std_normal();
  epsilon_q_raw ~ std_normal();
  
  // Half-normal priors on standard deviations (implicitly truncated at 0)
  sigma_a_q_raw ~ normal(0, 1);
  sigma_b_q_raw ~ normal(0, 1);
  sigma_obs_q_raw ~ normal(0, 1);
  
  //mu_d ~ normal(0, 1.87);                   // Weakly informative prior on global mean
    mu_d ~ normal(-1.49, 0.3);                   //informative prior on global mean

  // Standard normal priors for the raw parameters (Matt trick)
  a_d_raw ~ std_normal();
  b_d_raw ~ std_normal();
  epsilon_d_raw ~ std_normal();
  
  // Half-normal priors on standard deviations (implicitly truncated at 0)
  sigma_a_d_raw ~ normal(0, 1);
  sigma_b_d_raw ~ normal(0, 1);
  sigma_obs_d_raw ~ normal(0, 1);
  
  
  // Likelihood
  Y ~ poisson_log(log_lambda);      
  
  for (m in 1:M) {                     // Binomial thinning process
    SCR[m] ~ binomial_logit(Y[m], logit_q[m]);
  }
  
  for (m in 1:M) {                     // Binomial thinning process
    LTBI[m] ~ binomial_logit(SCR[m], logit_d[m]);
  }
  
}

generated quantities {
  array[M] int<lower=0> Y_rep;
  array[M] int<lower=0> SCR_rep;
  array[M] int<lower=0> LTBI_rep;
  
  // Vector to store log-likelihood for LOO-CV model comparison
  vector[M] log_lik; 

  for (m in 1:M) {
    // 1. Simulate Total Identified
    Y_rep[m] = poisson_log_rng(log_lambda[m]);
    
    // 2. Simulate Screened (conditional on observed Y)
    SCR_rep[m] = binomial_rng(Y[m], inv_logit(logit_q[m]));
    
    // 3. Simulate LTBI (conditional on observed SCR)
    LTBI_rep[m] = binomial_rng(SCR[m], inv_logit(logit_d[m]));
    
    // 4. Calculate log-likelihood
    log_lik[m] = poisson_log_lpmf(Y[m] | log_lambda[m]) + 
                 binomial_logit_lpmf(SCR[m] | Y[m], logit_q[m]) + 
                 binomial_logit_lpmf(LTBI[m] | SCR[m], logit_d[m]);
  }
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
  iter_sampling = 1000,
  adapt_delta = 0.93
)

# ==============================================================================
# POSTERIOR PREDICTIVE CHECKS (GLOBAL AND GROUPED)
# ==============================================================================

color_scheme_set("blue")
theme_set(theme_classic())

# 1. Extract Posterior Draws
# ---------------------------------------------------------
Y_rep    <- as_draws_matrix(fit$draws("Y_rep"))
SCR_rep  <- as_draws_matrix(fit$draws("SCR_rep"))
LTBI_rep <- as_draws_matrix(fit$draws("LTBI_rep"))

# Define the grouping variable (Setting)
setting_group <- as.factor(stan_data$S)

# ==============================================================================
# PART A: GLOBAL DIAGNOSTICS
# ==============================================================================

# 1. Global Density Overlays & Bar Plots
# ---------------------------------------------------------
p_global_y <- ppc_dens_overlay(y = stan_data$Y, yrep = Y_rep[1:100, ]) + 
  ggtitle("Global PPC: Total Identified (Y)")

p_global_scr <- ppc_dens_overlay(y = stan_data$SCR, yrep = SCR_rep[1:100, ]) + 
  ggtitle("Global PPC: Screened (SCR)")

p_global_ltbi <- ppc_bars(y = stan_data$LTBI, yrep = LTBI_rep[1:100, ]) + 
  coord_cartesian(xlim = c(-0.5, 20)) + 
  ggtitle("Global PPC: Latent TB (LTBI)")

# Display Global Count Plots (using patchwork) and save
p_global_counts <- p_global_y / p_global_scr / p_global_ltbi
ggsave("plots/global_counts.png", plot = p_global_counts, width = 8, height = 10)


# 2. Global Proportion of Zeros
# ---------------------------------------------------------
prop_zero <- function(x) mean(x == 0)

p_global_prop_zero <- ppc_stat(y = stan_data$LTBI, yrep = LTBI_rep, stat = "prop_zero") + 
  ggtitle("Global PPC: Proportion of Zeros in LTBI")

ggsave("plots/global_prop_zero.png", plot = p_global_prop_zero, width = 8, height = 6)


# 3. Global Predictive Intervals for LTBI Counts
# ---------------------------------------------------------
p_global_intervals <- ppc_intervals(y = stan_data$LTBI, yrep = LTBI_rep) + 
  ggtitle("Global PPC: Predictive Intervals for LTBI Counts") +
  coord_flip()

ggsave("plots/global_intervals.png", plot = p_global_intervals, width = 8, height = 8)


# 4. Global Probabilities (Histograms)
# ---------------------------------------------------------
# Screening Probability
obs_global_screen <- sum(stan_data$SCR) / sum(stan_data$Y)
rep_global_screen <- rowSums(SCR_rep) / rowSums(Y_rep)

p_prob_screen <- ggplot(data.frame(Simulated_Rate = rep_global_screen), aes(x = Simulated_Rate)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  geom_vline(xintercept = obs_global_screen, color = "#002b5e", linewidth = 1.5) +
  labs(title = "Global PPC: Average Screening Rate",
       subtitle = "Dark blue line = Observed rate; Light blue bars = Model predictions",
       x = "Overall Screening Rate (SCR / Y)", y = "Frequency")

# LTBI Positivity Probability
obs_global_ltbi <- sum(stan_data$LTBI) / sum(stan_data$SCR)
rep_global_ltbi <- rowSums(LTBI_rep) / rowSums(SCR_rep)

p_prob_ltbi <- ggplot(data.frame(Simulated_Rate = rep_global_ltbi), aes(x = Simulated_Rate)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  geom_vline(xintercept = obs_global_ltbi, color = "#002b5e", linewidth = 1.5) +
  labs(title = "Global PPC: Average LTBI Positivity Rate",
       subtitle = "Dark blue line = Observed rate; Light blue bars = Model predictions",
       x = "Overall LTBI Positivity Rate (LTBI / SCR)", y = "Frequency")

# Display Global Probability Plots and save
p_global_probs <- p_prob_screen / p_prob_ltbi
ggsave("plots/global_probabilities.png", plot = p_global_probs, width = 8, height = 8)


# ==============================================================================
# PART B: GROUPED DIAGNOSTICS (BY SETTING)
# ==============================================================================

# 1. Grouped Density Overlays & Bar Plots
# ---------------------------------------------------------
p_grouped_y <- ppc_dens_overlay_grouped(y = stan_data$Y, yrep = Y_rep[1:100, ], group = setting_group) + 
  ggtitle("Grouped PPC: Total Identified (Y) by Setting")
ggsave("plots/grouped_y.png", plot = p_grouped_y, width = 10, height = 8)

p_grouped_scr <- ppc_dens_overlay_grouped(y = stan_data$SCR, yrep = SCR_rep[1:100, ], group = setting_group) + 
  ggtitle("Grouped PPC: Screened (SCR) by Setting")
ggsave("plots/grouped_scr.png", plot = p_grouped_scr, width = 10, height = 8)

p_grouped_ltbi <- ppc_bars_grouped(y = stan_data$LTBI, yrep = LTBI_rep[1:100, ], group = setting_group) + 
  coord_cartesian(xlim = c(-0.5, 20)) + 
  ggtitle("Grouped PPC: Latent TB (LTBI) by Setting")
ggsave("plots/grouped_ltbi.png", plot = p_grouped_ltbi, width = 10, height = 8)


# 2. Grouped Proportion of Zeros
# ---------------------------------------------------------
p_grouped_prop_zero <- ppc_stat_grouped(y = stan_data$LTBI, yrep = LTBI_rep, group = setting_group, stat = "prop_zero") + 
  ggtitle("Grouped PPC: Proportion of Zeros in LTBI by Setting")

ggsave("plots/grouped_prop_zero.png", plot = p_grouped_prop_zero, width = 10, height = 8)


# 3. Grouped Probabilities (Caterpillar Plots)
# ---------------------------------------------------------
J <- max(stan_data$S) # Total number of settings

# Initialize containers
obs_rate_screen <- numeric(J)
rep_rate_screen <- matrix(NA, nrow = nrow(Y_rep), ncol = J)
obs_rate_ltbi   <- numeric(J)
rep_rate_ltbi   <- matrix(NA, nrow = nrow(Y_rep), ncol = J)

# Calculate rates per setting
for (j in 1:J) {
  idx <- which(stan_data$S == j) # Get rows for setting j
  
  # Screening Rate per Setting
  if (sum(stan_data$Y[idx]) > 0) {
    obs_rate_screen[j]   <- sum(stan_data$SCR[idx]) / sum(stan_data$Y[idx])
    rep_rate_screen[, j] <- rowSums(SCR_rep[, idx, drop = FALSE]) / rowSums(Y_rep[, idx, drop = FALSE])
  } else {
    obs_rate_screen[j]   <- NA
    rep_rate_screen[, j] <- NA
  }
  
  # LTBI Rate per Setting
  if (sum(stan_data$SCR[idx]) > 0) {
    obs_rate_ltbi[j]   <- sum(stan_data$LTBI[idx]) / sum(stan_data$SCR[idx])
    rep_rate_ltbi[, j] <- rowSums(LTBI_rep[, idx, drop = FALSE]) / rowSums(SCR_rep[, idx, drop = FALSE])
  } else {
    obs_rate_ltbi[j]   <- NA
    rep_rate_ltbi[, j] <- NA
  }
}

# Plot Grouped Screening Rates
screen_group <- 
  ppc_intervals(y = obs_rate_screen, yrep = rep_rate_screen) + 
  ggtitle("Group Average: Screening Rate by Setting") +
  labs(x = "Setting Index", y = "Screening Rate (SCR/Y)") +
  coord_flip()

# Plot Grouped LTBI Positivity Rates
ltbi_group <- 
  ppc_intervals(y = obs_rate_ltbi, yrep = rep_rate_ltbi) + 
  ggtitle("Group Average: LTBI Positivity Rate by Setting") +
  labs(x = "Setting Index", y = "LTBI Positivity Rate (LTBI/SCR)") +
  coord_flip()

# Combine and save
p_grouped_probs <- screen_group / ltbi_group
ggsave("plots/grouped_probabilities.png", plot = p_grouped_probs, width = 8, height = 10)

###################
# cascade PPC plot

setting_names <- c(
  "Commercial", 
  "Education", 
  "Factory/workplace", 
  "Hospital/clinic/care centre", 
  "Other"
)

# 1. Setup the total number of settings and an empty data frame
J <- max(stan_data$S)
df_ppc_cascade <- data.frame()

# 2. Calculate observed and predicted aggregates per setting
for (j in 1:J) {
  # Get indices for observations in setting j
  idx <- which(stan_data$S == j)
  
  # A. Sum observed data for this setting
  obs_y <- sum(stan_data$Y[idx])
  obs_scr <- sum(stan_data$SCR[idx])
  obs_ltbi <- sum(stan_data$LTBI[idx])
  
  # B. Sum the simulated predictions for this setting (across all rows/draws)
  pred_y_sums <- rowSums(Y_rep[, idx, drop = FALSE])
  pred_scr_sums <- rowSums(SCR_rep[, idx, drop = FALSE])
  pred_ltbi_sums <- rowSums(LTBI_rep[, idx, drop = FALSE])
  
  # C. Helper function to calculate median and 95% CrI for each stage
  make_stage_row <- function(stage_name, obs, preds) {
    data.frame(
      Setting = setting_names[j],
      Stage = stage_name,
      Observed = obs,
      Pred_Med = median(preds),
      Pred_Lo = quantile(preds, 0.025, na.rm = TRUE),
      Pred_Hi = quantile(preds, 0.975, na.rm = TRUE)
    )
  }
  
  # D. Append to the main data frame
  df_ppc_cascade <- bind_rows(
    df_ppc_cascade,
    make_stage_row("Identified", obs_y, pred_y_sums),
    make_stage_row("Screened", obs_scr, pred_scr_sums),
    make_stage_row("Latent", obs_ltbi, pred_ltbi_sums)
  )
}

# 3. Format factors to ensure correct left-to-right plotting order
df_ppc_cascade$Stage <- factor(df_ppc_cascade$Stage, levels = c("Identified", "Screened", "Latent"))

# 4. Generate the PPC Cascade Plot
p_cascade_ppc <- ggplot(df_ppc_cascade, aes(x = Stage, group = Setting)) +
  # Model Prediction Uncertainty (95% Credible Interval Ribbon)
  geom_ribbon(aes(ymin = Pred_Lo, ymax = Pred_Hi), fill = "lightblue", alpha = 0.5) +
  # Model Prediction Median (Dashed line)
  geom_line(aes(y = Pred_Med), color = "blue", linetype = "dashed", linewidth = 1) +
  geom_point(aes(y = Pred_Med), color = "blue", shape = 1, size = 2) +
  # Observed True Data (Solid line)
  geom_line(aes(y = Observed), color = "black", linewidth = 1.2) +
  geom_point(aes(y = Observed), color = "black", size = 2.5) +
  
  # Aesthetics and scaling
  scale_y_log10(labels = comma) + 
  # facet_wrap(~Setting, scales = "free_y") + # Creates a grid of plots, one for each setting
  facet_wrap(~Setting) +
  theme_bw() +
  labs(
    title = "PPC: Cascade by Setting",
    subtitle = "Black Solid Line = Observed Data | Blue Dashed Line & Ribbon = Model Prediction (Median & 95% CrI)",
    x = "Cascade Stage",
    y = "Total Count (Log10 Scale)"
  ) +
  theme(
    strip.background = element_rect(fill = "#002b5e"),
    strip.text = element_text(color = "white", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_cascade_ppc)

ggsave("plots/ppc_cascade_per_setting.png", plot = p_cascade_ppc, width = 10, height = 8)
