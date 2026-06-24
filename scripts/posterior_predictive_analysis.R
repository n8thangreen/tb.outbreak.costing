# scripts/posterior_predictive_analysis.R
# Run posterior predictive simulation for costing model

library(dplyr)
library(purrr)
library(ggplot2)
library(reshape2)

devtools::load_all(".")
source("scripts/model_data.R")
load("input_data/BUGS_output.RData")

dat <- read.csv("input_data/cleaned_data.csv", check.names = FALSE)

num_settings <- 5
mcmc_dat <- res_bugs$BUGSoutput$sims.list
n_samples <- res_bugs$BUGSoutput$n.sims

# We will generate posterior predictive counts and costs
out_pred_cost_total <- vector(mode = "list", length = num_settings)
out_pred_cost_per_inc <- vector(mode = "list", length = num_settings)
out_pred_id <- vector(mode = "list", length = num_settings)
out_pred_screen <- vector(mode = "list", length = num_settings)
out_pred_ltbi <- vector(mode = "list", length = num_settings)

# Set random seed for reproducibility
set.seed(42)

for (s in seq_len(num_settings)) {
  # 1. Simulate posterior predictive counts
  # Using R's vectorized distributions
  s_rate_id <- mcmc_dat$srate_id[, s]
  s_p_screen <- mcmc_dat$sp_screen[, s]
  s_p_ltbi <- mcmc_dat$sp_ltbi[, s]
  s_rate_inc <- mcmc_dat$srate_inc[, s]
  
  # Draw random variables representing the counts in a single new incident
  pred_id <- rpois(n_samples, s_rate_id)
  # Screened cannot exceed identified
  pred_screen <- rbinom(n_samples, pred_id, s_p_screen)
  # LTBI cannot exceed screened
  pred_ltbi <- rbinom(n_samples, pred_screen, s_p_ltbi)
  
  # Save counts
  out_pred_id[[s]] <- pred_id
  out_pred_screen[[s]] <- pred_screen
  out_pred_ltbi[[s]] <- pred_ltbi
  
  # 2. Calculate costs using simulated counts
  pred_cost_total <- numeric(n_samples)
  pred_cost_per_inc <- numeric(n_samples)
  
  for (i in 1:n_samples) {
    # Draw simulated incident counts in a year
    pred_inc <- rpois(1, s_rate_inc[i])
    
    id_val <- pred_id[i]
    screen_val <- pred_screen[i]
    ltbi_val <- pred_ltbi[i]
    
    pred_cost_total[i] <- total_year_cost(
      inc_sample = pred_inc,
      id_per_inc = id_val,
      screen_per_inc = screen_val,
      ltbi_per_inc = ltbi_val
    )
    
    pred_cost_per_inc[i] <- total_year_cost(
      inc_sample = 1,
      id_per_inc = id_val,
      screen_per_inc = screen_val,
      ltbi_per_inc = ltbi_val
    )
  }
  
  out_pred_cost_total[[s]] <- pred_cost_total
  out_pred_cost_per_inc[[s]] <- pred_cost_per_inc
}

setting_names <- levels(as.factor(dat$setting))
names(out_pred_cost_total) <- setting_names
names(out_pred_cost_per_inc) <- setting_names
names(out_pred_id) <- setting_names
names(out_pred_screen) <- setting_names
names(out_pred_ltbi) <- setting_names

# Compare expected-value costs (current) with posterior predictive costs
save(out_pred_cost_total, out_pred_cost_per_inc, 
     out_pred_id, out_pred_screen, out_pred_ltbi,
     file = "input_data/posterior_predictive_output.RData")

message("Posterior predictive analysis completed and saved to input_data/posterior_predictive_output.RData")

# Print summary comparison of means and SDs
cat("\n--- Comparison of Cost per Incident ---\n")
for (setting in setting_names) {
  cat("\nSetting:", setting, "\n")
  ev_costs <- sapply(1:n_samples, function(i) {
    total_year_cost(
      inc_sample = 1,
      id_per_inc = mcmc_dat$srate_id[i, which(setting_names == setting)],
      screen_per_inc = mcmc_dat$pred_n_screen[i, which(setting_names == setting)],
      ltbi_per_inc = mcmc_dat$pred_n_ltbi[i, which(setting_names == setting)]
    )
  })
  
  cat("Expected Value approach: mean =", round(mean(ev_costs), 2), ", sd =", round(sd(ev_costs), 2), "\n")
  cat("Posterior Predictive approach: mean =", round(mean(out_pred_cost_per_inc[[setting]]), 2), ", sd =", round(sd(out_pred_cost_per_inc[[setting]]), 2), "\n")
}
