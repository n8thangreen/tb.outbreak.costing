# ================================
# posterior forest plots
# using stan fit (cmdstanr & bayesplot)
# ================================

library(dplyr)
library(ggplot2)
library(bayesplot)
library(grid)
library(gridExtra)
library(posterior)
library(patchwork)

devtools::load_all(".")
dat <- read.csv(here::here("input_data/cleaned_data.csv"), check.names = FALSE)

# Load the cmdstanr fit object
fit_stan <- readRDS(here::here("data/stan_fit.rds"))


# ==========================================================
# 1. Reconstruct Year x Setting Grid Parameters
# ==========================================================
# We need to construct the conditional expectations for each Year/Setting combo
# using the intercepts (mu), year effects (a), and setting effects (b).

mu  <- as.numeric(fit_stan$draws("mu", format = "draws_matrix"))
a   <- as.matrix(fit_stan$draws("a", format = "draws_matrix"))
b   <- as.matrix(fit_stan$draws("b", format = "draws_matrix"))

mu_q <- as.numeric(fit_stan$draws("mu_q", format = "draws_matrix"))
a_q  <- as.matrix(fit_stan$draws("a_q", format = "draws_matrix"))
b_q  <- as.matrix(fit_stan$draws("b_q", format = "draws_matrix"))

mu_d <- as.numeric(fit_stan$draws("mu_d", format = "draws_matrix"))
a_d  <- as.matrix(fit_stan$draws("a_d", format = "draws_matrix"))
b_d  <- as.matrix(fit_stan$draws("b_d", format = "draws_matrix"))

# Create the grid reference
year_levels <- levels(as.factor(dat$year))
set_levels  <- levels(as.factor(dat$setting))

grid <- expand.grid(year = year_levels, setting = set_levels)
grid_names <- paste(grid$year, grid$setting)

n_iters <- nrow(a)
n_grid  <- nrow(grid)

# Initialize matrices to hold the grid calculations
p_ltbi_mat   <- matrix(NA, nrow = n_iters, ncol = n_grid)
p_screen_mat <- matrix(NA, nrow = n_iters, ncol = n_grid)
rate_id_mat  <- matrix(NA, nrow = n_iters, ncol = n_grid)

colnames(p_ltbi_mat)   <- grid_names
colnames(p_screen_mat) <- grid_names
colnames(rate_id_mat)  <- grid_names

# Calculate probabilities and rates across the grid
# Assuming year and setting indices map directly to 1:L and 1:J
for (i in 1:n_grid) {
  z <- as.numeric(as.factor(grid$year))[i]
  s <- as.numeric(as.factor(grid$setting))[i]
  
  rate_id_mat[, i]  <- exp(mu + a[, z] + b[, s])
  p_screen_mat[, i] <- plogis(mu_q + a_q[, z] + b_q[, s])
  p_ltbi_mat[, i]   <- plogis(mu_d + a_d[, z] + b_d[, s])
}


# ==========================================================
# 2. Reconstruct Setting-Only Parameters (Pooled Year)
# ==========================================================
# Using the marginal expectations (Y_rep) to account for all variance components

Y_rep_mat    <- as.matrix(fit_stan$draws("Y_rep", format = "draws_matrix"))
SCR_rep_mat  <- as.matrix(fit_stan$draws("SCR_rep", format = "draws_matrix"))
LTBI_rep_mat <- as.matrix(fit_stan$draws("LTBI_rep", format = "draws_matrix"))

n_settings <- length(set_levels)

sp_ltbi_mat       <- matrix(NA, nrow = n_iters, ncol = n_settings)
sp_screen_mat     <- matrix(NA, nrow = n_iters, ncol = n_settings)
srate_id_mat      <- matrix(NA, nrow = n_iters, ncol = n_settings)
pred_n_screen_mat <- matrix(NA, nrow = n_iters, ncol = n_settings)
pred_n_ltbi_mat   <- matrix(NA, nrow = n_iters, ncol = n_settings)

# Set column names to setting names for automatic labeling in bayesplot
colnames_set <- set_levels
colnames(sp_ltbi_mat)       <- colnames_set
colnames(sp_screen_mat)     <- colnames_set
colnames(srate_id_mat)      <- colnames_set
colnames(pred_n_screen_mat) <- colnames_set
colnames(pred_n_ltbi_mat)   <- colnames_set

for (j in 1:n_settings) {
  # Assuming stan_data$S contains the setting index for each observation
  cols_j <- which(dat$setting == set_levels[j]) 
  
  srate_id_mat[, j]      <- rowMeans(Y_rep_mat[, cols_j])
  pred_n_screen_mat[, j] <- rowMeans(SCR_rep_mat[, cols_j])
  pred_n_ltbi_mat[, j]   <- rowMeans(LTBI_rep_mat[, cols_j])
  
  sp_screen_mat[, j]     <- pred_n_screen_mat[, j] / srate_id_mat[, j]
  sp_ltbi_mat[, j]       <- pred_n_ltbi_mat[, j] / pred_n_screen_mat[, j]
}


# ==========================================================
# 3. Standard Forest Plots (Replacing mcmcplots::caterplot)
# ==========================================================
# bayesplot automatically puts names on the y-axis

# prob LTBI cases from screened
mcmc_intervals(p_ltbi_mat) + ggtitle("Probability LTBI (Grid)")
# prob screen
mcmc_intervals(p_screen_mat) + ggtitle("Probability Screened (Grid)")
# identification rate
mcmc_intervals(rate_id_mat) + ggtitle("Identification Rate (Grid)")
# incident rate / year (REMOVED FROM STAN MODEL)
# mcmc_intervals(rate_inc_mat) 

# grid setting only (pooled year)
# You can view these individually or wrap them in gridExtra
mcmc_intervals(sp_ltbi_mat) + ggtitle("probability LTBI") + xlim(-0.1, 0.3)
mcmc_intervals(sp_screen_mat) + ggtitle("probability screened") + xlim(0.5, 1.1)
mcmc_intervals(srate_id_mat) + ggtitle("number identified") + xlim(-100, 400)
# mcmc_intervals(srate_inc_mat) + ggtitle("number of incidents") + xlim(-0.1, 12)
mcmc_intervals(pred_n_screen_mat) + ggtitle("number screened") + xlim(-0.10, 300)
mcmc_intervals(pred_n_ltbi_mat) + ggtitle("number LTBI") + xlim(-4, 20)


# ==========================================================
# 4. Bespoke Clearer Plots (Exporting to file)
# ==========================================================
# Assuming your custom `stan_forest_plot` functions are built around ggplot, 
# replacing them with mcmc_intervals ensures full compatibility.

p_ltbi_fp <- mcmc_intervals(p_ltbi_mat) + ggtitle("Probability LTBI")
ggsave("plots/p_ltbi_forest_plot.png", plot = p_ltbi_fp, width = 30, height = 20, units = "cm", dpi = 640)

p_screen_fp <- mcmc_intervals(p_screen_mat) + ggtitle("Probability Screened")
ggsave("plots/p_screen_forest_plot.png", plot = p_screen_fp, width = 30, height = 20, units = "cm", dpi = 640)

rate_id_fp <- mcmc_intervals(rate_id_mat) + xlab("Number identified")
ggsave("plots/rate_id_forest_plot.png", plot = rate_id_fp, width = 30, height = 20, units = "cm", dpi = 640)

# rate_inc_fp <- mcmc_intervals(rate_inc_mat) + xlab("Number of incidents")
# ggsave("plots/rate_inc_forest_plot.png", plot = rate_inc_fp, width = 30, height = 20, units = "cm", dpi = 640)

## setting only
fp_ltbi    <- mcmc_intervals(sp_ltbi_mat) + ggtitle("Probability LTBI")
fp_screen  <- mcmc_intervals(sp_screen_mat) + ggtitle("Probability Screened")
fp_id      <- mcmc_intervals(srate_id_mat) + ggtitle("Identification rate")
# fp_inc   <- mcmc_intervals(srate_inc_mat) + ggtitle("Incident rate")
fp_nscreen <- mcmc_intervals(pred_n_screen_mat) + ggtitle("Number screened")
fp_nltbi   <- mcmc_intervals(pred_n_ltbi_mat) + ggtitle("Number LTBI")

# Because bayesplot outputs standard ggplots, your custom legend arranger still works
# fp <- grid_arrange_shared_legend(fp_ltbi, fp_screen, fp_id, fp_nscreen, fp_nltbi, nrow = 2, ncol = 3)

fp <- grid.arrange(fp_ltbi, fp_screen, fp_id, fp_nscreen, fp_nltbi, nrow = 2, ncol = 3)

fp <- wrap_plots(fp_ltbi, fp_screen, fp_id, fp_nscreen, fp_nltbi, nrow = 2, ncol = 3)

ggsave(here::here("plots/forest_plot_setting.png"), plot = fp, width = 40, height = 30, units = "cm", dpi = 640)
