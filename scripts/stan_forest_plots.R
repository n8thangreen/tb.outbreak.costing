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
# 2. Reconstruct Setting-Only Parameters (Marginalized)
# ==========================================================

for (j in 1:n_settings) {
  cols_j <- which(dat$setting == set_levels[j]) 
  
  # 1. Expected Counts 
  # (Means across incidents for the "Average Incident" plots)
  srate_id_mat[, j]      <- rowMeans(Y_rep_mat[, cols_j])
  pred_n_screen_mat[, j] <- rowMeans(SCR_rep_mat[, cols_j])
  pred_n_ltbi_mat[, j]   <- rowMeans(LTBI_rep_mat[, cols_j])
  
  # 2. Marginalized Probabilities (Accounting for OLRE)
  # Ratio of the SUMS across all incidents in the setting per iteration.
  # This fully accounts for the OLRE variance, naturally weights by incident size, 
  # and guarantees the probabilities are strictly bounded between 0 and 1.
  
  sp_screen_mat[, j] <- rowSums(SCR_rep_mat[, cols_j]) / rowSums(Y_rep_mat[, cols_j])
  
  # We add a tiny constant (1e-9) to the denominator to prevent division by zero 
  # in the extremely rare edge-case where 0 people are screened in a simulation.
  sp_ltbi_mat[, j]   <- rowSums(LTBI_rep_mat[, cols_j]) / (rowSums(SCR_rep_mat[, cols_j]) + 1e-9)
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

# ==========================================================
# Publication-Ready Forest Plots
# ==========================================================

build_pub_plot <- function(mat, title, x_limits = NULL) {
  
  # 1. Extract Median and 95% CrI from the matrix
  plot_data <- data.frame(
    Setting = colnames(mat),
    med = apply(mat, 2, median, na.rm = TRUE),
    l95 = apply(mat, 2, quantile, probs = 0.025, na.rm = TRUE),
    u95 = apply(mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  )
  
  # Ensure Settings are factored so they plot in the correct top-to-bottom order
  # Reversing the levels ensures the first level is at the top of the y-axis
  plot_data$Setting <- factor(plot_data$Setting, levels = rev(colnames(mat)))
  
  # 2. Build the ggplot
  p <- ggplot(plot_data, aes(x = med, y = Setting, color = Setting)) +
    # Draw the intervals and point estimates
    geom_pointrange(aes(xmin = l95, xmax = u95), size = 0.6, linewidth = 1) +
    
    # Titles and labels
    labs(title = title, x = NULL, y = NULL) +
    
    # Use standard ggplot2 hue colors (which match your original JPEG exactly)
    scale_color_discrete(name = "Setting") +
    
    # Apply publication styling
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0, size = 16, margin = margin(b = 10)),
      axis.text.y = element_blank(),               # Remove y-axis text
      axis.ticks.y = element_line(color = "grey80"),
      panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      legend.position = "none"                     # Hide individual legends
    )
  
  # Apply manual x-axis limits if provided
  if (!is.null(x_limits)) {
    p <- p + coord_cartesian(xlim = x_limits)
  }
  
  return(p)
}

# ==========================================================
# Generate Individual Plots
# ==========================================================
# Using the matrices calculated in the previous step (sp_ltbi_mat, etc.)

fp_ltbi    <- build_pub_plot(sp_ltbi_mat, "Probability LTBI", x_limits = c(0.0, 0.10))
fp_screen  <- build_pub_plot(sp_screen_mat, "Probability Screened", x_limits = c(0.55, 1))
fp_id      <- build_pub_plot(srate_id_mat, "Identification rate", x_limits = c(0, 120))
fp_nscreen <- build_pub_plot(pred_n_screen_mat, "Number screened", x_limits = c(0, 90))
fp_nltbi   <- build_pub_plot(pred_n_ltbi_mat, "Number LTBI", x_limits = c(0, 6))

# ==========================================================
# Combine and Format with Patchwork
# ==========================================================

# Wrap the plots into a grid. 
# Because there are 5 plots, patchwork will leave the bottom-right space empty.
fp_combined <- wrap_plots(
  fp_ltbi, fp_screen, fp_id, 
  fp_nscreen, fp_nltbi, 
  nrow = 2, ncol = 3
) + 
  # guides = "collect" pulls the hidden legends from the plots and combines them
  plot_layout(guides = "collect") & 
  
  # The '&' operator applies this theme to the ENTIRE patchwork assembly
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )

# View the plot
print(fp_combined)

# Save the final publication plot
ggsave(here::here("plots/forest_plot_setting.png"), 
       plot = fp_combined, width = 16, height = 10, dpi = 640)
