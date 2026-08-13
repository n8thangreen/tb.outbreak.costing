# =================================================================
# VALUE-TB: Posterior Predictive Checks and LaTeX Table Generation
# =================================================================

library(ggplot2)
library(dplyr)
library(patchwork)
library(cmdstanr)
library(posterior)
library(knitr)
library(kableExtra)

# =================================================================
# 1. Setup & Data Loading
# =================================================================

# Load the cmdstanr fit object
fit_stan <- readRDS(here::here("data/stan_fit.rds"))

# Note: Ensure your raw data list used for Stan is loaded as 'stan_data'
# load(here::here("input_data/stan_data.RData")) # Example


# =================================================================
# 2. Calculate Empirical Mean Counts and Standard Errors
# =================================================================

empirical_df <- data.frame(
  SettingIndex = stan_data$S,
  Y = stan_data$Y,
  SCR = stan_data$SCR,
  LTBI = stan_data$LTBI
) %>%
  group_by(SettingIndex) %>%
  summarize(
    n = n(),
    y_identify  = mean(Y),
    se_identify = sd(Y) / sqrt(n),
    
    y_screen    = mean(SCR),
    se_screen   = sd(SCR) / sqrt(n),
    
    y_ltbi      = mean(LTBI),
    se_ltbi     = sd(LTBI) / sqrt(n),
    .groups     = "drop"
  )

# =================================================================
# 3. Extract and Aggregate Generated Quantities (Marginal Expected Counts)
# =================================================================

# Extract the M x Iters matrices (where M is total observations)
Y_rep_mat    <- as.matrix(fit_stan$draws("Y_rep", format = "draws_matrix"))
SCR_rep_mat  <- as.matrix(fit_stan$draws("SCR_rep", format = "draws_matrix"))
LTBI_rep_mat <- as.matrix(fit_stan$draws("LTBI_rep", format = "draws_matrix"))

n_iters    <- nrow(Y_rep_mat)
n_settings <- max(stan_data$S)

# Initialize empty matrices to store the Setting-level means per iteration
y_rep_setting    <- matrix(NA, nrow = n_iters, ncol = n_settings)
scr_rep_setting  <- matrix(NA, nrow = n_iters, ncol = n_settings)
ltbi_rep_setting <- matrix(NA, nrow = n_iters, ncol = n_settings)

# Average across observations within each setting for every iteration
for (j in 1:n_settings) {
  cols_j <- which(stan_data$S == j)
  
  y_rep_setting[, j]    <- rowMeans(Y_rep_mat[, cols_j])
  scr_rep_setting[, j]  <- rowMeans(SCR_rep_mat[, cols_j])
  ltbi_rep_setting[, j] <- rowMeans(LTBI_rep_mat[, cols_j])
}

# =================================================================
# 4. Generate Forest Plots
# =================================================================

# Helper to extract plotting summaries (Median, 50% CrI, 95% CrI)
summarize_posterior_plot <- function(mat, prefix) {
  data.frame(
    SettingIndex = 1:ncol(mat),
    med = apply(mat, 2, median),
    l95 = apply(mat, 2, quantile, probs = 0.025),
    u95 = apply(mat, 2, quantile, probs = 0.975),
    l50 = apply(mat, 2, quantile, probs = 0.25),
    u50 = apply(mat, 2, quantile, probs = 0.75)
  ) %>%
    rename_with(~ paste0(prefix, "_", .), -SettingIndex)
}

post_identify <- summarize_posterior_plot(y_rep_setting, "identify")
post_screen   <- summarize_posterior_plot(scr_rep_setting, "screen")
post_ltbi     <- summarize_posterior_plot(ltbi_rep_setting, "ltbi")

plot_data <- empirical_df %>%
  left_join(post_identify, by = "SettingIndex") %>%
  left_join(post_screen, by = "SettingIndex") %>%
  left_join(post_ltbi, by = "SettingIndex")

col_y    <- "#1C366B" 
col_yrep <- "#C5D4E8" 

# Plot builder with vertical dodging
build_forest_plot <- function(data, x_emp, x_emp_se, x_med, x_l50, x_u50, x_l95, x_u95, 
                              title, x_label) {
  
  nudge_emp <- position_nudge(y = 0.15)
  nudge_rep <- position_nudge(y = -0.15)
  
  ggplot(data, aes(y = factor(SettingIndex))) +
    # Posterior intervals (Nudged down)
    geom_linerange(aes(xmin = !!sym(x_l95), xmax = !!sym(x_u95), color = "yrep"), 
                   linewidth = 0.6, position = nudge_rep) +
    geom_linerange(aes(xmin = !!sym(x_l50), xmax = !!sym(x_u50), color = "yrep"), 
                   linewidth = 2, position = nudge_rep) +
    geom_point(aes(x = !!sym(x_med), color = "yrep"), 
               size = 3.5, position = nudge_rep) +
    
    # Empirical 95% Confidence Interval (Nudged up)
    geom_linerange(aes(xmin = !!sym(x_emp) - 1.96 * !!sym(x_emp_se), 
                       xmax = !!sym(x_emp) + 1.96 * !!sym(x_emp_se), color = "y"), 
                   linewidth = 1, position = nudge_emp) +
    geom_point(aes(x = !!sym(x_emp), color = "y"), 
               size = 2, position = nudge_emp) +
    
    scale_color_manual(
      name = NULL, 
      values = c("y" = col_y, "yrep" = col_yrep),
      labels = c(expression(y), expression(y[rep]))
    ) +
    labs(title = title, x = x_label, y = "Setting Index") +
    theme_classic() +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 11),
      plot.title = element_text(size = 12, face = "plain")
    )
}

p_identify <- build_forest_plot(
  plot_data, "y_identify", "se_identify", "identify_med", 
  "identify_l50", "identify_u50", "identify_l95", "identify_u95",
  "Expected Eligible (Identified) per Incident by Setting", "Count Identified (Y)"
)

p_screen <- build_forest_plot(
  plot_data, "y_screen", "se_screen", "screen_med", 
  "screen_l50", "screen_u50", "screen_l95", "screen_u95",
  "Expected Screened per Incident by Setting", "Count Screened (SCR)"
)

p_ltbi <- build_forest_plot(
  plot_data, "y_ltbi", "se_ltbi", "ltbi_med", 
  "ltbi_l50", "ltbi_u50", "ltbi_l95", "ltbi_u95",
  "Expected LTBI Positive per Incident by Setting", "Count LTBI Positive"
)

final_plot <- p_identify / p_screen / p_ltbi
print(final_plot)

ggsave(here::here("plots/posterior_predictive_counts_forest_plot.png"), 
       plot = final_plot, width = 8, height = 10, dpi = 300)


# =================================================================
# 5. Generate LaTeX Table
# =================================================================

# Calculate the implied proportions based on the simulated marginal counts
p_screen_rep <- scr_rep_setting / y_rep_setting
p_ltbi_rep   <- ltbi_rep_setting / scr_rep_setting

# Helper function to extract Mean and 95% CrI for the table
format_table_cell <- function(mat) {
  mean_val <- apply(mat, 2, mean, na.rm = TRUE)
  lower    <- apply(mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  upper    <- apply(mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  paste0("\\makecell{", sprintf("%.2f", mean_val), " \\\\ {[", 
         sprintf("%.2f", lower), "--", 
         sprintf("%.2f", upper), "]}}")
}

tab_pop_inc_latex <- data.frame(
  Setting = c(
    "Commercial", 
    "Education", 
    "Factory/workplace", 
    "\\makecell[l]{Hospital/clinic/ \\\\ care centre}", 
    "Other"
  ),
  n_identify = format_table_cell(y_rep_setting),
  n_screen   = format_table_cell(scr_rep_setting),
  p_screen   = format_table_cell(p_screen_rep),
  n_ltbi     = format_table_cell(ltbi_rep_setting),
  p_ltbi     = format_table_cell(p_ltbi_rep)
)

col_names <- c(
  "\\textbf{Setting}", 
  "\\textbf{\\makecell{No. eligible \\\\ (identified)}}",
  "\\textbf{\\makecell{No. \\\\ screened}}",
  "\\textbf{\\makecell{Proportion \\\\ screened}}",
  "\\textbf{\\makecell{No. LTBI \\\\ positive}}",
  "\\textbf{\\makecell{Proportion LTBI \\\\ positive}}"
)

# Updated kable block with corrected caption
latex_table <- kable(
  tab_pop_inc_latex, 
  format = "latex", 
  escape = FALSE,
  booktabs = TRUE,
  col.names = col_names,
  # Removed "median counts" from the caption
  caption = "Posterior expected (marginal) counts and 95\\% credible intervals (CrI) of individuals per TB incident contact investigation by setting.",
  label = "posterior-counts",
  align = "lccccc"
) %>%
  kable_styling(latex_options = "hold_position")

writeLines(as.character(latex_table), here::here("output_data/pop_summary_table_per_inc.tex"))
