# ================================
# posterior forest plots
# ================================

library(ggplot2)
library(bayesplot)
library(grid)
library(gridExtra)


devtools::load_all(".")
dat <- read.csv(here::here("input_data/cleaned_data.csv"), check.names = FALSE)
load(file = here::here("input_data/BUGS_output.RData"))

R2WinBUGS::attach.bugs(res_bugs$BUGSoutput)

grid <- 
  expand.grid(
    levels(as.factor(dat$year)),
    levels(as.factor(dat$setting)))
grid[,"names"] <- paste(grid[,"Var1"], grid[,"Var2"])

# prob LTBI cases from screened
mcmcplots::caterplot(res_bugs, parms = c("p_ltbi"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")
# prob screen
mcmcplots::caterplot(res_bugs, parms = c("p_screen"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")
# identification rate
mcmcplots::caterplot(res_bugs, parms = c("rate_id"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")
# incident rate / year
mcmcplots::caterplot(res_bugs, parms = c("rate_inc"), reorder = FALSE,
                     labels = grid$names, labels.loc = "above")

# grid setting only (pooled year)
par(mfrow = c(3,2))
mcmcplots::caterplot(res_bugs, parms = c("sp_ltbi"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-0.1,0.3))
title("probability LTBI")
mcmcplots::caterplot(res_bugs, parms = c("sp_screen"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(0.5,1.1))
title("probability screened")
mcmcplots::caterplot(res_bugs, parms = c("srate_id"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-100,400))
title("number identified")
mcmcplots::caterplot(res_bugs, parms = c("srate_inc"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-0.1,12))
title("number of incidents")
mcmcplots::caterplot(res_bugs, parms = c("pred_n_screen"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-0.10,300))
title("number screened")
mcmcplots::caterplot(res_bugs, parms = c("pred_n_ltbi"), reorder = FALSE,
                     labels = levels(as.factor(dat$setting)), labels.loc = "above", val.lim = c(-4,20))
title("number LTBI")


###########################
# bespoke clearer plots

stan_forest_plot(res_bugs, param = "p_ltbi")
ggsave("plots/p_ltbi_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "p_screen")
ggsave("plots/p_screen_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "rate_id") + xlab("Number identified")
ggsave("plots/rate_id_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)

stan_forest_plot(res_bugs, param = "rate_inc") + xlab("Number of incidents")
ggsave("plots/rate_inc_forest_plot.png", width = 30, height = 20, units = "cm", dpi = 640)


## setting only
fp_ltbi <- stan_forest_plot_setting(res_bugs, param = "sp_ltbi", title = "Probability LTBI")
fp_screen <- stan_forest_plot_setting(res_bugs, param = "sp_screen", title = "Probability Screened")
fp_id <- stan_forest_plot_setting(res_bugs, param = "srate_id", title = "Identification rate")
fp_inc <- stan_forest_plot_setting(res_bugs, param = "srate_inc", title = "Incident rate")
fp_nscreen <- stan_forest_plot_setting(res_bugs, param = "pred_n_screen", title = "Number screened")
fp_nltbi <- stan_forest_plot_setting(res_bugs, param = "pred_n_ltbi", title = "Number LTBI")

fp <- grid_arrange_shared_legend(fp_ltbi, fp_screen, fp_id, fp_inc, fp_nscreen, fp_nltbi, nrow = 2, ncol = 3)

ggsave(plot = fp, here::here("plots/forest_plot_setting.png"), width = 40, height = 30, units = "cm", dpi = 640)

##################
# model checking

# screen
screen <- dat$`Total No Screened`
screen_cols <- grep("^screen_rep\\[", colnames(out$sims.matrix))
screen_rep_matrix <- out$sims.matrix[, screen_cols]

# Compare the variance of the observed successes to the replicated successes
ppc_stat(screen, screen_rep_matrix, stat = "var") +
  theme_minimal() +
  labs(title = "Binomial Overdispersion Check: Variance")

# Shows the count of each discrete outcome category
ppc_bars(screen, screen_rep_matrix) +
  theme_minimal() +
  labs(title = "Posterior Predictive Bar Chart of Successes")

# LTBI

latent <- dat$Latent
latent_cols <- grep("^ltbi_rep\\[", colnames(out$sims.matrix))
latent_rep_matrix <- out$sims.matrix[, latent_cols]

# Compare the variance of the observed successes to the replicated successes
ppc_stat(latent, latent_rep_matrix, stat = "var") +
  theme_minimal() +
  labs(title = "Binomial Overdispersion Check: Variance")

# Shows the count of each discrete outcome category
ppc_bars(latent, latent_rep_matrix) +
  theme_minimal() +
  labs(title = "Posterior Predictive Bar Chart of Successes")

# number of incidents ---

inc_cols <- grep("^inc_rep\\[", colnames(out$sims.matrix))
inc_rep_matrix <- out$sims.matrix[, inc_cols]
inc <- table(dat$year, dat$setting)
inc_obs_flat <- unlist(inc, use.names = FALSE)

ppc_rootogram(inc_obs_flat, inc_rep_matrix) +
  theme_minimal()

id_cols <- grep("^id_rep\\[", colnames(out$sims.matrix))
id_rep_matrix <- out$sims.matrix[, id_cols]

ppc_rootogram(dat$`Total No identified`, id_rep_matrix) +
  theme_minimal()

# split by year or setting ---

# Check if the model captures the mean LTBI counts correctly
ppc_stat_grouped(
  y = dat$Latent, 
  yrep = latent_rep_matrix, 
  group = dat$setting, 
  stat = "mean"
) +
  theme_minimal() +
  labs(title = "Mean LTBI by Setting")

ppc_stat_grouped(
  y = dat$Latent, 
  yrep = latent_rep_matrix, 
  group = dat$year, 
  stat = "mean"
) +
  theme_minimal() +
  labs(title = "Mean LTBI by Year")

# Check if the model captures the variance in screening 
ppc_stat_grouped(
  y = dat$`Total No Screened`, 
  yrep = screen_rep_matrix, 
  group = dat$setting, 
  stat = "var"
) +
  theme_minimal() +
  labs(title = "Variance in Screening by Year")

ppc_stat_grouped(
  y = dat$`Total No Screened`, 
  yrep = screen_rep_matrix, 
  group = dat$year, 
  stat = "var"
) +
  theme_minimal() +
  labs(title = "Variance in Screening by Year")
