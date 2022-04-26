##TODO: read in parameter table and loop over 
##      use mean posterior rather than whole sample


# dsa_costs.R:
# calculate total cost using mean BUGS output
# for probabilities and counts


library(dplyr)
library(purrr)
library(ggplot2)
library(reshape2)


# load parameter values
source(here::here("scripts/model_data.R"))
load(here::here("input_data/BUGS_output.RData"))

dat <-
  read.csv(
    here::here("input_data/cleaned_data.csv"), check.names = FALSE)

dsa_inputs <-
  read.csv(
    here::here("input_data/dsa_inputs.csv"), check.names = FALSE)

# output list by setting
num_settings <- 5
n_dsa <- nrow(dsa_inputs)
mcmc_dat <- res_bugs$BUGSoutput$sims.list
n_samples <- 2# res_bugs$BUGSoutput$n.sims
names_setting <- levels(as.factor(dat$setting))

out_total <- vector(mode = "list",
                    length = n_dsa)
out_per_inc <- vector(mode = "list",
                      length = n_dsa)
cost_per_ltbi <- vector(mode = "list",
                        length = n_dsa)

for (j in seq_len(n_dsa)) {
  
  # set deterministic parameter values
  list2env(as.list(dsa_inputs[j, ]), envir = globalenv())
  out_total[[j]] <- vector(mode = "list",
                           length = num_settings)
  out_per_inc[[j]] <- vector(mode = "list",
                             length = num_settings)
  cost_per_ltbi[[j]] <- vector(mode = "list",
                               length = num_settings)
  
  for (s in seq_len(num_settings)) {
    for (i in 1:n_samples) {
      
      ## year totals
      out_total[[j]][[s]][i] <-
        total_year_cost(
          inc_sample = mcmc_dat$srate_inc[i,s],
          id_per_inc = mcmc_dat$srate_id[i,s],
          screen_per_inc = mcmc_dat$pred_n_screen[i,s],
          ltbi_per_inc = mcmc_dat$pred_n_ltbi[i,s])
      
      ## per incident
      out_per_inc[[j]][[s]][i] <-
        total_year_cost(
          inc_sample = 1,
          id_per_inc = mcmc_dat$srate_id[i,s],
          screen_per_inc = mcmc_dat$pred_n_screen[i,s],
          ltbi_per_inc = mcmc_dat$pred_n_ltbi[i,s])
      
      cost_per_ltbi[[j]][[s]][i] <-
        out_per_inc[[j]][[s]][i]/mcmc_dat$pred_n_ltbi[i,s]
    }
  }
  names(out_total[[j]]) <- names_setting
  names(out_per_inc[[j]]) <- names_setting
  names(cost_per_ltbi[[j]]) <- names_setting
}


saveRDS(out_total, file = here::here("input_data", "cost_BUGS_setting_dsa.Rds"))
saveRDS(out_per_inc, file = here::here("input_data", "cost_BUGS_setting_per_inc_dsa.Rds"))


##########
# output #
##########

xx <- list()
for (i in seq_along(out_total)) {
  xx[[i]] <- map_dbl(out_total[[i]], mean)
}
dsa_output <- cbind(dsa_inputs, do.call(rbind, xx))

# clean setting names
names(dsa_output) <- gsub("/|\\s", ".", names(dsa_output))
ggtorn <- list()

for (i in gsub("/|\\s", ".", names_setting)) {
  torn_formula <-
    paste0(i, " ~ t_admin_id + t_admin_appt + t_admin_post + t_siteRA + t_phone_preRA + max_screen + d_site + c_phleb + c_blood")
  s_analysis <-
    model.frame(formula = as.formula(torn_formula),
                data = dsa_output) %>% 
    ceplot::s_analysis_to_tornado_plot_data(baseline_input = c(0.25,0.33,0.25,2,0.25,100,20,220,36))
  
  ggtorn[[i]] <-
    ceplot::ggplot_tornado(s_analysis, baseline_output = dsa_output[[i]][1]) +
    ggtitle(i) +
    ylim(75000, NA)
}

library(gridExtra)
library(grid)

# do.call("grid.arrange", c(ggtorn, ncol=3))
torn_grid <- do.call("grid_arrange_shared_legend", c(ggtorn, ncol = 3, nrow = 2))

ggsave(torn_grid, filename = "plots/tornado_plots.png", dpi = 640,
       width = 20, height = 20, units = "cm")

  