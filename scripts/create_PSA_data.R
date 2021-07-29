
# generate PSA input data
# for total_year_cost()
#
# sample from MoM distn from bootstrapped data
# i.e. smoother but same shape
#
# sample total number in each (year, setting)
# this is the input for Excel model


n_samples <- 1000

load(file = here::here("data", "tab_per_inc.RData"))


# method of moments parameter values --------------------------------------

beta_params <- list()
gamma_params <- list()

beta_params$pscreen <- map2_df(tab_per_inc$mu_pscreen,
                               tab_per_inc$sd_pscreen,
                               .f = MoM_beta)
beta_params$platent <- map2_df(tab_per_inc$mu_platent,
                               tab_per_inc$sd_platent,
                               .f = MoM_beta)

gamma_params$inc <- map2_df(tab_per_inc$mu_inc,
                            tab_per_inc$sd_inc,
                            .f = MoM_gamma)
gamma_params$id <- map2_df(tab_per_inc$mu_id_per_inc,
                           tab_per_inc$sd_id_per_inc,
                           .f = MoM_gamma)

write.csv(as.data.frame(beta_params),
          file = here::here("data", "beta_params.csv"),
          row.names = FALSE)

write.csv(as.data.frame(gamma_params),
          file = here::here("data", "gamma_params.csv"),
          row.names = FALSE)


# incidents ---------------------------------------------------------------

# using mean and range in normal
# sample_inc <- rnorm_boot(tab$mu_inc, tab$lCI_inc)

# using gamma parameters
sample_inc <- map2_dfc(.x = gamma_params$inc$shape,
                       .y = gamma_params$inc$scale,
                       .f = ~ rgamma(n = n_samples, shape = .x, scale = .y))

# check against means
# hist(sample_inc[[4]], breaks = 20)
# abline(v = tab_per_inc$mu_inc[4], col = "red")

# sample_inc <- as_tibble(cbind(year = orig_dat$year, sample_inc))
# write.csv(sample_inc, file = "sample_inc.csv")

sample_inc <- data.frame(setting = orig_means$setting, t(sample_inc))
write.csv(sample_inc, file = here::here("data", "sample_inc_mean.csv"))


# identified --------------------------------------------------------------

# using mean and range in normal
# sample_id <- rnorm_boot(tab$mu_id, tab$lCI_id)

# using gamma parameters
sample_id <- map2_dfc(.x = gamma_params$id$shape,
                      .y = gamma_params$id$scale,
                      .f = ~ rgamma(n = n_samples, shape = .x, scale = .y))

sample_id <- data.frame(setting = orig_means$setting, t(sample_id))
write.csv(sample_id, file = here::here("data", "sample_id_mean.csv"))

# sample_id <- as_tibble(cbind(year = orig_dat$year, sample_id))
# sample_id <- include_year_totals(sample_id)
# write.csv(sample_id, file = "sample_id.csv")


# screened ----------------------------------------------------------------

# using mean and range in normal
# sample_screen <- rnorm_boot(tab$mu_s, tab$lCI_s)

# using beta parameters
sample_screen <- map2_dfc(.x = beta_params$pscreen$a,
                          .y = beta_params$pscreen$b,
                          .f = ~ rbeta(n = n_samples, shape1 = .x, shape2 = .y))

# check against means
# hist(sample_screen[[5]], breaks = 40)
# abline(v = tab_per_inc$mu_pscreen[5], col = "red")

sample_screen <- data.frame(setting = orig_means$setting, t(sample_screen))
write.csv(sample_screen, file = here::here("data", "sample_screen_mean.csv"))

# sample_screen <- as_tibble(cbind(year = orig_dat$year, sample_screen))
# sample_screen <- include_year_totals(sample_screen)
# write.csv(sample_screen, file = "sample_screen.csv")


# ltbi positive tests -----------------------------------------------------

# using mean and range in normal
# sample_ltbi <- rnorm_boot(tab$mu_ltbi, tab$lCI_ltbi)

# using beta parameters
sample_ltbi <- map2_dfc(.x = beta_params$platent$a,
                        .y = beta_params$platent$b,
                        .f = ~ rbeta(n = n_samples, shape1 = .x, shape2 = .y))

sample_ltbi <- data.frame(setting = orig_means$setting, t(sample_ltbi))
write.csv(sample_ltbi, file = here::here("data", "sample_ltbi_mean.csv"))

# sample_ltbi <- as_tibble(cbind(year = orig_dat$year, sample_ltbi))
# sample_ltbi <- include_year_totals(sample_ltbi)
# write.csv(sample_ltbi, file = "sample_ltbi.csv")


# # check against original data
# hist(sample_ltbi[30, ], breaks = 50)
# abline(col = "red", v = orig_dat$latent[20])



## generate a fixed sample of uniform[0, 1] to use in Excel
## model with inverse cdf
n_setting <- 5
n_rep <- 1000

runif_inc <- matrix(data = runif(n_setting*n_rep), nrow = n_setting)
runif_id  <- matrix(data = runif(n_setting*n_rep), nrow = n_setting)
runif_screen <- matrix(data = runif(n_setting*n_rep), nrow = n_setting)
runif_ltbi <- matrix(data = runif(n_setting*n_rep), nrow = n_setting)

write.csv(runif_inc, file = "data/runif_inc.csv")
write.csv(runif_id, file = "data/runif_id.csv")
write.csv(runif_screen, file = "data/runif_screen.csv")
write.csv(runif_ltbi, file = "data/runif_ltbi.csv")



