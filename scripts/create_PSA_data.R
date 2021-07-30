
# generate PSA input data
# for total_year_cost()
#
# sample from MoM distn from bootstrapped data
# i.e. smoother but same shape
#
# sample total number in each (year, setting)
# this is the input for Excel model


n_samples <- 1000

# summary table
load(file = here::here("data", "tab_per_inc.RData"))


setting_names <- tab_per_inc$setting


##########################################################
# method of moments parameter values

beta_params <- list()
gamma_params <- list()

beta_params$pscreen <- map2_df(tab_per_inc$mu_pscreen,
                               tab_per_inc$sd_pscreen,
                               .f = MoM_beta)

##TODO: error
beta_params$platent <- map2_df(tab_per_inc$mu_platent,
                               tab_per_inc$sd_platent,
                               .f = MoM_beta)

gamma_params$inc <- map2_df(tab_per_inc$mu_inc,
                            tab_per_inc$sd_inc,
                            .f = MoM_gamma)

gamma_params$id <- map2_df(tab_per_inc$mu_id_per_inc,
                           tab_per_inc$sd_id_per_inc,
                           .f = MoM_gamma)


#########################################################
# sample from distributions

## incidents

# using mean and range in normal
# sample_inc <- rnorm_boot(tab$mu_inc, tab$lCI_inc)

# using gamma parameters
sample_inc <-
  map2_dfc(.x = gamma_params$inc$shape,
           .y = gamma_params$inc$scale,
           .f = ~ rgamma(n = n_samples,
                         shape = .x,
                         scale = .y))

sample_inc <-
  data.frame(setting = setting_names,
             t(sample_inc))


## identified

# using mean and range in normal
# sample_id <- rnorm_boot(tab$mu_id, tab$lCI_id)

# using gamma parameters
sample_id <- map2_dfc(.x = gamma_params$id$shape,
                      .y = gamma_params$id$scale,
                      .f = ~ rgamma(n = n_samples,
                                    shape = .x,
                                    scale = .y))

sample_id <-
  data.frame(setting = setting_names,
             t(sample_id))


# sample_id <- as_tibble(cbind(year = orig_dat$year, sample_id))
# sample_id <- include_year_totals(sample_id)
# write.csv(sample_id, file = "sample_id.csv")


## screened

# using mean and range in normal
# sample_screen <- rnorm_boot(tab$mu_s, tab$lCI_s)

# using beta parameters
sample_screen <-
  map2_dfc(.x = beta_params$pscreen$a,
           .y = beta_params$pscreen$b,
           .f = ~ rbeta(n = n_samples,
                        shape1 = .x,
                        shape2 = .y))

sample_screen <-
  data.frame(setting = setting_names,
             t(sample_screen))


# sample_screen <- as_tibble(cbind(year = orig_dat$year, sample_screen))
# sample_screen <- include_year_totals(sample_screen)
# write.csv(sample_screen, file = "sample_screen.csv")


## ltbi positive tests

# using mean and range in normal
# sample_ltbi <- rnorm_boot(tab$mu_ltbi, tab$lCI_ltbi)

# using beta parameters
sample_ltbi <-
  map2_dfc(.x = beta_params$platent$a,
           .y = beta_params$platent$b,
           .f = ~ rbeta(n = n_samples,
                        shape1 = .x,
                        shape2 = .y)) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(setting = setting_names) %>% 
  relocate(setting)


# sample_ltbi <- as_tibble(cbind(year = orig_dat$year, sample_ltbi))
# sample_ltbi <- include_year_totals(sample_ltbi)
# write.csv(sample_ltbi, file = "sample_ltbi.csv")


###################################################################
# generate a fixed sample of uniform[0,1] to use in Excel
# sample other distributions with inverse cdf

runif_excel <- function(n_setting = 5, n_rep = 1000)
  matrix(data = runif(n_setting*n_rep),
         nrow = n_setting)

runif_inc <- runif_excel()
runif_id  <- runif_excel()
runif_screen <- runif_excel()
runif_ltbi <- runif_excel()


########
# save #
########

write.csv(as.data.frame(beta_params),
          file = here::here("data", "beta_params.csv"),
          row.names = FALSE)

write.csv(as.data.frame(gamma_params),
          file = here::here("data", "gamma_params.csv"),
          row.names = FALSE)

write.csv(sample_inc, file = here::here("data", "sample_inc_mean.csv"))
write.csv(sample_id, file = here::here("data", "sample_id_mean.csv"))
write.csv(sample_screen, file = here::here("data", "sample_screen_mean.csv"))
write.csv(sample_ltbi, file = here::here("data", "sample_ltbi_mean.csv"))

write.csv(runif_inc, file = here::here("data", "runif_inc.csv"))
write.csv(runif_id, file = here::here("data", "runif_id.csv"))
write.csv(runif_screen, file = here::here("data", "runif_screen.csv"))
write.csv(runif_ltbi, file = here::here("data", "runif_ltbi.csv"))

