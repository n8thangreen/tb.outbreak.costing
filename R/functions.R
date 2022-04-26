
# method of moments beta distn parameter estimation
MoM_beta <- function(xbar,
                     vbar) {
  if (vbar == 0) {
    stop("zero variance not allowed")
  } else if (xbar * (1 - xbar) < vbar) {
    stop("mean or var inappropriate")
  } else{
    a <- xbar * (((xbar * (1 - xbar)) / vbar) - 1)
    b <- (1 - xbar) * (((xbar * (1 - xbar)) / vbar) - 1)
  }
  list(a = a, b = b)
}

# method of moments gamma distn parameter estimation
MoM_gamma <- function(mean,
                      var) {
  stopifnot(var >= 0)
  stopifnot(mean >= 0)
  names(mean) <- NULL
  names(var)  <- NULL
  
  list(shape = mean ^ 2 / var,
       scale = var / mean)
}

# use for second/outer statistic of bootstrap totals
mean_by_setting <- function(dat){
  
  dat %>%
    group_by(setting) %>% 
    summarise(
      identified = median(identified, na.rm = TRUE),
      screen = median(screen, na.rm = TRUE),
      latent = median(latent, na.rm = TRUE),
      incidents = median(incidents, na.rm = TRUE),
      p_screen = median(p_screen, na.rm = TRUE),
      p_ltbi = median(p_screen, na.rm = TRUE),
      id_per_inc = median(id_per_inc, na.rm = TRUE),
      screen_per_inc = median(screen_per_inc, na.rm = TRUE),
      latent_per_inc = median(latent_per_inc, na.rm = TRUE)
    )
}

# summary statistic of individuals
# within each group (e.g. year and setting )
fn_by_group <- function(dat, fn, ...){
  
  dat %>%
    group_by(...) %>% 
    summarise(
      identified = fn(`Total No identified`, na.rm = TRUE),
      screen = fn(`Total No Screened`, na.rm = TRUE),
      latent = fn(`Latent`, na.rm = TRUE),
      incidents = n()) %>% 
    mutate(p_screen = screen/identified,
           p_ltbi = latent/screen,
           id_per_inc = identified/incidents,  #num identified per incident
           screen_per_inc = screen/incidents,  #num screened per incident
           latent_per_inc = latent/incidents)  #num ltbi per incident
}

sum_by_group <- purrr::partial(fn_by_group, fn = sum)
mean_by_group <- purrr::partial(fn_by_group, fn = mean)

# generate sample from bootstrap statistic distribution
rnorm_boot <- function(mu, lCI,
                       n_sample = 100){
  sample_res <- NULL
  
  for (i in seq_along(mu)){
    
    sample_res <-
      rnorm(n = n_sample,
            mean = mu[i],
            # mean =    , # from raw data
            sd = (mu[i] - lCI[i])/1.96) %>% 
      round(digits = 2) %>% 
      pmax(0) %>%     #left censoring at origin
      rbind.data.frame(sample_res, .)
  }
  
  sample_res
}

#
include_year_totals <- function(sample_dat){
  
  sample_dat %>%
    group_by(year) %>%
    summarise_all(sum) %>%               #column totals
    rbind.data.frame(sample_dat, .) %>%  #append to bottom
    arrange(year)
}


# derived model values

c_phoneRA_BIRM <- function() t_phone_preRA*(c_nurse_7_outside_hr + (c_nurse_7_outside_yr - NI_min)*p_pensionNI/(days_2018*7.5))

c_siteRA_BIRM <- function() 2*t_siteRA*(c_nurse_7_outside_hr + (c_nurse_7_outside_yr - NI_min)*p_pensionNI/(days_2018*7.5)) + 2*c_drive*d_site

