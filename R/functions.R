
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

