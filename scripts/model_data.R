## model parameter values loaded from central config CSV

rm(list = ls())

save_csv <- TRUE

# We can use the package load_parameters function to populate the global environment,
# or we can read and evaluate the CSV directly in globalenv().
# Sourcing this script is done to populate globalenv(), so we pass globalenv().
# If the package is loaded, load_parameters is exported, so we can use it!
if (requireNamespace("tb.outbreak.costing", quietly = TRUE)) {
  tb.outbreak.costing::load_parameters(globalenv())
} else {
  # Fallback if package is not installed/loaded
  csv_path <- here::here("inst/extdata/parameters.csv")
  params_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  eval_env <- new.env(parent = emptyenv())
  for (i in seq_len(nrow(params_df))) {
    name <- params_df$parameter[i]
    val_str <- params_df$value[i]
    val <- tryCatch({
      eval(parse(text = val_str), envir = eval_env)
    }, error = function(e) {
      val_str
    })
    assign(name, val, envir = eval_env)
    assign(name, val, envir = globalenv())
  }
}

# Now compute derived parameters that need global variables (e.g. for BIRM)
c_inc_meet_BIRM <<-
  t_inc_meet*ADJUSTED_SALARY(
    c_TBphys_outside_hr + c_hpp_outside_hr + c_nurse_lead_outside_hr + 2*c_nurse_6_outside_hr,
    c_TBphys_outside_yr + c_hpp_outside_yr + c_nurse_lead_outside_yr + 2*c_nurse_6_outside_yr)

c_meeting_review_BIRM <<-
  t_rev_meet*ADJUSTED_SALARY(
    c_TBphys_outside_hr + c_hpp_outside_hr + c_nurse_lead_outside_hr + 2*c_nurse_6_outside_hr,
    c_TBphys_outside_yr + c_hpp_outside_yr + c_nurse_lead_outside_yr + 2*c_nurse_6_outside_yr)

p_invite <<- 1 - p_site_screen
odds_advise <<- (1 - p_screen_incid)/p_screen_incid

# save all as csv table for backward compatibility/reference
if (save_csv) {
  rm(save_csv)
  
  params <- mget(ls())
  tab <-
    data.frame(name = names(params),
               value = as.matrix(params),
               row.names = NULL)
  
  write.csv(as.matrix(tab), file = here::here("input_data/param_vals.csv"))
}

rm(params, tab)
