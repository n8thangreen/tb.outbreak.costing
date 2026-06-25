# R/config.R

#' Package environment to store costing parameters
#' @export
pkg_env <- new.env(parent = emptyenv())

#' Load parameters from the central CSV file
#' @param envir The environment to load the parameters into
#' @export
load_parameters <- function(envir) {
  csv_path <- system.file("extdata/parameters.csv", package = "tb.outbreak.costing")
  if (csv_path == "") {
    csv_path <- here::here("inst/extdata/parameters.csv")
  }
  if (!file.exists(csv_path)) {
    stop("Parameters CSV file not found at: ", csv_path)
  }
  
  params_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # For evaluation to find earlier variables, we evaluate in a temporary environment
  eval_env <- new.env(parent = parent.env(envir))
  
  for (i in seq_len(nrow(params_df))) {
    name <- params_df$parameter[i]
    val_str <- params_df$value[i]
    
    # Try to parse and evaluate the value in eval_env
    val <- tryCatch({
      eval(parse(text = val_str), envir = eval_env)
    }, error = function(e) {
      # If evaluation fails (e.g. string syntax like "TB Nurse Band 3"), treat as character
      val_str
    })
    
    # Assign the evaluated value to both the evaluation environment (so subsequent
    # formulas can refer to it) and the target environment
    assign(name, val, envir = eval_env)
    assign(name, val, envir = envir)
  }
  
  return(params_df$parameter)
}

# Populate default values from central CSV
param_names <- load_parameters(pkg_env)

# Setup active bindings in package environment during load
# This allows referencing these variables directly in the package namespace,
# falling back to the global environment if defined there (for scripts/model_data.R overrides).
env <- environment()

for (name in param_names) {
  local({
    nm <- name
    makeActiveBinding(nm, function(v) {
      if (!missing(v)) assign(nm, v, envir = pkg_env)
      if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
        get(nm, envir = .GlobalEnv, inherits = FALSE)
      } else {
        get(nm, envir = pkg_env)
      }
    }, env)
  })
}

# Helper function to get a parameter with global-env override
get_override <- function(nm, default_val) {
  if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
    get(nm, envir = .GlobalEnv, inherits = FALSE)
  } else {
    default_val
  }
}

# Computed variables bindings with global overrides
makeActiveBinding("c_inc_meet_BIRM", function(v) {
  get_override("c_inc_meet_BIRM", 
    t_inc_meet * ADJUSTED_SALARY(
      c_TBphys_outside_hr + c_hpp_outside_hr + c_nurse_lead_outside_hr + 2*c_nurse_6_outside_hr,
      c_TBphys_outside_yr + c_hpp_outside_yr + c_nurse_lead_outside_yr + 2*c_nurse_6_outside_yr
    )
  )
}, env)

makeActiveBinding("c_meeting_review_BIRM", function(v) {
  get_override("c_meeting_review_BIRM",
    t_rev_meet * ADJUSTED_SALARY(
      c_TBphys_outside_hr + c_hpp_outside_hr + c_nurse_lead_outside_hr + 2*c_nurse_6_outside_hr,
      c_TBphys_outside_yr + c_hpp_outside_yr + c_nurse_lead_outside_yr + 2*c_nurse_6_outside_yr
    )
  )
}, env)

makeActiveBinding("p_invite", function(v) {
  get_override("p_invite", 1 - p_site_screen)
}, env)

makeActiveBinding("odds_advise", function(v) {
  get_override("odds_advise", (1 - p_screen_incid) / p_screen_incid)
}, env)
