# R/config.R

#' Package environment to store costing parameters
#' @export
pkg_env <- new.env(parent = emptyenv())

# Populate default values
pkg_env$p_pension18 <- 0.2060
pkg_env$p_pension19 <- 0.1430
pkg_env$p_admin <- 0.0008
pkg_env$p_apprent <- 0.005
pkg_env$p_NI <- 0.1505
pkg_env$p_pensionNI <- 0.3498
pkg_env$d_avail <- 253
pkg_env$d_pubhol <- 8
pkg_env$d_leave0 <- 27
pkg_env$d_leave5 <- 29
pkg_env$d_leave10 <- 33
pkg_env$d_actual <- 224
pkg_env$d_actual_hrs <- 1815
pkg_env$days_2018 <- 261
pkg_env$NI_min <- 8632.52
pkg_env$NI_min_week <- 166.01
pkg_env$c_phleb <- 220
pkg_env$c_apptnurse <- 85
pkg_env$c_drive <- 0.50
pkg_env$t_admin_post <- 0.25
pkg_env$t_admin_appt <- 0.33
pkg_env$t_admin_id <- 0.25
pkg_env$admin_job <- "TB Nurse Band 3"
pkg_env$t_QandA <- 2
pkg_env$t_inform <- 0.5
pkg_env$t_inform_pp <- 0.33
pkg_env$t_siteRA <- 2
pkg_env$t_phoneRA <- 1
pkg_env$t_phone_preRA <- 0.25
pkg_env$t_site_screen <- 7.5
pkg_env$d_site <- 20
pkg_env$t_rev_meet <- 1
pkg_env$t_inc_meet <- 1
pkg_env$max_screen <- 100
pkg_env$t_enquire <- 2
pkg_env$p_site_screen <- 0.9
pkg_env$p_screen_incid <- 0.85
pkg_env$c_fup_appt <- 93
pkg_env$c_blood <- 36
pkg_env$c_TST <- 1.32 * (1.035)^4
pkg_env$c_TBphys_outside_yr <- 86449
pkg_env$c_TBphys_outside_hr <- 86449/1815
pkg_env$c_hpp_outside_yr <- 49218
pkg_env$c_hpp_outside_hr <- 49218/1815
pkg_env$c_nurse_3_outside_yr <- 20330
pkg_env$c_nurse_3_outside_hr <- 20330/1815
pkg_env$c_nurse_6_outside_yr <- 34172
pkg_env$c_nurse_6_outside_hr <- 34172/1815
pkg_env$c_nurse_7_outside_yr <- 42121
pkg_env$c_nurse_7_outside_hr <- 42121/1815
pkg_env$c_nurse_lead_outside_yr <- 47126
pkg_env$c_nurse_lead_outside_hr <- 47126/1815
pkg_env$c_meeting_weekly <- 43.58
pkg_env$c_Tx <- 0

# Setup active bindings in package environment during load
# This allows referencing these variables directly in the package namespace,
# falling back to the global environment if defined there (for scripts/model_data.R overrides).
env <- environment()

param_names <- c("p_pension18", "p_pension19", "p_admin", "p_apprent", "p_NI", "p_pensionNI", 
                 "d_avail", "d_pubhol", "d_leave0", "d_leave5", "d_leave10", "d_actual", 
                 "d_actual_hrs", "days_2018", "NI_min", "NI_min_week", "c_phleb", "c_apptnurse", 
                 "c_drive", "t_admin_post", "t_admin_appt", "t_admin_id", "admin_job", 
                 "t_QandA", "t_inform", "t_inform_pp", "t_siteRA", "t_phoneRA", "t_phone_preRA", 
                 "t_site_screen", "d_site", "t_rev_meet", "t_inc_meet", "max_screen", 
                 "t_enquire", "p_site_screen", "p_screen_incid", "c_fup_appt", "c_blood", 
                 "c_TST", "c_TBphys_outside_yr", "c_TBphys_outside_hr", "c_hpp_outside_yr", 
                 "c_hpp_outside_hr", "c_nurse_3_outside_yr", "c_nurse_3_outside_hr", 
                 "c_nurse_6_outside_yr", "c_nurse_6_outside_hr", "c_nurse_7_outside_yr", 
                 "c_nurse_7_outside_hr", "c_nurse_lead_outside_yr", "c_nurse_lead_outside_hr", 
                 "c_meeting_weekly", "c_Tx")

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
