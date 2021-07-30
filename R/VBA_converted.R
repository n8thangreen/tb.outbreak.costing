
#' ADJUSTED_SALARY
#'
#' take into account pension and
#' national insurance contributions
#' 
#' @param c_jobtitle_hr hourly
#' @param c_jobtitle_yr yearly
#'
#' @return
#' @export
#'
ADJUSTED_SALARY <- function(c_jobtitle_hr,
                            c_jobtitle_yr){
  hrs_per_day <- 7.5
  total_hrs <- days_2018 * hrs_per_day
  excess_salary <- (c_jobtitle_yr - NI_min)/total_hrs * p_pensionNI
  return(c_jobtitle_hr + excess_salary)
}


#' PATH_INVITE_BIRM
#'
#' @param n_id number identified
#' @param n_screen 
#' @param n_latent 
#'
#' @return
#' @export
#'
PATH_INVITE_BIRM <- function(n_id,
                             n_screen,
                             n_latent) {
  
  # risk assessment
  RA <- c_inc_meet_BIRM + c_phoneRA_BIRM + c_siteRA_BIRM
  screen <-
    CINVITE_SCREEN(n_id, n_screen) + CFUP(n_latent) + c_meeting_review_BIRM
  
  return(RA + screen)
}

#' CINVITE_SCREEN
#'
#' @param n_id number identified
#' @param n_screen 
#'
#' @return
#' @export
#'
CINVITE_SCREEN <- function(n_id,
                           n_screen) {
  
  # total administration time
  T_ADMIN <- t_admin_appt * n_id + t_admin_post * n_screen
  c_nurse_3_hr_adj <- ADJUSTED_SALARY(c_nurse_3_outside_hr, c_nurse_3_outside_yr)
  
  return(n_screen * (c_apptnurse + c_blood) + (T_ADMIN * c_nurse_3_hr_adj))
}


#' CALLTX
#' 
#' cost of all treatment for ltbi positive
#' 
#' @param n_latent 
#'
#' @return
#' @export
#'
CALLTX <- function(n_latent) {
  
  return(c_Tx * n_latent)
}


#' PATH_SITE_BIRM
#'
#' @param n_id number identified
#' @param n_screen 
#' @param n_latent
#' @param phleb_thresh numbered screened to use external company for taking blood
#'
#' @return
#' @export
#'
PATH_SITE_BIRM <- function(n_id,
                           n_screen,
                           n_latent,
                           phleb_thresh = 25) {
  
  RA <- c_inc_meet_BIRM + c_phoneRA_BIRM + c_siteRA_BIRM
  
  if (n_screen > phleb_thresh) {
    screen <- CSITE_SCREEN_PHLEB(n_id, n_screen)
  }
  else if (n_screen <= phleb_thresh) {
    screen <- CSITE_SCREEN_NURSE(n_id, n_screen)
  } else{
   stop("error in PATH_SITE_BIRM() phleb_thresh", call. = FALSE)
  }
  
  return(RA + screen + CFUP(n_latent) + c_meeting_review_BIRM)
}


#' CSITE_SCREEN_PHLEB
#'
#' @param n_id number identified
#' @param n_screen 
#'
#' @return
#' @export
#'
CSITE_SCREEN_PHLEB <- function(n_id,
                               n_screen){
  
  C_TESTS <- c_blood * n_screen
  T_ADMIN <- t_admin_id * n_id + t_admin_post * n_screen
  DUR = n_screen/max_screen
  n_days <- ceiling(DUR)
  TSITE <- t_site_screen * n_days
  
  c_nurse_7_hr_adj <- ADJUSTED_SALARY(c_nurse_7_outside_hr, c_nurse_7_outside_yr)
  c_nurse_3_hr_adj <- ADJUSTED_SALARY(c_nurse_3_outside_hr, c_nurse_3_outside_yr)
  c_hpp_hr_adj     <- ADJUSTED_SALARY(c_hpp_outside_hr, c_hpp_outside_yr)
  
  n_phleb <- 3
  n_drive <- 2
  
  C_PEOPLE <- (c_nurse_7_hr_adj + c_hpp_hr_adj) * TSITE + c_nurse_3_hr_adj * T_ADMIN
  C_OTHER <- C_TESTS + c_inc_meet_BIRM + (n_phleb * c_phleb + n_drive * c_drive * d_site) * n_days
  
  return(C_PEOPLE + C_OTHER)
}


#' CFUP
#' 
#' cost of follow-up appointments
#' 
#' @param n_latent 
#'
#' @return
#' @export
#'
CFUP <- function(n_latent) {
  
  return(c_fup_appt * n_latent)
}


#' PATH_INFORM
#' 
#' @param n_id number identified
#'
#' @return
#' @export
#'
PATH_INFORM <- function(n_id) {
  
  c_nurse_3_hr_adj <- ADJUSTED_SALARY(c_nurse_3_outside_hr, c_nurse_3_outside_yr)
  
  return(c_nurse_3_hr_adj * t_inform)
}


#' CSITE_SCREEN_NURSE
#'
#' @param n_id number identified
#' @param n_screen 
#'
#' @return
#' @export
#'
CSITE_SCREEN_NURSE <- function(n_id,
                               n_screen) {
  
  C_TESTS <- c_blood * n_screen
  T_ADMIN <- t_admin_id * n_id + t_admin_post * n_screen
  TSITE <- t_site_screen
  
  c_nurse_7_hr_adj <- ADJUSTED_SALARY(c_nurse_7_outside_hr, c_nurse_7_outside_yr)
  c_nurse_3_hr_adj <- ADJUSTED_SALARY(c_nurse_3_outside_hr, c_nurse_3_outside_yr)
  c_hpp_hr_adj     <- ADJUSTED_SALARY(c_hpp_outside_hr, c_hpp_outside_yr)
  
  n_nurse <- 4
  n_drive <- 5
  
  C_PEOPLE <- ((n_nurse * c_nurse_7_hr_adj + c_hpp_hr_adj) * TSITE + c_nurse_3_hr_adj * T_ADMIN)
  C_OTHER <- C_TESTS + c_inc_meet_BIRM + (n_drive * c_drive * d_site)
  
  return(C_PEOPLE + C_OTHER)
}


#' total_year_cost
#'
#' this is the main overall cost function.
#' 
#' @param inc_sample number of incidents 
#' @param id_per_inc number identified per incident  
#' @param screen_per_inc number screened per incident
#' @param ltbi_per_inc  number of ltbi per incident
#'
#' @return real
#' @export
#' 
#' @examples 
#' 
#' total_year_cost(10,10,5,1)
#' 
total_year_cost <- function(inc_sample,
                            id_per_inc,
                            screen_per_inc,
                            ltbi_per_inc){
  
  if (id_per_inc < screen_per_inc)
    stop("can't have more screened than identified", call. = FALSE)
  if (screen_per_inc < ltbi_per_inc)
    stop("can't have more ltbi than screened", call. = FALSE)
  
  invite_cost <- PATH_INVITE_BIRM(id_per_inc, screen_per_inc, ltbi_per_inc)
  site_cost <- PATH_SITE_BIRM(id_per_inc, screen_per_inc, ltbi_per_inc)
  screen_cost <- invite_cost*p_invite + site_cost*p_site_screen
  
  inc_sample*(screen_cost + odds_advise*PATH_INFORM(id_per_inc))
}


#' vectorised costs
#' in order to use in mutate()
#' @export
#' 
vtotal_year_cost <- Vectorize(total_year_cost)

