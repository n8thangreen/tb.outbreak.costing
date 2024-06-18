## model parameter values

rm(list = ls())

save_csv <- TRUE

## on-costs

p_pension18 <<- 0.2060
p_pension19 <<- 0.1430  # 2022
p_admin <<- 0.0008
p_apprent <<- 0.005
# p_NI <<- 0.138  # 2018
p_NI <<- 0.1505   # 2022
p_pensionNI <<- 0.3498

d_avail <<- 253
d_pubhol <<- 8
d_leave0 <<- 27         # annual leave days
d_leave5 <<- 29
d_leave10 <<- 33

d_actual <<- 224       # actual working days
d_actual_hrs <<- 1815  # 37.5*52 = 1950 without holidays and bank holidays 

days_2018 <<- 261       # working-days per year
NI_min <<- 8632.52      # national insurance
NI_min_week <<- 166.01

c_phleb <<- 220
# c_apptnurse <<- 76  # 2018
c_apptnurse <<- 85    # 2022
c_drive <<- 0.50                #per mile
t_admin_post <<- 0.25
t_admin_appt <<- 0.33
t_admin_id <<- 0.25
admin_job <<- "TB Nurse Band 3"

t_QandA <<- 2           # question and answer session
t_inform <<- 0.5
t_inform_pp <<- 0.33
t_siteRA <<- 2          # risk assessment
t_phoneRA <<- 1
t_phone_preRA <<- 0.25
t_site_screen <<- 7.5

d_site <<- 20           # average total distance to/from site

t_rev_meet <<- 1        # review meeting duration
t_inc_meet <<- 1        # incident meeting duration


max_screen <<- 100      # maximum number screened per day
t_enquire <<- 2         # time phone line manning
p_site_screen <<- 0.9   # proportion of screening event that are site visits
p_screen_incid <<- 0.85 # probability that screening follows an Incident Management Meeting
# c_fup_appt <<- 59.96*(1.035)^2    # follow-up appointment cost (2016)
c_fup_appt <<- 93       # 2022
c_blood <<- 36          # unit cost of IGRA blood test
# c_TST <<- 1.32          # unit cost of skin test, 2018
c_TST <<- 1.32*(1.035)^4    # 2022


###########
# salaries
# inside/outside London

# MD
c_TBphys_outside_yr <<- 86449  # 2018
c_TBphys_outside_hr <<- c_TBphys_outside_yr/d_actual_hrs

# c_hpp_outside_yr <<- 38765     # 2018
# c_hpp_outside_hr <<- 19.88
c_hpp_outside_yr <<- 49218     # 2022
c_hpp_outside_hr <<- c_hpp_outside_yr/d_actual_hrs

# c_nurse_3_outside_yr <<- 18157  # 2018
# c_nurse_3_outside_hr <<- 9.28
c_nurse_3_outside_yr <<- 20330  # 2022
c_nurse_3_outside_hr <<- c_nurse_3_outside_yr/d_actual_hrs

# c_nurse_6_outside_yr <<- 28746  # 2018
# c_nurse_6_outside_hr <<- 14.70
c_nurse_6_outside_yr <<- 34172   # 2022
c_nurse_6_outside_hr <<- c_nurse_6_outside_yr/d_actual_hrs

# c_nurse_7_outside_yr <<- 33895  # 2018
# c_nurse_7_outside_hr <<- 17.34
c_nurse_7_outside_yr <<- 42121   # 2022
c_nurse_7_outside_hr <<- c_nurse_7_outside_yr/d_actual_hrs

# c_nurse_lead_outside_yr <<- 43469  # 2018
# c_nurse_lead_outside_hr <<- 22.23
c_nurse_lead_outside_yr <<- 47126   # 2022
c_nurse_lead_outside_hr <<- c_nurse_lead_outside_yr/d_actual_hrs

c_meeting_weekly <<- 43.58   ##TODO: what's this?

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

# save all as csv table
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
