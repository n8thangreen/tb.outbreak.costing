
## on-costs

p_pension18 <<- 0.2060
p_pension19 <<- 0.1430
p_admin <<- 0.0008
p_apprent <<- 0.005
p_NI <<- 0.138
p_pensionNI <<- 0.3498

d_avail <<- 253
d_pubhol <<- 8
d_leave0 <<- 27         # annual leave days
d_leave5 <<- 29
d_leave10 <<- 33

d_actual <<- 224
days_2018 <<- 261       # working-days per year
NI_min <<- 8632.52      # national insurance
NI_min_week <<- 166.01

c_phleb <<- 220
c_apptnurse <<- 76
t_admin_post <<- 0.25
t_admin_appt <<- 0.33
t_admin_id <<- 0.25
admin_job <<- "TB Nurse Band 3"
c_drive <<- 0.50                #per mile

t_QandA <<- 2           # question and answer session
t_inform <<- 0.5
t_inform_pp <<- 0.33
t_siteRA <<- 2          # risk assessment
t_phoneRA <<- 1
t_phone_preRA <<- 0.25
t_site_screen <<- 7.5

d_site <<- 20           # average total distance to/from site
t_meet_review <<- 1     # review meeting duration
t_incid_meet <<- 1      # incident meeting duration
max_screen <<- 100      # maximum number screened per day
p_site_screen <<- 0.9   # proportion of screening event that are site visits
c_fup_appt <<- 59.96*(1.035)^2    # follow-up appointment cost (2016)
t_enquire <<- 2         # time phone line manning
p_screen_incid <<- 0.85 # probability that screening follows an Incident Management Meeting
c_blood <<- 36          # unit cost of IGRA blood test
c_TST <<- 1.32          # unit cost of skin test


## salaries

c_TBphys_outside_yr <<- 86449
c_TBphys_outside_hr <<- 44.33

c_hpp_outside_yr <<- 38765
c_hpp_outside_hr <<- 19.88

c_nurse_3_outside_yr <<- 18157
c_nurse_3_outside_hr <<- 9.28

c_nurse_6_outside_yr <<- 28746
c_nurse_6_outside_hr <<- 14.70

c_nurse_7_outside_yr <<- 33895
c_nurse_7_outside_hr <<- 17.34

c_nurse_lead_outside_yr <<- 43469
c_nurse_lead_outside_hr <<- 22.23

c_inc_meet_BIRM <<- 148.62
c_meeting_review_BIRM <<- 148.62
c_meeting_weekly <<- 43.58


p_invite <<- 1 - p_site_screen
odds_advise <<- (1 - p_screen_incid)/p_screen_incid

c_phoneRA_BIRM <<- t_phone_preRA*(c_nurse_7_outside_hr + (c_nurse_7_outside_yr - NI_min)*p_pensionNI/(days_2018*7.5))

c_siteRA_BIRM <<- 2*t_siteRA*(c_nurse_7_outside_hr + (c_nurse_7_outside_yr - NI_min)*p_pensionNI/(days_2018*7.5)) + 2*c_drive*d_site

