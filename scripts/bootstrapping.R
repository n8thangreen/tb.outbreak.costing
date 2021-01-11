
# TB incident contact investigation costing
# N Green
#
# bootstrap raw incidence data then aggregate
# to get 95% CI estimates using quantiles
# and then sample from normal distns
# with these parameter values to get counts of
# incidents, identified, screened and latent positive


library(reshape2)
library(dplyr)
library(rsample)
library(purrr)
library(tidyr)
library(broom)


source("scripts/model_data.R")


########
# prep #
########

# individual incidents counts by setting and year
dat_raw <-
  readxl::read_xlsx(
    path = here::here("../../data", "Birmingham", "incidents2.xlsx"),
    sheet = "data")

dat <- dat_raw[, c("year",
                   "setting2",
                   "Total No identified",
                   "Total No Screened",
                   "Latent")]

names(dat)[names(dat) == "setting2"] <- "setting"

# remove incidents with missing data
dat <- dat[dat$year %in% 2013:2018, ]
dat <- dat[!is.na(dat$`Total No identified`), ]
dat <- dat[!is.na(dat$`Total No Screened`), ]

dat$Latent[is.na(dat$Latent)] <- 0

dat <-
  dat %>% 
  mutate(setting = factor(setting),
         p_screen = `Total No Screened`/`Total No identified`,  #prop screened of identified for each incident
         p_ltbi = `Latent`/`Total No Screened`)                 #prop ltbi of screened for each incident


# total number of individuals within each year and setting
# and proportions
total_year_setting <-
  dat %>%
  sum_by_group(year, setting)

# # mean number of individuals within each setting
# dat_means <-
#   dat %>%
#   mean_by_group(setting)

# create to
# check against bootstrap estimates
dat_means <-
  mean_by_setting(total_year_setting)


#############
# bootstrap #
#############
#https://cran.r-project.org/web/packages/rsample/vignettes/Basics.html

# 100 bootstraps samples
boots <- bootstraps(dat, times = 100)

## bootstrap statistic
# over each bootstrap sample
# as.data.frame() transforms from bootstrap object

boot_tabs <-
  boots %>% 
  mutate(
    # mean totals across years for each setting
    total =
      map(splits,
          function(x)
            mean_by_setting(
              sum_by_group(dat = as.data.frame(x),
                           year, setting))),
    # total individuals within each year and setting
    per_inc =
      map(splits,
          function(x)
            sum_by_group(dat = as.data.frame(x),
                         year, setting)))

# - mean of year totals for each setting
# - mean(#identified)/mean(#incidents) 
# stack all bootstrap dataframes into single long format
# stacked_boot <- 
#   boot_tabs %>%
#   select(-splits, -per_inc) %>%
#   # Turn it into a tibble by stacking
#   unnest(cols = c(total)) %>% 
#   mutate(id_per_inc = round(identified/incidents, 2),
#          screen_per_inc = round(screen/incidents, 2),
#          latent_per_inc = round(latent/incidents, 2))



# bootstrapped costs ------------------------------------------------------

# - year totals for each setting
# - mean(#identified/#incidents) 
stacked_per_inc <- 
  boot_tabs %>%
  select(-splits, -total) %>%
  unnest(cols = c(per_inc)) %>% 
  group_by(id, setting) %>% 
  summarise_at(vars(identified:latent_per_inc),
               function(x) round(median(x, na.rm = TRUE), 2)) %>% 
  mutate(screen_per_inc2 = round(id_per_inc*p_screen, 2),
         latent_per_inc2 = round(screen_per_inc2*p_ltbi, 2),
         total_cost = vtotal_year_cost(incidents,
                                       id_per_inc,
                                       screen_per_inc2,
                                       latent_per_inc2),
         cost_per_id = total_cost/identified,
         cost_per_screen = total_cost/screen,
         cost_per_ltbi = total_cost/latent)

write.csv(stacked_per_inc,
          file = here::here("data", "stacked_per_inc.csv"),
          row.names = FALSE)


# view bootstrap samples
# par(mfrow = c(3,3))
# 
# for (i in 2010:2018){
#   hist(stacked_boot[stacked_boot$year == i &
#                     stacked_boot$setting == "education", "screen"] %>%
#          unlist(),
#        breaks = 40,
#        main = i)
# }


# calculate mean and upper, lower bounds of 95% CI
# tab_total <-
#   stacked_boot %>% 
#   # group_by(year, setting) %>% # for totals
#   group_by(setting) %>%         # for means
#   summarise(
#     mu_id = round(mean(identified, na.rm = TRUE), 0),
#     lCI_id = round(quantile(identified, probs = 0.25, na.rm = TRUE), 0),
#     uCI_id = round(quantile(identified, probs = 0.975, na.rm = TRUE), 0),
#     mu_s = round(mean(screen, na.rm = TRUE), 0),
#     lCI_s = round(quantile(screen, probs = 0.25, na.rm = TRUE), 0),
#     uCI_s = round(quantile(screen, probs = 0.975, na.rm = TRUE), 0),
#     mu_ltbi = round(mean(latent, na.rm = TRUE), 0),
#     lCI_ltbi = round(quantile(latent, probs = 0.25, na.rm = TRUE), 0),
#     uCI_ltbi = round(quantile(latent, probs = 0.975, na.rm = TRUE), 0),
#     mu_inc = round(mean(incidents, na.rm = TRUE), 0),
#     lCI_inc = round(quantile(incidents, probs = 0.25, na.rm = TRUE), 0),
#     uCI_inc = round(quantile(incidents, probs = 0.975, na.rm = TRUE), 0))
# 
# tab_total$sigma_inc <- (tab_total$mu_inc - tab_total$lCI_inc)/1.96
# tab_total$sigma_id <- (tab_totalab$mu_id - tab_total$lCI_id)/1.96
# tab_total$sigma_screen <- (tab_total$mu_s - tab_total$lCI_s)/1.96
# tab_total$sigma_ltbi <- (tab_total$mu_ltbi - tab_total$lCI_ltbi)/1.96
# 
# # write.csv(tab, file = here::here("data", "bar_dat_boot.csv"), row.names = FALSE)  
# write.csv(tab_total, file = here::here("data", "bar_dat_boot_mean.csv"), row.names = FALSE)  


## summary table

tab_per_inc <-
  stacked_per_inc %>% 
  # group_by(year, setting) %>% # for totals
  group_by(setting) %>%         # for means
  summarise(
    mu_inc  = mean(incidents, na.rm = TRUE),
    sd_inc  = sd(incidents, na.rm = TRUE),
    lCI_inc = round(quantile(incidents, probs = 0.025, na.rm = TRUE), 2),
    uCI_inc = round(quantile(incidents, probs = 0.975, na.rm = TRUE), 2),
    
    mu_id_per_inc  = mean(id_per_inc, na.rm = TRUE),
    sd_id_per_inc  = sd(id_per_inc, na.rm = TRUE),
    lCI_id_per_inc = round(quantile(id_per_inc, probs = 0.025, na.rm = TRUE), 2),
    uCI_id_per_inc = round(quantile(id_per_inc, probs = 0.975, na.rm = TRUE), 2),
    
    mu_s  = round(mean(screen_per_inc2, na.rm = TRUE), 2),
    sd_s  = round(sd(screen_per_inc2, na.rm = TRUE), 2),
    lCI_s = round(quantile(screen_per_inc2, probs = 0.025, na.rm = TRUE), 2),
    uCI_s = round(quantile(screen_per_inc2, probs = 0.975, na.rm = TRUE), 2),
    
    mu_ltbi  = round(mean(latent_per_inc2, na.rm = TRUE), 2),
    sd_ltbi  = round(sd(latent_per_inc2, na.rm = TRUE), 2),
    lCI_ltbi = round(quantile(latent_per_inc2, probs = 0.025, na.rm = TRUE), 2),
    uCI_ltbi = round(quantile(latent_per_inc2, probs = 0.975, na.rm = TRUE), 2),
    
    mu_platent  = round(mean(p_ltbi, na.rm = TRUE), 2),
    sd_platent  = sd(p_ltbi, na.rm = TRUE),
    lCI_platent = round(quantile(p_ltbi, probs = 0.025, na.rm = TRUE), 2),
    uCI_platent = round(quantile(p_ltbi, probs = 0.975, na.rm = TRUE), 2),
    
    mu_pscreen  = round(mean(p_screen, na.rm = TRUE), 2),
    sd_pscreen  = sd(p_screen, na.rm = TRUE),
    lCI_pscreen = round(quantile(p_screen, probs = 0.025, na.rm = TRUE), 2),
    uCI_pscreen = round(quantile(p_screen, probs = 0.975, na.rm = TRUE), 2),
    
    mu_cost  = mean(total_cost, na.rm = TRUE),
    sd_cost  = sd(total_cost, na.rm = TRUE),
    lCI_cost = round(quantile(total_cost, probs = 0.025, na.rm = TRUE), 2),
    uCI_cost = round(quantile(total_cost, probs = 0.975, na.rm = TRUE), 2),
    
    mu_cost_per_id  = mean(cost_per_id, na.rm = TRUE),
    sd_cost_per_id  = sd(cost_per_id, na.rm = TRUE),
    lCI_cost_per_id = round(quantile(cost_per_id, probs = 0.025, na.rm = TRUE), 2),
    uCI_cost_per_id = round(quantile(cost_per_id, probs = 0.975, na.rm = TRUE), 2),

    mu_cost_per_screen  = mean(cost_per_screen, na.rm = TRUE),
    sd_cost_per_screen  = sd(cost_per_screen, na.rm = TRUE),
    lCI_cost_per_screen = round(quantile(cost_per_screen, probs = 0.025, na.rm = TRUE), 2),
    uCI_cost_per_screen = round(quantile(cost_per_screen, probs = 0.975, na.rm = TRUE), 2),
    
    mu_cost_per_ltbi = mean(cost_per_ltbi, na.rm = TRUE),
    sd_cost_per_ltbi  = sd(cost_per_ltbi, na.rm = TRUE),
    lCI_cost_per_ltbi = round(quantile(cost_per_ltbi, probs = 0.025, na.rm = TRUE), 2),
    uCI_cost_per_ltbi = round(quantile(cost_per_ltbi, probs = 0.975, na.rm = TRUE), 2)
  )


##TODO: why this doesnt equal the sd() values?
# tab_per_inc$sigma_id <- (tab_per_inc$mu_id - tab_per_inc$lCI_id)/1.96
# tab_per_inc$sigma_inc <- (tab_per_inc$mu_inc - tab_per_inc$lCI_inc)/1.96
# tab_per_inc$sigma_screen <- (tab_per_inc$mu_s - tab_per_inc$lCI_s)/1.96
# tab_per_inc$sigma_ltbi <- (tab_per_inc$mu_ltbi - tab_per_inc$lCI_ltbi)/1.96
# tab_per_inc$sigma_platent <- (tab_per_inc$mu_platent - tab_per_inc$lCI_platent)/1.96
# tab_per_inc$sigma_pscreen <- (tab_per_inc$mu_pscreen - tab_per_inc$lCI_pscreen)/1.96

# write.csv(tab, file = here::here("data", "bar_dat_boot.csv"), row.names = FALSE)  
write.csv(tab_per_inc,
          file = here::here("data", "dat_boot_mean_per_inc.csv"),
          row.names = FALSE)

save(tab_per_inc, file = here::here("data", "tab_per_inc.RData"))

