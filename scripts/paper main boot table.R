
# TB incident contact investigation costing
# N Green
#
# paper main tables
# 1. broken down by year and setting
# 2. setting only averages


library(dplyr)

#
print_mean_with_CI <- function(mid,
                               lowerCI,
                               upperCI,
                               digits = 2) {
  
  paste0(round(mid, digits), " [",
         round(lowerCI, digits), ", ",
         round(upperCI, digits), "]")
}


####################
# year and setting #
####################

# total counts directly from raw data
# no cost calcs

## with original data

tab_dat <- read.csv(file = here::here("data", "bar_dat.csv"))

tab_dat <- tab_dat[ ,c("year", "setting", "incidents",
                       "identified", "screen", "latent")]

# include year totals
# append to bottom of array and then
# sort by year
tab_dat <-
  tab_dat %>%
  group_by(year) %>%
  summarise(incidents = sum(incidents),
            identified = sum(identified),
            screen = sum(screen),
            latent = sum(latent)) %>% 
  mutate(setting = NA) %>% 
  select(year, setting, incidents, identified, screen, latent) %>% 
  rbind.data.frame(tab_dat, .) %>% 
  arrange(year)

## for paper
## include proportion in brackets after count
# tab_dat$latent <- paste(tab_dat$latent, " (", round(tab_dat$latent/tab_dat$screen, 2), ")", sep = "")
# tab_dat$screen <- paste(tab_dat$screen, " (", round(tab_dat$screen/tab_dat$identified, 2), ")", sep = "")

## for Excel  ---
## proportions and 'per' counts in separate columns
tab_dat$platent <- round(tab_dat$latent/tab_dat$screen, 2)
tab_dat$pscreen <- round(tab_dat$screen/tab_dat$identified, 2)
tab_dat$id_per_incid <- round(tab_dat$identified/tab_dat$incidents, 2)
tab_dat$screen_per_incid <- round(tab_dat$screen/tab_dat$incidents, 2)
tab_dat$latent_per_incid <- round(tab_dat$latent/tab_dat$incidents, 2)

write.csv(tab_dat, file = here::here("paper_datatable.csv"))


## with bootstrapped data
# generated directly from the orginal raw data
# not the normal approximation samples

tab_datb <-
  read.csv(file = "bar_dat_boot.csv") %>% 
  merge(tab_dat, by = c("year", "setting"), all.y = TRUE)

## for paper
## include proportion in brackets after count
# tab_datb$latent <- paste(tab_dat$mu_ltbi, " (", round(tab_datb$mu_ltbi/tab_datb$mu_s, 2), ")",
tab_datb$latent <- print_mean_with_CI(mean = tab_datb$latent,
                                      lowerCI = tab_datb$lCI_ltbi,
                                      upperCI = tab_datb$uCI_ltbi)

tab_datb$platent <- print_mean_with_CI(mean = round(tab_datb$latent/tab_datb$screen, 2),
                                       lowerCI = NA,
                                       upperCI = NA)

# tab_datb$screen <- paste(tab_datb$mu_s, " (", round(tab_datb$mu_s/tab_datb$mu_id, 2), ")", 
tab_datb$screen <- print_mean_with_CI(mean = tab_datb$screen,
                                      lowerCI = tab_datb$lCI_s,
                                      upperCI = tab_datb$uCI_s)

tab_datb$pscreen <- print_mean_with_CI(mean = round(tab_datb$screen/tab_datb$identified, 2),
                                       lowerCI = NA,
                                       upperCI = NA)

# tab_datb$identify <- paste(tab_datb$mu_id, 
tab_datb$identified <- print_mean_with_CI(mean = tab_datb$identified,
                                          lowerCI = tab_datb$lCI_id,
                                          upperCI = tab_datb$uCI_id)

tab_datb$incidents <- print_mean_with_CI(mean = tab_datb$incidents,
                                         lowerCI = tab_datb$lCI_inc,
                                         upperCI = tab_datb$uCI_inc)

write.csv(tab_datb, file = here::here("paper_datatable_boot.csv"))



################
# setting only #
################

tab_per_inc <- read.csv(file = here::here("data", "boot_mean_per_inc.csv"))

paper_table <-
  cbind.data.frame(
    setting = tab_per_inc$setting,
    incidents = paste0(round(tab_per_inc$mu_inc, 2), " (", tab_per_inc$lCI_inc, ", ", tab_per_inc$uCI_inc, ")"),
    identified = paste0(round(tab_per_inc$mu_id_per_inc, 2), " (", tab_per_inc$lCI_id_per_inc, ", ", tab_per_inc$uCI_id_per_inc, ")"),
    screened = paste0(tab_per_inc$mu_s, " (", tab_per_inc$lCI_s, ", ", tab_per_inc$uCI_s, ")"),
    pscreened = paste0(tab_per_inc$mu_pscreen, " (", tab_per_inc$lCI_pscreen, ", ", tab_per_inc$uCI_pscreen, ")"),
    ltbi = paste0(tab_per_inc$mu_ltbi, " (", tab_per_inc$lCI_ltbi, ", ", tab_per_inc$uCI_ltbi, ")"),
    pltbi = paste0(tab_per_inc$mu_platent, " (", tab_per_inc$lCI_platent, ", ", tab_per_inc$uCI_platent, ")"),
    total_cost = paste0(round(tab_per_inc$mu_cost, 2), " (", tab_per_inc$lCI_cost, ", ", tab_per_inc$uCI_cost, ")"),
    cost_per_id = paste0(round(tab_per_inc$mu_cost_per_id, 2), " (", tab_per_inc$lCI_cost_per_id, ", ", tab_per_inc$uCI_cost_per_id, ")"),
    cost_per_screen = paste0(round(tab_per_inc$mu_cost_per_screen, 2), " (", tab_per_inc$lCI_cost_per_screen, ", ", tab_per_inc$uCI_cost_per_screen, ")"),
    cost_per_ltbi = paste0(round(tab_per_inc$mu_cost_per_ltbi, 2), " (", tab_per_inc$lCI_cost_per_ltbi, ", ", tab_per_inc$uCI_cost_per_ltbi, ")")
  )

paper_table

write.csv(paper_table,
          file = here::here("data", "paper_table.csv"),
          row.names = FALSE)


