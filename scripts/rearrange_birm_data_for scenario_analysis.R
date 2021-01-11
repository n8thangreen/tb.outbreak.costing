
# TB incident contact investigation costing
# N Green
#
# Rearrange wide table in to long format
# so can use for cost calculation in Excel sheet
# outbreak_costing.xlsm


library(reshape2)
library(dplyr)

dat <- read.csv(here::here("TB outbreak costing/data/birm_input_wide.csv"),
                check.names = FALSE)

xx <-
  dat %>% 
  melt(measure.vars = c("pRA phone", "pRA site"),
       variable.name = "RA",
       value.name = "pRA") %>% 
  melt(measure.vars = c("pscreen invite", "pscreen site"),
       variable.name = "screen_type",
       value.name = "pscreen_type")

xx <- 
  xx %>%
  select("year", "setting", "RA", "screen_type", "incidents",
         "identified", "screen", "latent","pRA", "pscreen_type",
         "id_per_incid", "screen_per_incid", "latent_per_incid",
         "pscreen", "platent") %>%
  rename("total_incidents" = incidents) %>%
  mutate(incidents = total_incidents*pRA*pscreen_type) %>% 
  arrange(year, RA, screen_type)

xx <- xx[xx$incidents > 0, ]
xx <- xx[!is.na(xx$identified), ]

xx <- 
  xx %>%
  select("year", "setting", "RA", "screen_type", "incidents",
         "id_per_incid", "screen_per_incid", "latent_per_incid")

write.csv(xx, file = here::here("TB outbreak costing/data/birm_input_long.csv"))
