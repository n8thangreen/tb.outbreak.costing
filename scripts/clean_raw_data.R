
# clean raw incident data


library(dplyr)

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
  mutate(
    setting = factor(setting),
    p_screen = `Total No Screened`/`Total No identified`,  # prop screened of identified per incident
    p_ltbi = `Latent`/`Total No Screened`)                 # prop ltbi of screened per incident

##TODO: confirm what this should be
# row 132 too many latent. should be 1?
dat <-
  mutate(dat, Latent = pmin(`Total No Screened`, Latent))

write.csv(dat, file = "data/cleaned_data.csv", row.names = FALSE)

