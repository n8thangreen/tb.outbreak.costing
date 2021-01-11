
# tb incident contact investigation costing study
#
# cascade of cost per person identified, screened, positive
# pooling by year for each setting
# 
# N Green


library(dplyr)
library(reshape2)

dat <- read.csv(file = "../data/cost_per_person.csv")


plot_dat <-
  dat %>% 
  group_by(setting) %>% 
  # summarise(mean_total = log(mean(total, na.rm = TRUE)),
  #           mean_latent = log(mean(pp_latent, na.rm = TRUE)),
  #           mean_screen = log(mean(pp_screened, na.rm = TRUE)),
  #           mean_id = log(mean(pp_identified, na.rm = TRUE))) %>%
  summarise(Total = mean(total, na.rm = TRUE),
            Latent = mean(pp_latent, na.rm = TRUE),
            Screen = mean(pp_screened, na.rm = TRUE),
            Identified = mean(pp_identified, na.rm = TRUE)) %>%
  melt(id.vars = "setting")


plot_dat$setting <-
  plot_dat$setting %>% 
  plyr::mapvalues(from = c("commercial", "education", "factory/workplace", "hospital/clinic/care centre", "other", "total"),
                  to =   c("Commercial", "Education", "Factory/workplace", "Hospital/clinic/care centre", "Other", "Total"))

names(plot_dat)[names(plot_dat) == "setting"] <- "Setting"

ggplot(plot_dat,
       aes(x = variable, y = value, colour = Setting, group = Setting)) +
  scale_y_continuous(trans = 'log2') +  # log scale
  # coord_trans(y = "log2") +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("Cost per person (£)")

