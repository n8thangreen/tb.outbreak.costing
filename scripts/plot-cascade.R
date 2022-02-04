
# tb incident contact investigation costing study
#
# cascade plot of cost per person identified, screened, positive
# pooling by year for each setting
# 
# N Green


library(dplyr)
library(reshape2)

dat <- read.csv(file = here::here("output_data/cost_per_person.csv"))


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

# cascade line plot

ggplot(plot_dat,
       aes(x = variable, y = value, colour = Setting, group = Setting)) +
  scale_y_continuous(trans = 'log2') +  # log scale
  # coord_trans(y = "log2") +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("Cost per person (?)")



############################
# posteriors

# source("posterior_barplot.R")

plot_dat <- 
  plot_dat %>% 
  mutate(param = factor(param, levels = c("srate_id", "pred_n_screen", "pred_n_ltbi")),
         value = as.numeric(value),
         total_per_year = value*n_inc)

# cascade of counts
ggplot(plot_dat,
       aes(x = param, y = total_per_year, group = variable, colour = variable)) +
       # aes(x = param, y = value, group = variable, colour = variable)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("Number of individuals") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = here::here("plots/posterior_counts_cascade.png"),
       width = 20, height = 20, units = "cm")


## costs
out_total <- load(file = here::here("input_data", "cost_BUGS_setting.Rds"))
out_per_inc <- load(file = here::here("input_data", "cost_BUGS_setting_per_inc.Rds"))

load(here::here("input_data/BUGS_output.RData"))
mcmc_dat <- res_bugs$BUGSoutput$sims.list






