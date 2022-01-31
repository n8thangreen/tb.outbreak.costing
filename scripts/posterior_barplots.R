
# TB incident contact investigation costing study
# N Green

##TODO: should really do these calcs directly on the posterior
##      samples and then do the summary stats, rather than
##      the other way around


library(ggplot2)
library(reshape2)
library(dplyr)


dat <- read.csv(here::here("input_data/cleaned_data.csv"), check.names = FALSE)
load(file = here::here("input_data/BUGS_output.RData"))

R2WinBUGS::attach.bugs(res_bugs$BUGSoutput)

grid <- 
  expand.grid(
    levels(as.factor(dat$year)),
    levels(as.factor(dat$setting)))
grid[,"names"] <- paste(grid[,"Var1"], grid[,"Var2"])


########
# prep #
########

param <- c("pred_n_ltbi", "pred_n_screen", "srate_id")
sims <- res_bugs$BUGSoutput$sims.list[param]

setting <- c("Commercial","Education","Factory/workplace",
             "Hospital/clinic/care centre","Other")

n_inc <- 
  as_tibble(res_bugs$BUGSoutput$sims.list[["srate_inc"]]) %>% 
  summarise(across(everything(), list(mean))) %>% 
  `colnames<-`(setting) %>% 
  melt(value.name = "n_inc")

plot_dat <-
  rbind(
    summarise(as_tibble(sims[[1]]), across(everything(), list(mean))),
    summarise(as_tibble(sims[[2]]), across(everything(), list(mean))),
    summarise(as_tibble(sims[[3]]), across(everything(), list(mean)))) %>% 
  mutate(param = param) %>% 
  `colnames<-`(c(setting, "param")) %>% 
  melt(id.vars = "param") %>% 
  group_by(variable) %>% 
  mutate(group_only = value - lag(value),
         group_only = ifelse(is.na(group_only), value, group_only)) %>% 
  ungroup() %>% 
  merge(n_inc) %>% 
  mutate(value_per_year = group_only*n_inc)


########
# plot #
########

##########################
## per incident

# percent
ggplot() +
  geom_bar(aes(y = group_only, x = variable, fill = param),
           data = plot_dat, position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = "plots/stacked_barplot_posterior_percent.png",
       width = 20, height = 20, units = "cm")


# total counts
ggplot() +
  geom_bar(aes(y = group_only, x = variable, fill = param),
           data = plot_dat, stat = "identity") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = "plots/stacked_barplot_posterior_counts.png",
       width = 20, height = 20, units = "cm")

##########################
## per year

# total counts
ggplot() +
  geom_bar(aes(y = value_per_year, x = variable, fill = param),
           data = plot_dat, stat = "identity") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave(filename = "plots/stacked_barplot_posterior_count_year.png",
       width = 20, height = 20, units = "cm")
