
# TB incident contact investigation costing study
# N Green

# raw table data from Birmingham is stacked in to a single incident table 
# in Excel and then read in here.g
# This script aggregate across years and setting to get total counts.
# Plots are then made over years and setting.


library(ggplot2)
library(reshape2)
library(dplyr)


# dat <- readxl::read_xlsx(here::here("TB outbreak costing/data/Birmingham/incidents.xlsx"))
dat <- readxl::read_xlsx(here::here("input_data/Birmingham/incidents2.xlsx"), sheet = 2)

########
# prep #
########

dat2 <- dat[ , c("year", "setting2", "Total No identified", "Total No Screened", "Latent")]
names(dat2)[names(dat2) == "setting2"] <- "setting"

# sum within years and setting
bar_dat <-
  dat2 %>%
  group_by(year, setting) %>% 
  summarise(
    identified = sum(`Total No identified`, na.rm = TRUE),
    screen = sum(`Total No Screened`, na.rm = TRUE),
    latent = sum(`Latent`, na.rm = TRUE),
    incidents = n()) %>% 
  mutate(year_setting = paste(year, setting))

bar_dat$year_setting <- factor(bar_dat$year_setting,
                               levels = bar_dat$year_setting, ordered = TRUE)

write.csv(bar_dat, file = here::here("input_data", "bar_dat.csv"))


# sum within years only for all setting totals
# transform to long format for ggplots
line_dat <-
  dat2 %>%
  group_by(year) %>%
  summarise(Indentified = sum(`Total No identified`, na.rm = TRUE),
            Screened = sum(`Total No Screened`, na.rm = TRUE),
            LTBI = sum(Latent, na.rm = TRUE)) %>% 
  melt(id.vars = "year")

line_dat$year <- as.factor(line_dat$year)
line_dat$value[line_dat$value == 0] <- NA

# tidy labels
line_dat$variable <-
  line_dat$variable %>% 
  plyr::mapvalues(from = c("Identified", "Screened", "LTBI"),
                  to =   c("Identified", "Screened", "LTBI positive"))

line_dat <- 
  line_dat %>%
  rename(Year = year)


# cummulative counts for stacked bar chart
# nested groups
stack_dat <- bar_dat
stack_dat$screenonly <- stack_dat$screen - stack_dat$latent
stack_dat$identifiedonly <- stack_dat$identified - stack_dat$screen
stack_dat <- stack_dat[!stack_dat$year %in% c(2010, 2011, 2012), ]
stack_dat <- 
  stack_dat %>% 
  mutate(screen_per_inc = screenonly/incidents,
         latent_per_inc = latent/incidents,
         id_per_inc = identifiedonly/incidents)

stack_long <-
  melt(stack_dat[, c("year", "setting", "year_setting", "latent", "screenonly", "identifiedonly")],
       id.vars = c("year", "setting", "year_setting"),
       value.name = "count")

stack_long_per_inc <-
  melt(stack_dat[, c("year", "setting", "year_setting", "latent_per_inc", "screen_per_inc", "id_per_inc")],
       id.vars = c("year", "setting", "year_setting"),
       value.name = "count")


########
# plot #
########

# cascade of total counts
ggplot(line_dat,
       aes(x = variable, y = value, colour = Year, group = Year)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("") + ylab("Number of individuals") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(filename = "plots/birmingham_cascade.png",
       width = 20, height = 20, units = "cm")

######################
# bar plots

### identified
ggplot(bar_dat, aes(x = year_setting, y = identified)) +
  geom_bar(stat = "identity", fill = as.numeric(as.factor(bar_dat$setting)), colour = "white") +
  scale_x_discrete(breaks = bar_dat$year_setting, labels = bar_dat$setting,
                   expand = c(0.1,0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Number identified") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = round(seq(min(bar_dat$identified), max(bar_dat$identified), by = 200), 1))

ggsave(filename = "plots/birmingham_identified_barplot.png",
       width = 20, height = 20, units = "cm")


### screen
ggplot(bar_dat, aes(x = year_setting, y = screen)) +
  geom_bar(stat = "identity", fill = as.numeric(as.factor(bar_dat$setting)), colour = "white") +
  scale_x_discrete(breaks = bar_dat$year_setting, labels = bar_dat$setting,
                   expand = c(0.1,0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Number screened") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = round(seq(min(bar_dat$screen), max(bar_dat$screen), by = 200), 1))

ggsave(filename = "plots/birmingham_screened_barplot.png",
       width = 20, height = 20, units = "cm")

### latent
ggplot(bar_dat, aes(x = year_setting, y = latent)) +
  geom_bar(stat = "identity", fill = as.numeric(as.factor(bar_dat$setting)), colour = "white") +
  scale_x_discrete(breaks = bar_dat$year_setting, labels = bar_dat$setting,
                   expand = c(0.1, 0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Number latent TB") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = round(seq(min(bar_dat$latent), max(bar_dat$latent), by = 10), 1))

ggsave(filename = "plots/birmingham_latent_barplot.png",
       width = 20, height = 20, units = "cm")

### incidents
ggplot(bar_dat, aes(x = year_setting, y = incidents)) +
  geom_bar(stat = "identity", fill = as.numeric(as.factor(bar_dat$setting)), colour = "white") +
  scale_x_discrete(breaks = bar_dat$year_setting, labels = bar_dat$setting,
                   expand = c(0.1, 0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Number of incidents") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = round(seq(min(bar_dat$incidents), max(bar_dat$incidents), by = 1), 1))

ggsave(filename = "plots/birmingham_incidents_barplot.png",
       width = 20, height = 20, units = "cm")


##########################
## stacked

# Annotation data
annotation_data <- data.frame(
  setting = unique(stack_long$setting),
  variable = unique(stack_long$variable),
  x = unique(stack_long$year_setting),  # Unique x values
  y = rep(-15, length(unique(stack_long$year_setting))),  # Adjust y value to be below bars
  label = 1:length(unique(stack_long$year_setting))
)

# percent
ggplot() +
  geom_bar(aes(y = count, x = year_setting, fill = variable),
           data = stack_long, position = "fill", stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks = stack_long$year_setting, labels = stack_long$setting,
                   expand = c(0.1, 0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Percentage") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Group")

ggsave(filename = "plots/stacked_barplot_percent.png",
       width = 20, height = 20, units = "cm")

# total counts per year
ggplot() +
  geom_bar(aes(y = count, x = year_setting, fill = variable),
           data = stack_long, stat = "identity") +
  # scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks = stack_long$year_setting, labels = stack_long$setting,
                   expand = c(0.1, 0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0,"cm")) +
  xlab("") +
  ylab("Number of individuals") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name = "Cascade subgroup",
                      labels = c("LTBI postive", "Screened", "Identified")) +
  geom_text(data = filter(stack_long, variable == "screenonly") , aes(x = year_setting, y = -20, label = count),
            size = 3, angle = 0, hjust = 0.5) #+
  # coord_cartesian(clip = "off")


ggsave(filename = "plots/stacked_barplot_counts_year.png",
       width = 20, height = 20, units = "cm")

# total counts
ggplot() +
  geom_bar(aes(y = count, x = year_setting, fill = variable),
           data = stack_long_per_inc, stat = "identity") +
  # scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks = stack_long$year_setting, labels = stack_long$setting,
                   expand = c(0.1, 0.1)) +
  facet_grid(~ year, space = "free_x", scales = "free_x", switch = "x") +
  theme_bw() +
  theme(strip.placement = "outside",
        strip.background = element_rect(fill = NA, color = NA),
        panel.spacing = unit(0, "cm")) +
  xlab("") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Group")

ggsave(filename = "plots/stacked_barplot_counts.png",
       width = 20, height = 20, units = "cm")


