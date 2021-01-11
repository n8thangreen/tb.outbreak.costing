
# TB incident contact investigation costing
# N Green
#
# PSA boxplots_total
#
# total counts summary stats and boxplot from Excel model PSA
# copy and pasted code for cost per identified, screened, ltbi
# tidy and merge with main-paper-table.R


library(ggplot2)
library(dplyr)
library(reshape2)
library(tibble)


summarise_dat <- function(dat_long) {
  
  dat_long %>% 
  group_by(year, setting) %>% 
  summarise(mean = mean(value),
            median = median(value),
            L95CI = quantile(value, probs = 0.025),
            U95CI = quantile(value, probs = 0.975),
            combined = print_mean_with_CI(median, L95CI, U95CI))
}


# main --------------------------------------------------------------------

cost_per_inc <-
  readxl::read_excel(
    path = here::here("TB outbreak costing/costing xls tools/TB_incident_contact_tracing_costing/151019-TB_incident_contact_tracing_costing-Birmingham.xlsm"),
    sheet = "PSA_total",
    range = "A1:CL52")

cost_settings_only <- cost_per_inc[!is.na(cost_per_inc$setting), ]
year_setting <- cost_settings_only[, c("year", "setting")]          # extract year and setting columns

cost_per_inc$setting[is.na(cost_per_inc$setting)] <- "total"        # rename year total


## total cost

cost_long <-
  melt(cost_per_inc, id.vars = c("year", "setting")) %>% 
  filter(year >= 2013)

summary_cost <-
  summarise_dat(cost_long)

write.csv(summary_cost, file = "data/summary_psa.csv")

## cost per person identified

sample_id <-
  read.csv("data/sample_id.csv")[ ,-1] %>%   # remove row number column
  data.frame(year_setting, .) %>%            # prepend year and setting columns
  mutate(year = as.factor(year),
         setting = as.factor(setting))

# include year totals
# append to bottom and then sort by year
sample_id <-
  sample_id %>%
  select(-setting) %>% 
  group_by(year) %>%
  summarise_all(sum) %>%
  mutate(setting = "total") %>% 
  rbind.data.frame(sample_id, .) %>% 
  arrange(year)

nrep <- min(ncol(sample_id),
            ncol(cost_per_inc)) - 2          # number of samples in both datasets

cost_per_id <- cost_per_inc[ ,3:nrep]/sample_id[ ,3:nrep]

cost_per_id <-
  cost_per_id %>% 
  data.frame(cost_per_inc[, c("year", "setting")], .) %>% 
  melt(id.vars = c("year", "setting")) %>% 
  filter(year >= 2013)

summary_per_id <- summarise_dat(cost_per_id)

write.csv(summary_per_id, file = "data/summary_psa_per_id.csv")


## cost per person screened

sample_screen <-
  read.csv("data/sample_screen.csv")[ ,-1] %>%   # remove row number column
  data.frame(year_setting, .) %>%            # prepend year and setting columns
  mutate(year = as.factor(year),
         setting = as.factor(setting))

# include year totals
# append to bottom and then sort by year
sample_screen <-
  sample_screen %>%
  select(-setting) %>% 
  group_by(year) %>%
  summarise_all(sum) %>%
  mutate(setting = "total") %>% 
  rbind.data.frame(sample_screen, .) %>% 
  arrange(year)

nrep <- min(ncol(sample_screen),
            ncol(cost_per_inc)) - 2          # number of samples in both datasets

cost_per_screen <- cost_per_inc[ ,3:nrep]/sample_screen[ ,3:nrep]

cost_per_screen <-
  cost_per_screen %>% 
  data.frame(cost_per_inc[, c("year", "setting")], .) %>% 
  melt(id.vars = c("year", "setting")) %>% 
  filter(year >= 2013)

summary_per_screen <- summarise_dat(cost_per_screen)

write.csv(summary_per_screen, file = "data/summary_psa_per_screen.csv")


## cost per person latent positive

sample_ltbi <-
  read.csv("data/sample_ltbi.csv")[ ,-1] %>%   # remove row number column
  data.frame(year_setting, .) %>%              # prepend year and setting columns
  mutate(year = as.factor(year),
         setting = as.factor(setting))

# include year totals
# append to bottom and then sort by year
sample_ltbi <-
  sample_ltbi %>%
  select(-setting) %>% 
  group_by(year) %>%
  summarise_all(sum) %>%
  mutate(setting = "total") %>% 
  rbind.data.frame(sample_ltbi, .) %>% 
  arrange(year)

nrep <- min(ncol(sample_ltbi),
            ncol(cost_per_inc)) - 2          # number of samples in both datasets

cost_per_ltbi <- cost_per_inc[, 3:nrep]/sample_ltbi[, 3:nrep]

cost_per_ltbi <-
  cost_per_ltbi %>% 
  data.frame(cost_per_inc[, c("year", "setting")], .) %>% 
  melt(id.vars = c("year", "setting")) %>% 
  filter(year >= 2013)

summary_per_ltbi <- summarise_dat(cost_per_ltbi)

write.csv(summary_per_ltbi, file = "data/summary_psa_per_ltbi.csv")


########
# plot #
########
 
plot_dat <- 
  cost_long %>% 
  filter(!is.na(setting)) %>%    # remove year totals
  mutate(year = as.factor(year))

ee <- ggplot(plot_dat, aes(x = setting, y = value))

ee +
  geom_boxplot() +
  facet_wrap(~ year, scales = "free") +  # different y-axis scales for each plot
  theme_bw() +
  ylab("Total cost (£)") +
  xlab("Setting") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

        