
# tb incident costing study
# N Green
#
#

# combine over years for each output statistic
#   cost per person identified
#   cost per person screened
#   cost per person test positive
#

library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)

dat <- read.csv("C:/Users/ngreen1/Google Drive/TB outbreak costing/data/cost_per_person.csv")


########
# prep #
########

dat$setting <- factor(dat$setting, levels = c(levels(dat$setting), "total"))
dat$setting[dat$setting == ""] <- "total"

dat$year <- factor(dat$year)

########
# plot #
########

e1 <-
  ggplot(dat, aes(x = setting, y = total, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point", 
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point", 
               shape = 2, size = 3, color = "black") +
  theme(legend.position = "bottom") +
  xlab("") + ylab("Total cost (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

e2 <- ggplot(dat, aes(x = setting, y = pp_identified, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point", 
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point", 
               shape = 2, size = 3, color = "black") +
  xlab("") + ylab("Cost per person identified (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

e3 <- ggplot(dat, aes(x = setting, y = pp_screened, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point", 
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point", 
               shape = 2, size = 3, color = "black") +
  xlab("Setting") + ylab("Cost per person screened (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

e4 <- ggplot(dat, aes(x = setting, y = pp_latent, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point", 
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point", 
               shape = 2, size = 3, color = "black") +
  xlab("Setting") + ylab("Cost per person positive test (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend <- function(a.gplot){
  
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(e1)

p3 <- grid.arrange(arrangeGrob(e1 + theme(legend.position="none"),
                               e2 + theme(legend.position="none"),
                               e3 + theme(legend.position="none"),
                               e4 + theme(legend.position="none"),
                               nrow = 2),
                   mylegend, nrow = 2, heights = c(10, 1))

grid.arrange(e1, e2,
             e3, e4,
             nrow = 2)
