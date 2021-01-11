
# tb incident costing study
# N Green
#
#

# combine over years for each output statistic
#   - cost per person identified
#   - cost per person screened
#   - cost per person test positive
#

library(ggplot2)
library(reshape2)
library(dplyr)
library(gridExtra)
library(cowplot)

dat <- read.csv(file = "../data/cost_per_person.csv")


#########
# plots #
#########

# totals
e1 <-
  ggplot(dat, aes(x = setting, y = total, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +         # plot points for each setting
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point",                 # plot mean as star
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point",                 # plot median as triangle
               shape = 2, size = 3, color = "black") +
  theme(legend.position = "bottom") +                            # position legend at bottom
  xlab("") + ylab("Total cost (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))       # rotate x-axis labels

# per person identified
e2 <- ggplot(dat, aes(x = setting, y = pp_identified, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point", 
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point", 
               shape = 2, size = 3, color = "black") +
  xlab("") + ylab("Cost per person identified (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# per person screened
e3 <- ggplot(dat, aes(x = setting, y = pp_screened, color = year)) +
  geom_jitter(position = position_jitter(0), size = 3) +
  theme_bw() +
  # stat_summary(fun.y = "mean", geom = "point", 
  #              shape = 8, size = 3, color = "darkorchid4" ) +
  stat_summary(fun.y = "median", geom = "point", 
               shape = 2, size = 3, color = "black") +
  xlab("Setting") + ylab("Cost per person screened (£)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# per person positive test result
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

p3 <- grid.arrange(arrangeGrob(e1 + theme(legend.position = "none"),
                               e2 + theme(legend.position = "none"),
                               e3 + theme(legend.position = "none"),
                               e4 + theme(legend.position = "none"),
                               nrow = 2),
                   mylegend, nrow = 2, heights = c(10, 1))

# grid.arrange(e1, e2,
#              e3, e4,
#              nrow = 2)

# add plot labels
p <- ggpubr::as_ggplot(p3) +                                # transform to a ggplot
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0, 0.5, 0.5), y = c(1, 0.6, 1, 0.6)) # Add labels
p
