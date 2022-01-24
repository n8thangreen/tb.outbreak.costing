
#' Forest plot using BUGS output
#'
#' @importFrom epicontacts, adegenet
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return
#' @export
#'
##TODO: convert to this problem
#
stan_forest_plot <- function(res, param) {
  
  sims <- res$BUGSoutput$sims.list[[param]]
  
  plot_dat <-
    as.data.frame.table(sims, responseName = "prob") %>%
    rename(sim = Var1, Year = Var2, Setting = Var3) %>% 
    mutate(Year = factor(Year, labels = 2013:2018),
           Setting = factor(Setting,
                            labels = c("Commercial","Education","Factory/workplace",
                                       "Hospital/clinic/care centre","Other"))) %>% 
    group_by(Year, Setting) %>%
    summarise(mean = mean(prob),
              `2.5%` = quantile(prob, 0.025),
              `97.5%` = quantile(prob, 0.975)) %>% 
    ungroup()

  
  plot_dat %>%
    ggplot(aes(x = mean, #y = event,
               xmin = `2.5%`, xmax = `97.5%`,
               colour = Year,
               shape = Setting,
               size = 2)) +
    tidybayes::geom_pointinterval(position = position_dodge(width = 0.8)) +
    # geom_point(position = position_dodge(width = 0.8),
    #            aes(x = mean, #y = event,
    #                colour = `year`,
    #                shape = `setting`),
    #            size = 5, inherit.aes = FALSE) +
    # scale_shape_manual(values = c(3, 4, 15, 17, 19)) +
    # ylab("") +
    # xlim(0, 0.8) +
    xlab("Posterior probability") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          # axis.ticks.y = element_blank(),
          axis.text = element_text(size = 20),
          strip.text.x = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          panel.spacing = unit(2, "lines")) +
          # legend.position = "top") 
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))
}

