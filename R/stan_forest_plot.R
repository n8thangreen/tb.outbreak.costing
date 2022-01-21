
#' Forest plot using BUGS output
#'
#' @param folder String of location
#'
#' @importFrom epicontacts, adegenet
#' @import dplyr ggplot2 tidybayes purrr
#'
#' @return
#' @export
#'
##TODO: convert to this problem
#
stan_forest_plot <- function(folder = "data/",
                             save_name = ) {
  
  plot_dat <-
    flatten_dfr(summary_tabs) %>%
    mutate(scenario = rownames(.),
           event = ifelse(cf == "cf_global", "Cure fraction global",
                          ifelse(cf == "cf_os", "Cure fraction OS",
                                 "Cure fraction PFS")),
           treatment = ifelse(tx == "1]", "Ipilimumab",
                              ifelse(tx == "2]", "Nivolumab",
                                     "Nivolumab + Ipilimumab"))) %>%
    mutate(distns = as.factor(distns),
           cpt = ifelse(cpt == "_cpt_12m", "12 months",
                        ifelse(cpt == "_cpt_30m", "30 months",
                               "Complete")),
           cpt = as.factor(cpt)) %>%
    select(-cf, -tx) %>%
    rename(`Cut-point` = cpt,
           `Distribution` = distns)
  
  plot_dat %>%
    ggplot(aes(x = mean, y = event,
               xmin = `2.5%`, xmax = `97.5%`,
               colour = `Distribution`,
               shape = `Cut-point`,
               size = 2)) +
    tidybayes::geom_pointinterval(position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.8),
               aes(x = mean, y = event,
                   colour = `Distribution`, shape = `Cut-point`),
               size = 5, inherit.aes = FALSE) +
    facet_grid(. ~ treatment) +
    scale_shape_manual(values = c(3, 4, 15, 17, 19)) +
    ylab("") +
    xlab("Posterior probability") +
    xlim(0, 0.8) +
    theme_bw() +
    theme(axis.text = element_text(size = 20),
          strip.text.x = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          axis.title.x = element_text(size = 20),
          axis.text.x = element_text(size = 20),
          panel.spacing = unit(2, "lines"))
}

