#' plot_estimates_size
#'    Plot estimates and +/- 2 std errors of intercept by lab sample size. 
#'
#' @param df a date frame containing x, y, and Group columns
#' @param model - an lmer model
#' 
plot_estimates_size <- function(df,
                                model) {
  require(lmerTest)
  require(tidyverse)
  require(tidymodels)
  require(broom.mixed)
  require(RColorBrewer)
  require(ggpubr)
  require(arm)
  
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  colors <- getPalette(length(unique(df$Group)))
  
  df2 <- df %>%
    group_by(Group) %>%
    count()
  
  df2 <- df2 %>%
    add_column(se_ranef = se.ranef(model)$Group[, "(Intercept)"],
               mixed_est = coef(model)$Group[, "(Intercept)"])
  
  p <- ggplot(df2) +
    geom_point(aes(x = log(n), y = mixed_est, color = Group)) + 
    scale_colour_manual(values = colors) +
    # ylim(c(-0.05, 0.25)) +
    geom_errorbar(data = df2, aes(x = log(n), y = mixed_est,
                                  ymin = mixed_est - 2 * se_ranef, ymax = mixed_est + 2 * se_ranef)) +
    geom_abline(aes(intercept = fixef(model)[1], slope = 0)) +
    ggtitle(as.character(model@call)[2]) +
    xlab('Log Group Sample Size') +
    ylab('Intercept Estimate (+/- 2 std errors)')
  
  print(p)
}