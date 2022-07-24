#' dotplot_ranef - Plot random effects for each Lab with standard deviarion
#' Similar to lattice::dotplot
#'
#' @param model - An lmer model
#' 
dotplot_ranef <- function(model) {
  require(tidyverse)
  require(ggpubr)
  require(rlist)
  
  df <- tibble(as.data.frame(ranef(model), condVar = TRUE))
  
  df2 <- df %>% 
    filter(term == '(Intercept)')
  
  plot_list <- list()
  
  p1 <- ggplot(df2) +
    geom_point(aes(x = condval, y = grp)) + 
    geom_errorbarh(aes(y = grp, xmin = condval - condsd, xmax = condval + condsd)) +
    ylab('Group') + 
    xlab('Intercept Random Effect')
  plot_list <- list.append(plot_list, p1)
  
  df3 <- df %>% 
    filter(term == 'x')
  
  p2 <- ggplot(df3) +
    geom_point(aes(x = condval, y = grp)) + 
    geom_errorbarh(aes(y = grp, xmin = condval - condsd, xmax = condval + condsd)) +
    ylab('Group') + 
    xlab('x Random Effect') 
  plot_list <- list.append(plot_list, p2)
  
  p3 <- ggarrange(plotlist = plot_list,
                  ncol = 2, nrow = 1)
  p3 <- annotate_figure(p3, top = text_grob(paste('Random Effects', as.character(model@call)[2])))
  print(p3)
}
