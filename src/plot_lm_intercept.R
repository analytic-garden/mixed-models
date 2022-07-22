#' plot_lm_intercept
#'    Plot data showing showing different groups with color and linear fits as lines.
#'
#' @param df - a dataframe containg columns x, y, and Group
#' @param model - a liner model
#' @param title - an optional title for the plot.
#'
plot_lm_intercept <- function(df, model, title=NULL) {
  require(tidyverse)
  require(RColorBrewer)

  # get some colors. With lots of factors, this doesn't really show well.  
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  colors <- getPalette(21)
  
  df2 <- data.frame(df, fit = model$fitted.values) # add fitted values to dataframe
  
  p <- ggplot() +
    geom_point(data = df2, aes(x = x, y = y, color = Group)) +
    scale_color_manual(values = colors)
  
  groups <- sort(unique(df2$Group))
  for(gr in groups) {
    index <- which(df2$Group == gr)
    p <- p + geom_line(data = df2[index, ], aes(x = x, y = fit, color = Group))
  }

  p <- p + xlab('x') + 
    ylab('y')
  
  if(! is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  p <- p + guides(color = guide_legend(nrow = 2, byrow = TRUE))
  p <- p + theme(legend.position = "bottom")
  
  print(p)
}