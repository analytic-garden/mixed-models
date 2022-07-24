#' plot_lm_intercept
#'    Plot data showing showing different groups with color and linear fits as lines.
#'
#' @param df - a dataframe containg columns x, y, and Group
#' @param model - a linear model
#' @param title - an optional title for the plot.
#'
plot_lm_intercept <- function(df, model, title=NULL) {
  require(tidyverse)
  require(RColorBrewer)

  # get some colors. With lots of factors, this doesn't really show well.  
  getPalette <- colorRampPalette(brewer.pal(9, "Set1"))
  colors <- getPalette(length(unique(df$Group)) + 1)
  
  df2 <- data.frame(df, fit = fitted(model)) # add fitted values to dataframe
  
  p <- ggplot() +
    geom_point(data = df2, aes(x = x, y = y, color = Group))
  
  # plot a line for each group
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
  
  # is this an lmer model? Add fixed effect line to plot
  if(class(model) == "lmerModLmerTest") {
    df3 <- data.frame(x = df$x, y = fixef(model)[1] + fixef(model)[2] * df$x, Group = rep('fixef', nrow(df)))
    p <- p + geom_line(data = df3, aes(x = x, y = y, color = Group))
  }
  
  p <- p + scale_color_manual(values = colors)
  p <- p + guides(color = guide_legend(nrow = 2, byrow = TRUE))
  p <- p + theme(legend.position = "bottom")
  
  print(p)
}