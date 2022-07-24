#' write_mixed_model_coef - save model coefficients as a dataframe
#'
#' @param m - a model created by lmer()
#' @param model_file - where to write CSV file
#'
write_mixed_model_coefs <- function(model, model_file) {
  require(lmerTest)
  require(tidyverse)

  df_m <- tibble(coef(model)$Group) %>% 
    add_column(Group = 1:nrow(ranef(model)$Group)) %>%
    add_column(ranef_intercept = ranef(model)$Group[,1]) %>%
    rename(intercept = `(Intercept)`)
  
  if(ncol(ranef(model)$Group) > 1) {
    df_m <- df_m %>%
      add_column(ranef_SST = ranef(model)$Group[,2])
  }
  
  write.csv(df_m, file = model_file, row.names = FALSE)
}
