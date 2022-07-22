#' write_model - write a model as a dataframe in a CSV file
#'
#' @param m - a model name. The name of an object containing the output of lm() or lmer()
#' @param model_file - where to save CSV file
#' @param is_intercept_only - is this only an intecept model? Defaul = FALSE
#'
write_model <- function(m, 
                        model_file, 
                        is_intercept_only = FALSE,
                        confidence_limits = FALSE) {
  require(lmerTest)
  require(tidyverse)
  require(tidymodels)
  require(broom.mixed)
  
  #' get_stars - get string symbol of significance like those displayed by summary.lm().
  #'
  #' @param pr - double. A p-value.
  #'
  #' @return a string inidcating level of significance.
  #' @export
  #'
  get_stars <- function(pr) {
    # return a test indicator of significance
    if(is.na(pr)) {
      return('')
    }
    
    if(pr < 0.001) {
      return('***')
    }
    else if(pr < 0.01) {
      return('**')
    }
    else if(pr < 0.05) {
      return('*')
    }
    else if(pr < 0.1) {
      return('.')
    }
    else {
      return('')
    }
  }
  
  # m <- force(model_name)
  df_m <- tidy(m)
  df_m$Significance <- unlist(map(df_m$p.value, get_stars))
  
  if(confidence_limits) { 
    # handle mean only models separately
    if(is_intercept_only) {
      conf <- confint(m)
      df_m <- df_m %>% mutate(conf_2_5_pct = conf[1,1], conf_97_5_pct = conf[1,2])
    }
    else {
      # catch errors calling confint
      tryCatch(
        {
          conf <- confint(m, oldNames = FALSE)
          df_m[1:2, c('conf_2_5_pct', 'conf_97_5_pct')] <- conf[c('(Intercept)', 'x'),]
          df_m[3:nrow(conf), c('conf_2_5_pct', 'conf_97_5_pct')] <- conf[1:(nrow(conf) - 2),]
        },
        
        error = function(e) {
          conf <- confint(m, oldNames = FALSE, method = 'Wald')
          df_m[1:2, c('conf_2_5_pct', 'conf_97_5_pct')] <- conf[c('(Intercept)', 'x'),]
          df_m[3:nrow(conf), c('conf_2_5_pct', 'conf_97_5_pct')] <- conf[1:(nrow(conf) - 2),]
        },
        
        finally = function(e) {
          df_m[, c('conf_2_5_pct', 'conf_97_5_pct')] <- NA
        }
      )
    }
  }
  
  write.csv(df_m, file = model_file, row.names = FALSE)
}