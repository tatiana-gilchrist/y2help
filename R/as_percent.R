#' As Percent
#' 
#' This takes a column of numbers in decimal format and turns it into percent format.
#' @param df The dataframe with the number column. Defaults to the name frequencies 
#' @param var The name of the number column. Defaults to the name result
#' @return values will go from decimal to percent format like such: 0.03 -> "3%"
#' @examples 
#' frequencies <- c(.30,.47,.25, .65, .42,.57,.74,.19,.54,.50,.46,.28,.45) %>% 
#' as.data.frame() %>% 
#'  rename(
#'    result = "."
#'  )
#'  
#'  as_percent()

as_percent <- function(
  df = frequencies,
  var = result
){
  var_flag <- dplyr::enquo(var)
  
  df %>% 
    mutate(
      result_per = str_c(!!var_flag * 100, '%')
    )
}
