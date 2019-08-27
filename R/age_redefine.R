#' Age Redefine
#' 
#' This function takes an age variable in either year form (i.e. 1995) or age form (i.e. 24)
#' and turns it into a standard grouped age variable called AgeRange. 
#' @param df This is the name of the dataframe with the age variable
#' @param var This is the name of the age variable
#' @return Returns a variable with the following age ranges: "18-24", "25-34", "35-44", "45-54", "55-64", "65+"
#' @examples 
#' x <- c(1989,	  1972,	  1994,	  1954,	  1977,	  1962,	  1945,	  2000,	  1965,	  1969,	  1973,	  1991,	  1974,	  1987,	  1969,	  1981,	  1959,	  1980,	  1949,	  1975,	  1985,	  1952,	  1959,	  1973,	  1968,	  1973,	  1998,	  1991,	  1991,	1972) %>% 
#' as.data.frame() %>% 
#'  rename(
#'    year = "."
#'  )
#'
#' age_redefine(x, year)
#'
#'OR
#'
#' x <- c(30,47,25, 65, 42,57,74,19,54,50,46,28,45,32,50,38,60,39,70,44,34,67,60,46,51,46,21,28,28,47) %>% 
#' as.data.frame() %>% 
#'  rename(
#'    year = "."
#'  )
#'
#' age_redefine(x, year)




age_redefine <- function(
  df,
  var
){
  var_flag <- dplyr::enquo(var)
  
  if(df %>% select(!!var_flag) %>% max(na.rm = T) < 150){
    df %>% 
      mutate(
        var_numeric = as.numeric(!!var_flag)
      ) %>% 
      mutate(
        AgeRange = case_when(
          var_numeric <= 24 ~ '18-24',
          var_numeric <= 34 ~ '25-34',
          var_numeric <= 44 ~ '35-44',
          var_numeric <= 54 ~ '45-54',
          var_numeric <= 64 ~ '55-64',
          var_numeric > 64 ~ '65+'
        )
      ) %>% 
      select(
        -var_numeric
      )
  } else if(df %>% select(!!var_flag) %>% max(na.rm = T) > 1000){
    df %>% 
      mutate(
        var_numeric = as.numeric(!!var_flag)
      ) %>% 
      mutate(
        age_numeric = 2019 - !!var_flag
      ) %>% 
      mutate(
        AgeRange = case_when(
          age_numeric <= 24 ~ '18-24',
          age_numeric <= 34 ~ '25-34',
          age_numeric <= 44 ~ '35-44',
          age_numeric <= 54 ~ '45-54',
          age_numeric <= 64 ~ '55-64',
          age_numeric > 64 ~ '65+'
        )
      ) %>% 
      select(
        -var_numeric,
        -age_numeric
      )
  }
}

