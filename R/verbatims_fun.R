#' Open Ended Verbatims Function {verbatims_fun}
#'
#' This function takes open ended variables and compiles them to be used in topline reports.
#' @param df This is the name of the dataframe
#' @param freq_var This is the name or names of the open ended variables
#' @return Returns a data frame with the variable name, variable label, and individual responses. If there is no label it just says "no label"


verbatims_fun_single <- function(
  df,
  freq_var
){
  freq_flag <- dplyr::enquo(freq_var)

  var_label_list <- orderlabel::taking_names(df) %>%
    mutate(
      label = as.character(label)
    ) %>%
    mutate(
      label = case_when(
        label == "NULL" ~ "No label",
        label == "" ~ "No label",
        TRUE ~ label
      )
    ) %>%
    select(
      label
    ) %>%
    unlist()


  var_label(df) <- var_label_list

  freq_df <- df %>%
    select(
      !!freq_flag
    ) %>%
    gather(
      variable,
      label
    )

  labels <- df %>%
    select(
      !!freq_flag
    ) %>%
    mutate_all(
      list(
        ~case_when(
          is.na(.) ~ var_label(.),
          TRUE ~ var_label(.)
        )
      )
    ) %>%
    gather(
      variable,
      prompt
    ) %>%
    distinct(
      variable,
      .keep_all = T
    )

  left_join(freq_df, labels, by = c("variable")) %>%
    select(
      variable,
      prompt,
      label
    ) %>%
    filter(
      label != ""
    )

}



verbatims_fun <- function(
  df,
  ...
){
  freq_flags <- dplyr::quos(...)

  suppressWarnings(
    frequencies <- purrr::map_dfr(
      .x = freq_flags,
      .f = function(freq_flag) {
        verbatims_fun_single(df, !!freq_flag)
      }
    )
  )

}

