#' charvec to formula
#'
#' @param lhs lhs
#' @param rhs rhs
#' @keywords internal
#'
#' @return formula

charvec_to_formula <- function(lhs, rhs){

  if(rlang::is_empty(rhs)){return(NULL)} else{

    stringr::str_c(rhs, collapse = " + ") %>%
      stringr::str_c(lhs, " ~ ", ., collapse = "")  %>%
      parse(text = .) %>%
      eval()}
}
#' tidy formula construction
#'
#' @param .data dataframe
#' @param target lhs
#' @param ... tidyselect. rhs
#'
#' @return a formula
#' @export
tidy_formula <- function(.data, target, ...){

  rlang::as_name(rlang::ensym(target)) -> lhs_var

  .data %>%
    select_otherwise(..., otherwise = tidyselect::everything(), return_type = "names") %>%
    setdiff(lhs_var) -> rhs_vars

  charvec_to_formula(lhs_var, rhs_vars)
}






make_pretty <- function(.data, abbv, pretty_labels) {
  if (!pretty_labels) {
    rgx <- stringr::str_c("_", abbv, "[0-9]*$")

    .data %>%
      dplyr::mutate(dplyr::across(tidyselect::matches(rgx), as.integer))
  } else{
    .data
  }
}

rename_bin_lens <- function(bin_df, abbv, cols){

  bin_df %>%
    dplyr::summarize(dplyr::across(.cols = cols, .fns =  ~dplyr::n_distinct(remove_nas(.)))) %>%
    purrr::map_chr(1) %>%
    stringr::str_c("_", abbv, .) -> bin_lens


  bin_df %>%
    dplyr::rename_with( .fn = ~stringr::str_c(., bin_lens), .cols = cols)
}

#' one_wrapper
#'
#' @param bin_cols cols
#' @param .data dataframe
#' @param abbv char
#' @param bin_method char. bin method.
#' @param n_bins integer. number of bins
#' @param pretty_labels pretty_labels
#' @importFrom framecleaner make_na
#'
#' @return output
#' @export
#'
oner_wrapper <- function(bin_cols, .data, abbv, bin_method, n_bins = n_bins, pretty_labels = pretty_labels) {

  bin_cols %>%
    OneR::bin(nbins = n_bins, method = bin_method, na.omit = F) %>%
    framecleaner::make_na(tidyselect::everything(), vec = "NA")  -> bin_df

  bin_df %>% rename_bin_lens(abbv = abbv, cols = tidyselect::everything()) -> bin_df

  bin_df  %>% dplyr::bind_cols(.data) -> .data

  .data %>% make_pretty(abbv = abbv, pretty_labels = pretty_labels)
}


#' remove nas
#'
#' @param x vec
#' @keywords internal
#'
#' @return vec
#'
remove_nas <- function(x){

  x[which(!is.na(x))]
}
