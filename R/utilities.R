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


#' @param .data dataframe
#' @param ... tidyselect
#' @param otherwise tidyselect
#' @param col tidyselect
#' @param return_type choose to return column index, names, or df. defaults to index
#'
#' @return integer vector by default. possibly data frame or character vector
#' @keywords internal
#'
select_otherwise <- function(.data, ..., otherwise = NULL, col = NULL, return_type = c("index", "names", "df")){

  return_type <- match.arg(return_type)

  .dots <- rlang::expr(c(...))


  col <- rlang::enexpr(col)
  otherwise = rlang::enexpr(otherwise)


  tidyselect::eval_select(.dots, data = .data) -> eval1

  if(length(eval1) == 0){
    tidyselect::eval_select( otherwise, data = .data) -> eval1
  }

  tidyselect::eval_select(col, data = .data) %>%
    c(eval1) %>% sort() -> eval1


  if(return_type == "df"){

    out <- .data %>% dplyr::select(tidyselect::any_of(eval1))
  } else if(return_type == "names"){
    out <- names(eval1)
  } else{
    out <- eval1
  }

  out
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
    dplyr::summarize(dplyr::across(.cols = cols, .fns =  dplyr::n_distinct)) %>%
    purrr::map_chr(1) %>%
    stringr::str_c("_", abbv, .) -> bin_lens


  bin_df %>%
    dplyr::rename_with( .fn = ~stringr::str_c(., bin_lens), .cols = cols)
}

oner_wrapper <- function(bin_cols, .data, abbv, bin_method, n_bins = n_bins, pretty_labels = pretty_labels) {

  bin_cols %>% OneR::bin(nbins = n_bins, method = bin_method) -> bin_df

  bin_df %>% rename_bin_lens(abbv = abbv, cols = tidyselect::everything()) -> bin_df

  bin_df  %>% dplyr::bind_cols(.data) -> .data

  .data %>% make_pretty(abbv = abbv, pretty_labels = pretty_labels)
}
