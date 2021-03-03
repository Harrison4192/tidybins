#' bin equal value
#'
#' Bins a numeric column such that each bin contains 10% of the total value (sum) of that column.
#' Intended for positive numeric vectors that make sense to sum, such as sales. Negative and NAs get treated as 0.
#' The function never puts two rows with the same value into different bins.
#'
#' @param mdb a data frame
#' @param col unquoted column
#' @param n_bins number of bins
#'
#' @return a tibble
#' @export
bin_equal_value <- function(mdb, col, n_bins = 10){

  column<-col_sum<-column<- cumul<-cumul_frac <- mycol <-n <- NULL

  col <- rlang::ensym(col)

  name <- rlang::sym(stringr::str_glue("{rlang::as_name(col)}_va{n_bins}"))



  check_neg <- any(mdb %>% dplyr::filter(!is.na(!!col)) %>% dplyr::pull(!!col) %>% `<`(0))
  check_na <- any(is.na(mdb %>% dplyr::pull(!!col)))
  check_num <- is.numeric(mdb %>% dplyr::pull(!!col))

  if(check_neg) warning(stringr::str_glue("{rlang::as_name(col)} contains negative values. Negative values are treated as 0."), call. = F)
  if(check_na) warning(stringr::str_glue("{rlang::as_name(col)} contains NA values. NA values are treated as 0"), call. = F)
  if(!check_num) warning(stringr::str_glue("{rlang::as_name(col)} is not numeric"), call. = F,)

  mdb %>% dplyr::mutate(mycol := ifelse(is.na(!!col) | !!col < 0, 0, !!col)) -> mdb
  mdb %>% dplyr::mutate(col_sum = sum(mycol)) -> mdb
  mdb %>% dplyr::arrange(mycol)-> mdb
  mdb %>% dplyr::mutate(cumul = cumsum(mycol)) -> mdb
  mdb %>% dplyr::mutate(cumul_frac = cumul / col_sum * n_bins) -> mdb
  mdb %>% dplyr::mutate(!!name := cumul_frac %>% ceiling) -> mdb
  mdb %>% dplyr::group_by(mycol) %>% dplyr::mutate(!!name := max(!!name)) %>% dplyr::ungroup()-> mdb
  mdb %>% dplyr::select(-cumul, -cumul_frac, -col_sum, -mycol) -> mdb
  mdb %>% dplyr::relocate(!!name, .after = !!col) -> mdb
  mdb %>% dplyr::mutate(!!name := as.integer(!!name)) -> mdb


  mdb

}
