#' summarize bins
#'
#' Returns a summary of all bins created by `make_bins` in a data frame. Takes no arguments other than the data frame
#' but relies on regular expressions based of the `make_bins` output in order to identify the corresponding columns.
#'
#'
#' @param mdb dataframe output from make_bins
#'
#' @return a tibble
#' @export
summarize_bins <- function(mdb){

  column <- rank <- label <-  NULL

  mdb %>% names %>% stringr::str_subset("_[a-z][a-z][0-9]*$") -> cols

  bucket_rgx <- stringr::str_c(cols,  collapse = "|")

  mdb %>% names %>% stringr::str_subset(bucket_rgx) %>% rlang::syms()  -> bucks


  blist <- list()


  for(buck in bucks){

  mdb %>% dplyr::select(!!buck) %>% names() -> bnames

  bnames %>% stringr::str_extract("[a-z][a-z][0-9]*$")-> suffix
  suffix %>% stringr::str_remove("[0-9]*$")  -> suffix_letter
  suffix %>% stringr::str_remove("^[a-z][a-z]")  -> suffix_number

  bnames %>% stringr::str_remove("_[a-z][a-z][0-9]*$") %>% rlang::sym() -> org_col

    switch(suffix_letter,
           "ew" = "equal width",
           "ef" = "equal freq",
           "ev" = "equal value",
           "km" = "kmeans",
           "xg" = "xgboost",
           "wo" = "weight of evidence") -> method

    mdb %>%
      numeric_summary(original_col = !!org_col, bucket_col = !!buck) %>%
      dplyr::mutate(column := rlang::as_name(org_col),
                    method = method,
                    n_bins = suffix_number,
                    .before = 1) %>%
      dplyr::rename_with(function(x)c("rank", "label"), c(4,5)) %>%
      dplyr::mutate(rank = as.integer(rank),
                    label = as.character(label)) -> mdb1

  blist %>% rlist::list.append(mdb1) -> blist
  }


  blist %>% purrr::reduce(dplyr::bind_rows)
}
