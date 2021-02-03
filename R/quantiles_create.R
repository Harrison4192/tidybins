quantiles_create <- function(mdb, col, quantile_num = 10){
  col <- rlang::ensym(col)

  name <- rlang::sym(stringr::str_glue("{rlang::as_name(col)}_v{quantile_num}"))



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
  mdb %>% dplyr::mutate(cumul_frac = cumul / col_sum * quantile_num) -> mdb
  mdb %>% dplyr::mutate(!!name := cumul_frac %>% ceiling) -> mdb
  mdb %>% dplyr::group_by(mycol) %>% dplyr::mutate(!!name := max(!!name)) %>% dplyr::ungroup()-> mdb
  mdb %>% dplyr::select(-cumul, -cumul_frac, -col_sum, -mycol) -> mdb
  mdb %>% dplyr::relocate(!!name, .after = !!col) -> mdb

  mdb

}
