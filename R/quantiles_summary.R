five_number_summary <- function(x) {
  x %>%
    summary %>%
    tibble::enframe() %>%
    t() %>%
    janitor::row_to_names(1) %>%
    tibble::as_tibble() %>%
    purrr::map_df(as.numeric) %>%
    rlang::set_names(c("min", "q1", "median", "mean", "q3", "max"))

}

quantiles_summary <- function(mdb, col, quantile_num = 10){


  col <- rlang::ensym(col)
  name <- rlang::sym(stringr::str_glue("{rlang::as_name(col)}_q{quantile_num}"))
  nm1 <- rlang::sym(stringr::str_glue("relative_value_{rlang::as_name(name)}"))
  nm2 <- rlang::sym(stringr::str_glue("count_{rlang::as_name(name)}"))
  name2 <- rlang::sym(stringr::str_glue("total_potential_{rlang::as_name(name)}"))

  mdb %>%
    dplyr::group_by(!!(name), .add = T) %>%
    dplyr::summarize(!!(name2) := sum(!!col),
           dplyr::across(!!col, five_number_summary)) %>%
    dplyr::summarize(!!(name),  !!(name2), x)-> tot_pot



  mdb %>%
    dplyr::count(!!name) %>%
    dplyr::left_join(tot_pot) %>%
    dplyr::mutate(!!nm1 := !!name2 / n ) %>%
    dplyr::mutate(!!nm1 := !!nm1 / max(!!nm1) * 100) %>%
    dplyr::arrange(dplyr::desc(!!name)) %>%
    dplyr::mutate(!!name := as.integer(!!name)) %>%
    dplyr::rename(!!nm2 := n)

}
