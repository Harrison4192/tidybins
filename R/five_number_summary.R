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
