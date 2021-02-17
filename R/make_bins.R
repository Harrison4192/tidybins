#' Make Bins
#'
#' Make bins in a tidy fashion. Adds a column to your data frame containing the integer codes of the specified bins of a certain column.
#' Bin types are input as logical statements. Multiple bin types can be set to `TRUE`, creating multiple types of bins for one column.
#' Specifying multiple columns is only intended for supervised binning, so mutliple columns can be simultaneously binned
#' optimally with respect to a target variable.
#'
#' @param .data a data frame
#' @param col a column, vector of columns, or tidyselect
#' @param n_bins number of bins
#' @param bin_equal_frequency logical, bin type equal frequency (quantile, ntile)
#' @param bin_equal_width logical, bin type equal width (cut)
#' @param bin_equal_value logical, bin type equal value
#' @param bin_kmeans logical, bin type kmeans
#' @param bin_xgboost logical, supervised binning with xgboost. target must be specified
#' @param ... params to be passed to selected binning method
#' @param target unquoted column for supervised binning
#' @param pretty_labels logical. If true returns interval label rather than integer rank
#' @param seed seed for stochastic binning (xgboost)
#'
#' @return a tibble
#' @export
make_bins <- function(.data,
                           col,
                           n_bins = 10,
                           bin_equal_frequency = F,
                           bin_equal_width = F,
                           bin_equal_value = F,
                           bin_kmeans = F,
                           bin_xgboost = F,
                           ...,
                           target = NULL,
                           pretty_labels = TRUE,
                           seed = 1
){

if(!any(c(
  bin_equal_frequency,
  bin_equal_width,
  bin_equal_value,
  bin_kmeans,
  bin_xgboost))){

  bin_equal_frequency = T
}

  multi_cols <- rlang::enexpr(col)

  if(length(multi_cols) == 1){

  col1 <- rlang::ensym(col)
  col_str <- rlang::as_name(col1)}

  if(bin_equal_value){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_v{n_bins}"))

    .data %>%
      bin_equal_value(col = !!col1, n_bins = n_bins) -> .data

    if(pretty_labels){
      .data %>% make_labels(original_col = !!col1, bucket_col = !!col_nm) -> .data
    }
  }

  if(bin_kmeans){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_k{n_bins}"))

    .data %>%
      dplyr::pull(!!col1) %>%
      as.matrix() %>%
      ClusterR::KMeans_rcpp(clusters = n_bins, num_init = 10, ...) -> kmns

    .data %>%
      dplyr::mutate(!!col_nm := rank(kmns[["centroids"]])[kmns[["clusters"]]], .after = !!col1)  -> .data

    if(pretty_labels){
      .data %>%
        make_labels(original_col = !!col1, bucket_col = !!col_nm)  -> .data
    }

  }



  if(bin_xgboost){

    if(length(multi_cols) == 1){
    set.seed(seed)
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_x{n_bins}"))
    rlang::ensym(target) -> target1
    rlang::as_name(target1) -> outcome1
    rlang::new_formula(target1, rlang::sym(".")) -> myform

    xgb_rec <- recipes::recipe(myform, data = .data) %>%
      embed::step_discretize_xgb(!!col1, outcome = outcome1, num_breaks = n_bins, ...)

    xgb_rec <- recipes::prep(xgb_rec, training = .data)

    recipes::bake(xgb_rec, new_data = NULL) -> new_data

    new_data %>%
      dplyr::rename(!!col_nm := !!col1) %>%
      dplyr::bind_cols(.data %>% dplyr::select(!!col1)) %>%
      dplyr::relocate(!!col1, !!col_nm) -> .data}
    else{

      set.seed(seed)
      rlang::ensym(target) -> target1
      rlang::as_name(target1) -> outcome1
      rlang::new_formula(target1, rlang::sym(".")) -> myform

      xgb_rec <- recipes::recipe(myform, data = .data) %>%
        embed::step_discretize_xgb(!!multi_cols, outcome = outcome1, num_breaks = n_bins, ...)

      xgb_rec <- recipes::prep(xgb_rec, training = .data)

      recipes::bake(xgb_rec, new_data = NULL) -> new_data

      new_data %>%
        dplyr::relocate(!!multi_cols) %>%
        dplyr::mutate(dplyr::across(!!multi_cols, as.integer)) %>%
        dplyr::rename_with(.fn = ~stringr::str_c(., "_x" ,n_bins), .cols = !!multi_cols) %>%
        dplyr::bind_cols(.data %>% dplyr::select(!!multi_cols)) %>%
        dplyr::relocate(!!multi_cols) -> .data
    }
  if(!pretty_labels){
    .data %>%
      dplyr::mutate(dplyr::across(tidyselect::matches("_[wfvkx][0-9]*$"), as.integer)) -> .data
  }
  }

  if(pretty_labels){
    pretty_labels <- NULL
  }

  if(bin_equal_frequency){

    col_nm <- rlang::sym(stringr::str_glue("{col_str}_f{n_bins}"))


    .data %>%
      dplyr::mutate(!!col_nm := ggplot2::cut_number(!!col1, n = n_bins, labels = pretty_labels), .after = !!col1) -> .data
  }

  if(bin_equal_width){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_w{n_bins}"))

    .data %>%
      dplyr::mutate(!!col_nm := ggplot2::cut_interval(!!col1, n = n_bins, labels = pretty_labels), .after = !!col1) -> .data
  }



  .data

}


