create_buckets <- function(.data,
                           col,
                           n_buckets = 10,
                           bucket_type_equal_frequency = F,
                           bucket_type_equal_width = F,
                           bucket_type_equal_value = F,
                           bucket_type_kmeans = F,
                           bucket_type_xgboost = F,
                           ...,
                           target = NULL,
                           seed = 1
){

if(!any(c(
  bucket_type_equal_frequency,
  bucket_type_equal_width,
  bucket_type_equal_value,
  bucket_type_kmeans,
  bucket_type_xgboost))){

  bucket_type_equal_value = T
}

  multi_cols <- rlang::enexpr(col)

  if(length(multi_cols) == 1){

  col1 <- rlang::ensym(col)
  col_str <- rlang::as_name(col1)}


  if(bucket_type_equal_frequency){

    col_nm <- rlang::sym(stringr::str_glue("{col_str}_f{n_buckets}"))

    .data %>%
      dplyr::mutate(!!col_nm := ggplot2::cut_number(!!col1, n = n_buckets, labels = F), .after = !!col1) -> .data
  }

  if(bucket_type_equal_width){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_w{n_buckets}"))

    .data %>%
      dplyr::mutate(!!col_nm := ggplot2::cut_interval(!!col1, n = n_buckets, labels = F), .after = !!col1) -> .data
  }

  if(bucket_type_equal_value){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_v{n_buckets}"))

    .data %>%
      quantile_value(col = !!col1, quantile_num = n_buckets) -> .data
  }

  if(bucket_type_kmeans){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_k{n_buckets}"))

    .data %>%
      dplyr::pull(!!col1) %>%
      as.matrix() %>%
      ClusterR::KMeans_rcpp(clusters = n_buckets, num_init = 10, ...) -> kmns

    .data %>%
      dplyr::mutate(!!col_nm := rank(kmns[["centroids"]])[kmns[["clusters"]]], .after = !!col1)  -> .data

  }

  if(bucket_type_xgboost){

    if(length(multi_cols) == 1){
    set.seed(seed)
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_x{n_buckets}"))
    rlang::ensym(target) -> target1
    rlang::as_name(target1) -> outcome1
    rlang::new_formula(target1, rlang::sym(".")) -> myform

    xgb_rec <- recipes::recipe(myform, data = .data) %>%
      step_discretize_xgb(!!col1, outcome = outcome1, num_breaks = n_buckets, ...)

    xgb_rec <- recipes::prep(xgb_rec, training = .data)

    bake(xgb_rec, new_data = NULL) -> new_data

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
        step_discretize_xgb(!!multi_cols, outcome = outcome1, num_breaks = n_buckets, ...)

      xgb_rec <- recipes::prep(xgb_rec, training = .data)

      bake(xgb_rec, new_data = NULL) -> new_data

      new_data %>%
        dplyr::relocate(!!multi_cols) %>%
        dplyr::mutate(dplyr::across(!!multi_cols, as.integer)) %>%
        dplyr::rename_with(.fn = ~stringr::str_c(., "_x" ,n_buckets), .cols = !!multi_cols) %>%
        dplyr::bind_cols(.data %>% dplyr::select(!!multi_cols)) %>%
        dplyr::relocate(!!multi_cols) -> .data
    }

  }

  if(length(multi_cols) == 1){
  .data %>%
    dplyr::mutate(!!col_nm := as.integer(!!col_nm)) -> .data}

  .data

}


