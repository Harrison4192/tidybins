create_buckets <- function(.data,
                           col,
                           n_buckets,
                           bucket_type = c("equal_frequency",
                                         "equal_width",
                                         "equal_value",
                                         "kmeans",
                                         "xgboost"),
                           ...,
                           target = NULL
){

  col1 <- rlang::ensym(col)
  col_str <- rlang::as_name(col1)


  if(bucket_type == "equal_frequency"){

    col_nm <- rlang::sym(stringr::str_glue("{col_str}_f{n_buckets}"))

    .data %>%
      dplyr::mutate(!!col_nm := ggplot2::cut_number(!!col1, n = n_buckets), .after = !!col1) -> .data
  } else if(bucket_type == "equal_width"){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_w{n_buckets}"))

    .data %>%
      dplyr::mutate(!!col_nm := ggplot2::cut_interval(!!col1, n = n_buckets), .after = !!col1) -> .data
  } else if(bucket_type == "equal_value"){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_v{n_buckets}"))

    .data %>%
      quantiles_create(col = !!col1, quantile_num = n_buckets) -> .data
  }else if(bucket_type == "kmeans"){
    col_nm <- rlang::sym(stringr::str_glue("{col_str}_k{n_buckets}"))

    .data %>%
      dplyr::pull(!!col1) %>%
      as.matrix() %>%
      ClusterR::KMeans_rcpp(clusters = n_buckets, num_init = 10, ...) -> kmns

    .data %>%
      dplyr::mutate(!!col_nm := rank(kmns[["centroids"]])[kmns[["clusters"]]])  -> .data

  } else if(bucket_type == "xgboost"){
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
      dplyr::bind_cols(t2 %>% dplyr::select(!!col1)) %>%
      dplyr::relocate(!!col1, !!col_nm) -> .data

  }

  .data %>%
    mutate(!!col_nm := as.integer(!!col_nm))

}


