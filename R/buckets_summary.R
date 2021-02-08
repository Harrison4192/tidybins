buckets_summary <- function(mdb){

  mdb %>% names %>% stringr::str_subset("_[wfvkx][0-9]*$") -> cols

  bucket_rgx <- stringr::str_c(cols,  collapse = "|")

  mdb %>% names %>% stringr::str_subset(bucket_rgx) %>% rlang::syms()  -> bucks


  blist <- list()


  for(buck in bucks){

  mdb %>% dplyr::select(!!buck) %>% names() -> bnames

  bnames %>% stringr::str_extract("[wfvkx][0-9]*$")  %>% stringr::str_remove("[0-9]*$") -> suffix

  bnames %>% stringr::str_remove("_[wfvkx][0-9]*$") %>% rlang::sym() -> org_col

    switch(suffix,
           "w" = "equal width",
           "f" = "equal freq",
           "v" = "equal value",
           "k" = "kmeans",
           "x" = "xgboost") -> method

    mdb %>%
      numeric_summary(original_col = !!org_col, bucket_col = !!buck) %>%
      dplyr::mutate(column := rlang::as_name(org_col),
                    method = method, .before = 1) %>%
      dplyr::rename_with(function(x)c("rank", "label"), c(3,4)) -> mdb1

  blist %>% rlist::list.append(mdb1) -> blist
  }


  blist %>% reduce(bind_rows)
}
