buckets_summary <- function(mdb, col){

  mdb %>%
    dplyr::select({{col}}) %>% names -> cols

  bucket_rgx <- stringr::str_c(cols, "_[a-z][0-9]*$", collapse = "|")

  mdb %>% names %>% stringr::str_subset(bucket_rgx) %>% rlang::syms()  -> bucks


  blist <- list()


  for(buck in bucks){

  mdb %>% dplyr::select(!!buck) %>% names() -> bnames

  bnames %>% stringr::str_extract("[a-z][0-9]*$")  %>% stringr::str_remove("[0-9]*$") -> suffix

  bnames %>% stringr::str_remove("_[a-z][0-9]*$") %>% rlang::sym() -> org_col

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

# else{
#
#
#   mdb %>%
#     numeric_summary(original_col = {{col}}, bucket_col = !!bucks[[1]]) -> df_output
#
# }
