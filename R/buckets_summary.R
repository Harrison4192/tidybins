buckets_summary <- function(mdb, bucket_col){

  rlang::sym(stringr::str_remove(rlang::as_name(rlang::ensym(bucket_col)), "_[a-z][0-9]*$")) -> original_col

  mdb %>%
    numeric_summary(original_col = !!original_col, bucket_col = {{bucket_col}}) -> mdb

  mdb
 }
