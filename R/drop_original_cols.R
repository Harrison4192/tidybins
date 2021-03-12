drop_original_cols <- function(.data, restore_names = F){

  .data %>% names %>% stringr::str_subset("_[a-z][a-z][0-9]*$") -> new_cols
  new_cols %>% stringr::str_remove("_[a-z][a-z][0-9]*$")  -> org_col

  .data %>%
    dplyr::select(-tidyselect::any_of(org_col)) -> .data

  if(restore_names){
    .data %>%
      dplyr::rename_with(.fn = ~stringr::str_remove(., "_[a-z][a-z][0-9]*$"), .cols = tidyselect::any_of(new_cols)) -> .data
  }

  .data
}
