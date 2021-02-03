numeric_summary <- function(mdb, original_col, bucket_col){


  mdb %>%
    dplyr::group_by({{bucket_col}}, .add = T) %>%
    dplyr::summarize(count = n(),
                     sum = sum({{original_col}}, na.rm = T),
                     dplyr::across({{original_col}}, five_number_summary),
                     sd = sd({{original_col}}, na.rm = T)) %>%
    dplyr::summarize({{bucket_col}}, count, sum, {{original_col}}, sd) %>%
    dplyr::mutate(relative_value = sum / count, .after = count ) %>%
    dplyr::mutate(relative_value = relative_value / max(relative_value, .na.rm = T) * 100) %>%
    dplyr::arrange(dplyr::desc(max)) %>%
    dplyr::mutate(width = max - min) %>%
    dplyr::mutate("{{bucket_col}}_label" := factor(stringr::str_c("[",scales::number(min, accuracy = .001) , ",", scales::number(max, accuracy = .001), "]")), .after = 1, .keep = "unused")-> mdb

  mdb
}
