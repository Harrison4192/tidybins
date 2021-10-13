#' add_clusters
#'
#' @param .data dataframe
#' @param ... columns to cluster
#' @param n_clusters integer
#' @param cluster_name column name
#' @importFrom framecleaner select_otherwise
#'
#' @return data frame
#' @export
add_clusters <- function(.data, ..., n_clusters = 4, cluster_name = "cluster"){

  .data %>%
    select_otherwise(..., otherwise = where(is.numeric), return_type = "df") -> df1

  ClusterR::KMeans_rcpp(df1, clusters = n_clusters, num_init = 5) -> c_out

  .data %>%
    dplyr::mutate("{cluster_name}" := LETTERS[c_out$clusters]) -> .data1

  .data1

}

