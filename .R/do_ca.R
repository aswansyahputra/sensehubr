#' Perform CA on dataframe
#'
#' Pipe friendly CA
#'
#' @param .data a dataframe or tibble.
#' @param product name of column containing product information.
#' @param liking name of column containing hedonic liking (default to NULL).
#'
#' @import dplyr stringr FactoMineR
#' @importFrom magrittr set_rownames
#' @return Returns a list containing information of CA results.
#'
#' @export

do_ca <- function(.data, product, liking = NULL)
{
  if (missing(.data)) {
    stop("Data is not supplied", call. = FALSE)
  }
  else if (missing(product)) {
    stop("Arguments are not completely supplied", call. = FALSE)
  }
  df <-
    .data %>%
    select(product, everything()) %>%
    as.data.frame() %>%
    magrittr::set_rownames(.[[1]]) %>%
    select_if(is.numeric)
  if (is.null(liking)) {
    index_liking <- NULL
  }
  else {
    index_liking <- which(names(df) %in% liking)
  }
  res <- CA(df, quanti.sup = index_liking, graph = FALSE)
  return(res)
}
