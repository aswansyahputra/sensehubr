#' Generate Design
#'
#' Generate design of presentation order based on Williams design.
#'
#' @param n_panelist number of panelist
#' @param product a numeric value of number or product or vector of product names
#' @param use_coding wheteher to generate random three digit number for labeling
#'
#' @import crossdes dplyr purrr
#' @importFrom magrittr set_colnames
#' @importFrom plyr mapvalues
#'
#' @return Returns a dataframe of sensory design with Panelist column and Order of product presentation columns
#'
#' @examples
#' generate_design_(n_panelist = 10, product = letters[1:14], use_coding = FALSE)
#' generate_design(n_panelist = 10, product = letters[1:14], use_coding = TRUE)
#'
#' design <- generate_design_(n_panelist = 20, product = 5, use_coding = TRUE)
#' design
#'
#'
#' @export

generate_design_ <- function(n_panelist, product, use_coding = TRUE) {
  if (is.numeric(product) & length(product) == 1) {
    n_product <- product
    nms <- str_c("Produk", seq_len(n_product))
  } else if (is.character(product)) {
    n_product <- length(product)
    nms <- product
  }

  if (!isTRUE(use_coding)) {
    nms <- nms
  } else {
    set.seed(n_product + n_panelist)
    cds <- sample(100:999, n_product, replace = FALSE)
    nms <- str_c(nms, " (", cds, ")")
  }

  res <-
    williams(n_product) %>%
    as_tibble() %>%
    map_df(rep, n_panelist) %>%
    set_colnames(str_c("Urutan", seq_len(n_product))) %>%
    mutate_all(~plyr::mapvalues(.x, seq_len(n_product), nms)) %>%
    mutate(Panelis = str_c("Panelis", seq_len(nrow(.)))) %>%
    select(Panelis, everything()) %>%
    slice(seq_len(n_panelist))
  return(res)
}
