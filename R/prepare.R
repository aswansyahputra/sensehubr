#' Prepare sensory experiment
#'
#' Generate a design of presentation order based on Williams design.
#'
#' @param n_panelist number of panelist
#' @param product a numeric value of number or product or vector of product names
#' @param blind_code wheteher to generate random three digit number for labeling
#' @param seed an integer for anchoring randomisation
#'
#' @importFrom dplyr rename_all mutate_all mutate recode
#' @importFrom stringr str_pad
#' @importFrom stats setNames
#' @importFrom SensoMineR WilliamsDesign
#' @importFrom tibble as_tibble rownames_to_column
#' @importFrom purrr map_df
#'
#' @return a dataframe of sensory design with panelist column and order of product presentation columns
#' @export
#'
#' @examples
#' prepare(n_panelist = 30, product = letters[1:14], blind_code = FALSE)
#' prepare(n_panelist = 30, product = letters[1:14], blind_code = TRUE)
#'
#' design <- prepare(n_panelist = 20, product = 5, blind_code = TRUE)
#' design
prepare <- function(n_panelist, product, blind_code = FALSE, seed = NULL) {
  if (is.numeric(product) & length(product) == 1) {
    n_product <- product
    nms <- paste0("prod_", str_pad(seq_len(n_product), width = 2, pad = "0"))
  } else if (is.character(product)) {
    n_product <- length(product)
    nms <- product
  }

  if (!isTRUE(blind_code)) {
    nms <- nms
  } else {
    set.seed(seed)
    cds <- sample(100:999, n_product, replace = FALSE)
    nms <- paste0(nms, " (", cds, ")")
  }

  nms <- setNames(nms, seq_len(n_product))

  tbl <- WilliamsDesign(n_product, seed = seed) %>%
    as_tibble() %>%
    rename_all(~ paste0("order_", seq_len(n_product))) %>%
    map_df(rep, length.out = n_panelist) %>%
    mutate_all(~ recode(.x, !!!nms)) %>%
    rownames_to_column("panelist") %>%
    mutate(panelist = paste0("ind_", str_pad(panelist, width = 3, pad = "0")))

  res <- new_tibble(tbl,
    "n_panelist" = n_panelist,
    "n_product" = n_product,
    "blind_code" = blind_code,
    nrow = NROW(tbl),
    class = "tbl_sensory_design"
  )

  return(res)
}
