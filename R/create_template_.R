#' Create template for data input
#'
#' Create template for inputting data collected from sensory research
#'
#' @param .data output from \code{generate_design} function
#' @param panelist column containing panelist information
#' @param attribute a numeric value of number or sensory attributes or vector of sensory terms (lexicon)
#'
#' @import dplyr
#'
#' @return Returns a dataframe of sensory design with Panelist, Order, Product, and Sensory Attributes columns
#'
#' @examples
#'
#' design <- generate_design_(n_panelist = 20, product = 5, use_coding = TRUE)
#' design
#'
#' create_template_(design, panelist = Panelis, attribute = c("Sweetness", "Mint", "Green", "Herbal"))
#'
#'
#' @export

create_template_ <- function(.data, panelist, attribute) {
  panelist <- enquo(panelist)
  if (is.numeric(attribute) & length(attribute) == 1) {
    n_attribute <- attribute
    nms <- str_c("Atribut", seq_len(attribute))
  } else if (is.character(attribute)) {
    n_attribute <- length(attribute)
    nms <- attribute
    nms <- str_replace_all(nms, "([:punct:]|[:space:])", "_")
  }

  .data %>%
    gather("Urutan", "Produk", -!!panelist) %>%
    mutate_at(vars(!!panelist),
              ~factor(.x, levels = unique(.x))) %>%
    cbind(
      .,
      matrix(
        rep(NA_integer_, nrow(.) * n_attribute),
        ncol = n_attribute,
        dimnames = list(NULL, nms)
      )
    ) %>%
    as_tibble() %>%
    arrange(!!panelist, Urutan)
}
