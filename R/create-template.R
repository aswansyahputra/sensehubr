#' Create template for sensory table
#'
#' Create template for inputting data that collected from sensory research.
#'
#' @param data output from \code{prepare} function
#' @param attribute a numeric value of number or sensory attributes or vector of sensory terms (lexicon)
#' @param hedonic whether to include hedonic rating or not
#'
#' @importFrom dplyr arrange mutate
#' @importFrom stringr str_pad str_replace_all str_remove_all
#' @importFrom stats setNames
#' @importFrom tidyr extract
#'
#' @return Returns a dataframe of sensory design with panelist, presentation order, product, blind code (optional) and sensory attributes columns
#'
#' @export
#'
#' @examples
#' design <- prepare(n_panelist = 20, product = 5, blind_code = TRUE)
#' design
#'
#' create_template(design, attribute = c("Sweetness", "Mint", "Green", "Herbal"))
create_template <- function(data, attribute, hedonic = FALSE) {
  if (!any(class(data) == "tbl_sensory_design")) {
    stop("`data` should be a sensory table of design.", call. = FALSE)
  }

  if (is.numeric(attribute) & length(attribute) == 1) {
    nms <- paste0("attr_", str_pad(seq_len(attribute), width = 2, pad = "0"))
  } else if (is.character(attribute)) {
    nms <- str_replace_all(attribute, "([:punct:]|[:space:])", "_")
  }

  empty_attribute <- rep(NA_character_, length(nms)) %>%
    setNames(nms)

  tbl <- data %>%
    gather("pres_order", "product", -panelist) %>%
    arrange(panelist) %>%
    mutate(!!!empty_attribute)

  if (attr(data, "blind_code") == "TRUE") {
    tbl <- tbl %>%
      extract("product", "blind_code", regex = "(\\d{3})", remove = FALSE) %>%
      mutate(product = str_remove_all(product, "\\s\\(\\d{3}\\)$"))
  }
  
  if (isTRUE(hedonic)) {
    tbl <- tbl %>% 
      mutate(liking = NA)
  }

  res <- new_tibble(tbl,
    "panelist" = "panelist",
    "n_panelist" = parse_meta(data, "n_panelist"),
    "product" = "product",
    "n_product" = parse_meta(data, "n_product"),
    "attribute" = nms,
    "n_attribute" = length(empty_attribute),
    nrow = NROW(tbl),
    class = "tbl_sensory_template"
  )
  return(res)
}
