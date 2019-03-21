#' Get summary for global analysis
#'
#' @param .res output of PCA or CA
#' @param choice choices: "product" or "attribute"
#' @param axes dimension of interest
#'
#' @import dplyr
#' @importFrom tibble as_tibble
#' @importFrom factoextra facto_summarize
#'
#' @return a tibble
#' @export

get_global_summary <- function(.res, choice, axes = c(1, 2)) {
  element <- if (choice == "product") {
    switch(class(.res)[[1]],
           "CA" = "row",
           "PCA" = "ind")
  } else if (choice == "attribute") {
    switch(class(.res)[[1]],
           "CA" = "col",
           "PCA" = "var")
  }
  factoextra::facto_summarize(.res, element = element, axes = axes) %>%
    as_tibble() %>%
    rename(
      Komponen = name,
      `Kosinus Kuadarat` = cos2,
      Kontribusi = contrib
    ) %>%
    select(-coord)
}
