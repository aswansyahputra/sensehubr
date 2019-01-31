#' Get Performance of Sensory Panel and Panelist
#'
#' Perform calculation to get performance metrics
#'
#' @param .data a dataframe
#' @param panelist column contaning panelist information
#' @param product column contaning product information
#' @param session column containing session information
#' @param pres_order column contaning presentation order information
#' @param attribute vector of columns of sensory attributes
#'
#' @import dplyr SensoMineR
#' @importFrom magrittr set_colnames
#' @importFrom glue glue
#'
#' @return a named list of four dataframe
#'
#' @examples
#' \dontrun{
#' library(SensoMineR)
#' data(chocolates)
#' attrs <- colnames(sensochoc)[-c(1:4)]
#' out <-
#'   get_performance(sensochoc, panelist = Panelist, product = Product, session = Session, pres_order = Rank, attribute = attrs)
#' out
#' }
#'
#' @export

get_performance <- function(.data,
           panelist = NULL,
           product = NULL,
           session = NULL,
           pres_order = NULL,
           attribute = NULL) {
  panelist <- enquo(panelist)
  product <- enquo(product)
  session <- enquo(session)
  pres_order <- enquo(pres_order)
  attribute_quo <- enquo(attribute)

  df <-
    .data %>%
    mutate_at(
      vars(!!panelist, !!product, !!session, !!pres_order),
      ~ as.factor(.x)
    ) %>%
    select(
      !!panelist,
      !!product,
      !!session,
      !!pres_order,
      !!attribute_quo
    ) %>%
    as.data.frame()
  frml_model1 <- glue::glue(
      "~{product}+{panelist}+{session}+{pres_order}+{product}:{panelist}+{product}:{session}+{panelist}:{session}")[[2]]
  frml_model2 <- glue::glue("~{product}")[[2]]
  out <- paneliperf(
    df,
    formul = frml_model1,
    formul.j = frml_model2,
    col.j = 1,
    firstvar = min(which(names(df) %in% attribute)),
    lastvar = max(which(names(df) %in% attribute)),
    synthesis = TRUE,
    random = TRUE
  )

  # res <- list(
  #   "panel_metrics" = as_tibble(res[["p.value"]]) %>%
  #     magrittr::set_colnames(str_squish(colnames(.))) %>%
  #     arrange(!!product),
  #   "panelist_discrimination" = as_tibble(res[["prob.ind"]], rownames = "Panelist") %>% ,
  #   "panelist_consensus" = as_tibble(res[["agree.ind"]], rownames = "Panelist"),
  #   "panelist_consistency" = as_tibble(res[["res.ind"]], rownames = "Panelist")
  # )
  res <- list(
    "panel_metrics" = out %>%
      magrittr::extract2("p.value") %>%
      as_tibble(rownames = "Attribute") %>%
      magrittr::set_colnames(str_squish(colnames(.))) %>%
      arrange(!!product),
    "panelist_discrimination" = out %>%
      magrittr::extract2("prob.ind") %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "Panelist") %>%
      select(-median) %>%
      filter(Panelist != "median"),
    "panelist_consensus" = out %>%
      magrittr::extract2("agree.ind") %>%
      magicsort(method = "median", ascending = FALSE) %>%
      as_tibble(rownames = "Panelist") %>%
      select(-median) %>%
      select(Panelist, everything()) %>%
      filter(Panelist != "median"),
    "panelist_consistency" = out %>%
      magrittr::extract2("res.ind") %>%
      magicsort(method = "median") %>%
      as_tibble(rownames = "Panelist") %>%
      select(-median) %>%
      filter(Panelist != "median")
  )

  class(res) <- append("perf_metrics", class(res))
  invisible(res)
}
