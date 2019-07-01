#' Global analysis of sensory data
#'
#' Perform global analysis on sensory table.
#' @param data a sensory table
#'
#' @importFrom dplyr select
#' @importFrom SensoMineR paneliperf
#'
#' @examples
#' sensochoc %>%
#'   specify(
#'     sensory_method = "QDA",
#'     panelist = panelist,
#'     product = product,
#'     session = session,
#'     pres_order = rank,
#'     attribute = cocoa_a:granular
#'   ) %>%
#'   analyse_performance()
#' @export
analyse_performance <- function(data) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_session <- parse_meta(data, "session")
  meta_pres_order <- parse_meta(data, "pres_order")
  meta_attribute <- parse_meta(data, "attribute")

  if (!is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla1 <- "~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order"
      fmla2 <- "~ product + session + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla1 <- "~ product + panelist + session + panelist:product + panelist:session + product:session"
      fmla2 <- "~ product + session"
    }
  } else if (is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla1 <- "~ product + panelist + pres_order"
      fmla2 <- "~ product + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla1 <- "~ product + panelist"
      fmla2 <- "~ product"
    }
  }

  tbl <- select(data,
    panelist = meta_panelist,
    product = meta_product,
    session = meta_session,
    pres_order = meta_pres_order,
    meta_attribute
  )

  res_performance <- paneliperf(as.data.frame(tbl),
    formul = fmla1,
    formul.j = fmla2,
    col.j = 1,
    firstvar = min(which(names(tbl) %in% meta_attribute)),
    lastvar = max(which(names(tbl) %in% meta_attribute)),
    synthesis = TRUE,
    random = TRUE
  )

  tbl_panel <- inspect_panel(res_performance)
  tbl_panelist_dicrimination <- inspect_panelist(res_performance, metric = "discrimination")
  tbl_panelist_agreement <- inspect_panelist(res_performance, metric = "agreement")
  tbl_panelist_consistency <- inspect_panelist(res_performance, metric = "consistency")

  res <- list(
    panel = tbl_panel,
    panelist_discrimination = tbl_panelist_dicrimination,
    panelist_agreement = tbl_panelist_agreement,
    panelist_consistency = tbl_panelist_consistency,
    res_performance = res_performance
  )

  attr(res, "method_local") <- "Analysis of variance"
  attr(res, "panel_model") <- fmla1
  attr(res, "panelist_model") <- fmla2

  class(res) <- append(class(res), "tbl_sensory_performance")
  return(res)
}
