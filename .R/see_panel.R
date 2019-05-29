#' See performance of whole panel
#'
#' See the performance of panel as a whole
#'
#' @param res output from get_performance
#'
#' @return a dataframe containing p.value from ANOVA model
#'
#' @export

see_panel <- function(res) {
  if (!any(class(res) == "perf_metrics")) {
    stop("`res` must be output of get_performance", call. = FALSE)
  }
  res[["panel_metrics"]]
}
