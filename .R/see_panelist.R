#' See performance of panelist
#'
#' See the performance of each panelist in a panel
#'
#' @param res output from get_performance
#' @param ability character vector specifying panelist ability of interest, the choices are "discrimination", "consensus", and "consistency"
#'
#' @return a dataframe containing p-values of ANOVA model II (discrimination), or correlation values between Product coefficent of ANOVA model I and model II (consensus), or RMSE (consistency)
#'
#' @export

see_panelist <- function(res, ability = c("discrimination", "consensus","consistency")) {
  if (!any(class(res) == "perf_metrics")) {
    stop("`res` must be output of get_performance", call. = FALSE)
  }
  res[[paste0("panelist_", ability[[1]])]]
}
