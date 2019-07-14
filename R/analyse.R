#' Sensory data analysis
#' 
#' Perform statistical analysis on various sensory methods.
#' 
#' @param tbl_sensory a sensory table
#' @param choice choice of analysis, see details.
#' @param ... other arguments for specific method
#' 
#' @export
analyse <- function(tbl_sensory, choice, ...) {
  UseMethod("analyse")
}

#' @export
analyse.default <- function(tbl_sensory, choice, ...) {
  stop("`tbl_sensory` is not valid or not yet supported.", call. = FALSE)
}

#' @importFrom rlang arg_match
#' @export
analyse.tbl_sensory_qda <- function(tbl_sensory, choice, ...) {
  if (length(choice) > 1 || is.na(choice)) {
    stop("Please pick one choice only.", call. = FALSE)
  }
  
  choice <- arg_match(choice, values = c("local", "global", "liking", "mdpref", "prefmap", "performance"))
  
  if (choice == "local") {
    res <- perform_anova(tbl_sensory, ...)
  }
  
  if (choice == "global") {
    res <- perform_pca(tbl_sensory, ...)
  }
  
  if (choice == "liking") {
    res <- perform_liking_analysis(tbl_sensory, ...)
  }
  
  if (choice == "mdpref") {
    res <- perform_mdpref(tbl_sensory, ...)
  }
  
  if (choice == "prefmap") {
    res <- perform_prefmap(tbl_sensory, ...)
  }
  
  if (choice == "performance") {
    res <- perform_panel_analysis(tbl_sensory, ...)
  }
  
  return(res)
}

#' @importFrom rlang arg_match
#' @export
analyse.tbl_sensory_cata <- function(tbl_sensory, choice, ...) {
  if (length(choice) > 1 || is.na(choice)) {
    stop("Please pick one choice only.", call. = FALSE)
  }
  
  choice <- arg_match(choice, values = c("local", "global", "liking", "mdpref", "prefmap"))
  
  if (choice == "local") {
    res <- perform_cochran(tbl_sensory, ...)
  }
  
  if (choice == "global") {
    res <- perform_ca(tbl_sensory, ...)
  }
  
  if (choice == "liking") {
    res <- perform_liking_analysis(tbl_sensory, ...)
  }
  
  if (choice == "mdpref") {
    res <- perform_mdpref(tbl_sensory, ...)
  }
  
  if (choice == "prefmap") {
    res <- perform_prefmap(tbl_sensory, ...)
  }
  
  return(res)
}

#' @importFrom rlang arg_match
#' @export
analyse.tbl_sensory_rata <- function(tbl_sensory, choice, ...) {
  if (length(choice) > 1 || is.na(choice)) {
    stop("Please pick one choice only.", call. = FALSE)
  }
  
  choice <- arg_match(choice, values = c("local", "global", "liking", "mdpref", "prefmap"))
  
  if (choice == "local") {
    res <- perform_friedman(tbl_sensory, ...)
  }
  
  if (choice == "global") {
    res <- perform_ca(tbl_sensory, ...)
  }
  
  if (choice == "liking") {
    res <- perform_liking_analysis(tbl_sensory, ...)
  }
  
  if (choice == "mdpref") {
    res <- perform_mdpref(tbl_sensory, ...)
  }
  
  if (choice == "prefmap") {
    res <- perform_prefmap(tbl_sensory, ...)
  }
  
  return(res)
}

#' @importFrom rlang arg_match
#' @export
analyse.tbl_sensory_jar <- function(tbl_sensory, choice, ...) {
  if (length(choice) > 1 || is.na(choice)) {
    stop("Please pick one choice only.", call. = FALSE)
  }
  
  choice <- arg_match(choice, values = c("penalty"))
  
  if (choice == "penalty") {
    res <- perform_penalty(tbl_sensory, ...)
  }
  
  return(res)
}
