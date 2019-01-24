#' Get eigenvalues
#'
#' Function to augment eigenvalues from multivariate analysis
#'
#' @param .res Output from multivariate analyses
#'
#' @import dplyr stringr magrittr
#' @importFrom janitor clean_names
#' @return a tibble
#' @export


get_eig <- function(.res) {
  if (missing(.res)) {
    stop("Data is not supplied", call. = FALSE)
  } else if (!any(class(.res) %in% c("PCA", "CA", "MFA", "MCA"))) {
    stop("Data is not one of PCA, CA, MFA, MCA class", call. = FALSE)
  }
  .res %>%
    magrittr::extract2("eig") %>%
    as_tibble(rownames = "dim") %>%
    janitor::clean_names() %>%
    rename_(
      "var_percent" = "percentage_of_variance",
      "var_cummulative" = "cumulative_percentage_of_variance"
    ) %>%
    mutate(dim = str_replace_all(dim, "comp", "Dim"))
}
