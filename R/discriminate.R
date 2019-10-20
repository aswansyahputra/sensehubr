#' Discrimination test
#'
#' Perform statistical test for sensory discrimination method.
#'
#' @param n_correct the number of correct answer
#' @param n_panelist the total number of panelist
#' @param method the discimination test protocol. One of 2-AFC, 3-AFC, Duo-Trio, Triangle, Tetrad, Hexad, and Two-out-of-Five.
#' @param conf_level confidence level for the test
#'
#' @importFrom rlang arg_match
#' @importFrom sensR discrim
#' @importFrom stats binom.test
#' @importFrom dplyr mutate recode
#' @importFrom tibble as_tibble new_tibble
#' @importFrom janitor clean_names
#'
#' @examples
#' discriminate(n_correct = 30, n_panelist = 80, method = "Triangle")
#' @export
discriminate <- function(n_correct, n_panelist, method, conf_level = 0.95) {
  method <- arg_match(
    method,
    values = c(
      "2-AFC",
      "3-AFC",
      "Duo-Trio",
      "Triangle",
      "Tetrad",
      "Hexad",
      "Two-out-of-Five"
    )
  )

  res_discrim <- discrim(
    correct = n_correct,
    total = n_panelist,
    method = switch(method,
      "2-AFC" = "twoAFC",
      "3-AFC" = "threeAFC",
      "Duo-Trio" = "duotrio",
      "Triangle" = "triangle",
      "Tetrad" = "tetrad",
      "Hexad" = "hexad",
      "Two-out-of-Five" = "twofive"
    ), conf.level = conf_level
  )

  res_binom <- binom.test(
    x = n_correct,
    n = n_panelist,
    alternative = "two.sided",
    conf.level = conf_level
  )
  
  tbl <-
    res_discrim$coefficients %>%
    as_tibble(rownames = "parameter") %>%
    clean_names() %>%
    mutate(
      parameter = recode(parameter,
        pc = "% correct answer",
        pd = "% discriminator"
      ),
      p.value = c(res_binom$p.value, NA, res_discrim$p.value)
    )

  res <- new_tibble(tbl,
    "sensory_method" = method,
    "method_local" = "Exact binomial test",
    nrow = NROW(tbl),
    class = "tbl_sensory_discrim"
  )

  return(res)
}
