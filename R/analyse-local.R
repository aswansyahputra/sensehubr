#' Local analysis of sensory data
#'
#' Perform local analysis on sensory table.
#'
#' @param data a sensory table
#' @param ... other arguments to pass on specific method
#'
#' @examples
#' (df <- specify(
#'   data = perfume_qda_consumers,
#'   sensory_method = "QDA",
#'   panelist = consumer,
#'   product = product,
#'   attribute = intensity:green,
#'   hedonic = NULL
#' ))
#' analyse_local(df)
#'
#' # Using pipe %>%
#' perfume_qda_experts %>%
#'   specify(
#'     sensory_method = "QDA",
#'     panelist = panelist,
#'     product = product,
#'     session = session,
#'     pres_order = rank,
#'     attribute = spicy:wrapping
#'   ) %>%
#'   analyse_local()
#' @export
analyse_local <- function(data, ...) {
  UseMethod("analyse_local")
}

#' @export
analyse_local.default <- function(data, ...) {
  stop("`data` should be a sensory table.", call. = FALSE)
}

#' @importFrom dplyr select group_by mutate summarise arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map
#' @importFrom broom tidy
#' @importFrom tibble new_tibble
#'
#' @export
analyse_local.tbl_sensory_qda <- function(data, ...) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_session <- parse_meta(data, "session")
  meta_pres_order <- parse_meta(data, "pres_order")
  meta_attribute <- parse_meta(data, "attribute")

  if (!is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + session + panelist:product + panelist:session + product:session"
    }
  } else if (is.null(meta_session)) {
    if (!is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist + pres_order"
    } else if (is.null(meta_pres_order)) {
      fmla <- "value ~ product + panelist"
    }
  }

  tbl <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      session = meta_session,
      pres_order = meta_pres_order,
      meta_attribute
    ) %>%
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(
      model = map(
        data,
        ~ aov(as.formula(fmla),
          data = .x
        )
      ),
      stats = map(
        model,
        ~ anova(.x) %>%
          tidy() %>%
          filter(term == "product") %>%
          select(statistic, p.value)
      ),
      values = map(
        data,
        ~ group_by(.x, product) %>%
          summarise(value = mean(value, na.rm = TRUE)) %>%
          spread(product, value)
      )
    ) %>%
    select(attribute, stats, values) %>%
    unnest(stats, values) %>%
    arrange(desc(statistic))

  res <- new_tibble(tbl,
    "sensory_method" = parse_meta(data, "sensory_method"),
    "method_local" = "Analysis of Variance",
    "model" = fmla,
    nrow = NROW(tbl),
    class = "tbl_sensory_local"
  )

  return(res)
}

#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map map_dbl
#' @importFrom RVAideMemoire cochran.qtest
#' @importFrom tibble enframe new_tibble
#'
#' @export
analyse_local.tbl_sensory_cata <- function(data, ...) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_attribute <- parse_meta(data, "attribute")

  fmla <- "value ~ product | panelist"

  tbl <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      meta_attribute
    ) %>%
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(
      model = map(
        data, ~ cochran.qtest(as.formula(fmla), data = .x)
      ),
      statistic = map_dbl(model, "statistic"),
      p.value = map_dbl(model, "p.value"),
      values = map(model, ~ `[[`(.x, "estimate") %>%
        enframe(name = "product", value = "values") %>%
        mutate(product = str_remove_all(product, "proba in group ")))
    ) %>%
    unnest(values) %>%
    spread(product, values) %>%
    arrange(desc(statistic))

  res <- new_tibble(tbl,
    "sensory_method" = parse_meta(data, "sensory_method"),
    "method_local" = "Cochran's Q test",
    "model" = fmla,
    nrow = NROW(tbl),
    class = "tbl_sensory_local"
  )

  return(res)
}
#' @importFrom dplyr select group_by mutate arrange
#' @importFrom tidyr gather spread nest unnest
#' @importFrom purrr map map_dbl
#' @importFrom RVAideMemoire cochran.qtest
#' @importFrom tibble enframe new_tibble
#'
#' @export

analyse_local.tbl_sensory_rata <- function(data, ...) {
  meta_panelist <- parse_meta(data, "panelist")
  meta_product <- parse_meta(data, "product")
  meta_attribute <- parse_meta(data, "attribute")

  fmla <- "value ~ product | panelist"

  tbl <- data %>%
    select(
      panelist = meta_panelist,
      product = meta_product,
      meta_attribute
    ) %>%
    gather("attribute", "value", meta_attribute) %>%
    group_by(attribute) %>%
    nest() %>%
    mutate(
      model = map(
        data, ~ friedman.test(as.formula(fmla), data = .x)
      ),
      statistic = map_dbl(model, "statistic"),
      p.value = map_dbl(model, "p.value"),
      values = map(model, ~ `[[`(.x, "estimate") %>%
        enframe(name = "product", value = "values") %>%
        mutate(product = str_remove_all(product, "proba in group ")))
    ) %>%
    unnest(values) %>%
    spread(product, values) %>%
    arrange(desc(statistic))

  res <- new_tibble(tbl,
    "sensory_method" = parse_meta(data, "sensory_method"),
    "method_local" = "Cochran's Q test",
    "model" = fmla,
    nrow = NROW(tbl),
    class = "tbl_sensory_local"
  )

  return(res)
}
