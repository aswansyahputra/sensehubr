#' Perform JAR analysis
#'
#' @param .data a dataframe
#' @param panelist a column containing information of panelist
#' @param attribute set of columns containing information of  sensory attributes
#' @param jar_value JAR value (numeric)
#' @param liking a column containing information of liking
#'
#' @import dplyr
#' @importFrom broom tidy
#'
#' @return a tibble
#' @export

do_jar <- function(.data, panelist, attribute, jar_value, liking) {
  panelist_quo <- enquo(panelist)
  attribute_quos <- enquos(attribute)
  liking_quo <- enquo(liking)

  .data %>%
    mutate_at(vars(!!!attribute_quos),
              ~case_when(
                .x < jar_value ~ "Low",
                .x > jar_value ~ "High",
                TRUE ~ "JAR"
              )
    ) %>%
    gather(key = "attribute", value = "class", -!!panelist_quo, -!!liking_quo) %>%
    mutate(class = factor(class, levels = c("JAR", "High", "Low"))) %>%
    group_by(attribute) %>%
    nest(.key = "data") %>%
    mutate(
      model = map(data, ~ aov(liking ~ class, data = .)),
      anova = map(model, ~broom::tidy(summary.lm(.))),
      anova = map(anova, ~filter(.x, term != '(Intercept)')),
      count = map(data, ~count(.x, class)),
      prop = map(count, ~mutate(.x, frequency = 100*n/sum(n))),
      prop = map(prop, ~filter(.x, class != "JAR"))
    ) %>%
    unnest(anova, prop) %>%
    mutate(penalty = abs(estimate)) %>%
    select(
      attribute, class, penalty, everything(), -estimate, -term
    )
}
