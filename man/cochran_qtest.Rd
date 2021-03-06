% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cochran_qtest.R
\name{cochran_qtest}
\alias{cochran_qtest}
\title{[RVAideMemoire] Cochran's Q test}
\usage{
cochran_qtest(formula, data, alpha = 0.05, p.method = "fdr")
}
\arguments{
\item{formula}{a formula of the form a ~ b | c, where a, b and c give the data values and corresponding groups and blocks, respectively. a can be a numeric vector or a factor, with only two possible values.}

\item{data}{an optional data frame containing the variables in the formula formula. By default the variables are taken from environment(formula).}

\item{alpha}{significance level to compute pairwise comparisons.}

\item{p.method}{method for p-values correction. See help of p.adjust.}
}
\description{
[This function is exported from RVAideMemoire package] Performs the Cochran's Q test for unreplicated randomized block design experiments with a binary response variable and paired data. If the p-value of the test is significant, the function performs pairwise comparisons by using the Wilcoxon sign test.
}
\details{
If the response is a 0/1 variable, the probability of the '1' group is tested. In any other cases, the response is transformed into a factor and the probability of the second level is tested.
}
\author{
Maxime Hervé \email{mx.herve@gmail.com}
}
