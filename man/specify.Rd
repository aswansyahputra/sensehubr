% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/specify.R
\name{specify}
\alias{specify}
\title{Speficy sensory table}
\usage{
specify(.data, panelist = NULL, product = NULL, pres_order = NULL,
  attribute = NULL, hedonic = NULL, method = c("QDA", "CATA", "RATA",
  "FCP", "FP", "JAR", "IPM"))
}
\arguments{
\item{.data}{a dataframe}

\item{panelist}{panelist column}

\item{product}{product column}

\item{pres_order}{presentation order column}

\item{attribute}{sensory attribute columns}

\item{hedonic}{hedonic column}

\item{method}{sensory method, i.e. QDA, CATA, RATA, FCP, FP, JAR, IPM}
}
\value{
a sensory table (dataframe with class of \code{tbl_sensory})
}
\description{
Specify sensory informations into a raw dataframe. The minimal sensory informations are the panelist, the product, the sensory attributes, and the method in which the evaluation was conducted. Additonally the presentation order and hedonic evaluation can also be specified.
}
\examples{
data(perfume_qda_consumer)
(df <- specify(.data = perfume_qda_consumers, 
  panelist = consumer, 
  product = product, 
  attribute = intensity:green, 
  hedonic = NULL,
  method = "QDA"))
}