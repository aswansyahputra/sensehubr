#' [RVAideMemoire] Cochran's Q test
#' 
#' [This function is exported from RVAideMemoire package] Performs the Cochran's Q test for unreplicated randomized block design experiments with a binary response variable and paired data. If the p-value of the test is significant, the function performs pairwise comparisons by using the Wilcoxon sign test.
#' @param formula a formula of the form a ~ b | c, where a, b and c give the data values and corresponding groups and blocks, respectively. a can be a numeric vector or a factor, with only two possible values.
#' @param data an optional data frame containing the variables in the formula formula. By default the variables are taken from environment(formula).
#' @param alpha significance level to compute pairwise comparisons.
#' @param p.method method for p-values correction. See help of p.adjust.
#' 
#' @details If the response is a 0/1 variable, the probability of the '1' group is tested. In any other cases, the response is transformed into a factor and the probability of the second level is tested.
#' 
#' @author Maxime Hervé \email{mx.herve@@gmail.com}
#' @examples 
#' response <- c(0,1,1,0,0,1,0,1,1,1,1,1,0,0,1,1,0,1,0,1,1,0,0,1,0,1,1,0,0,1)
#' fact <- gl(3,1,30,labels=LETTERS[1:3])
#' block <- gl(10,3,labels=letters[1:10])
#' cochran.qtest(response~fact|block)
cochran_qtest <-
  function(formula,
           data,
           alpha = 0.05,
           p.method = "fdr")
  {
    if (missing(formula)) {
      stop("formula missing")
    }
    if ((length(formula) != 3) || (length(formula[[3]]) != 3) ||
        (formula[[3]][[1]] != as.name("|")) ||
        (length(formula[[3]][[2]]) !=
         1) ||
        (length(formula[[3]][[3]]) != 1)) {
      stop("incorrect specification for formula")
    }
    formula[[3]][[1]] <- as.name("+")
    m <- match.call()
    m$formula <- formula
    if (is.matrix(eval(m$data, parent.frame()))) {
      m$data <- as.data.frame(m$data)
    }
    m[[1]] <- as.name("model.frame")
    m$alpha <- m$p.method <- NULL
    mf <- eval(m, parent.frame())
    mf <- droplevels(mf[complete.cases(mf),])
    dname <- paste(names(mf)[1], " by ", names(mf)[2], ", block = ",
                   names(mf)[3], sep = "")
    resp <- mf[, 1]
    fact <- mf[, 2]
    block <- mf[, 3]
    if (length(na.omit(unique(resp))) != 2) {
      stop(paste(names(mf)[1], "is not a binary variable"))
    }
    resp <- as.numeric(factor(resp)) - 1
    proba <- tapply(resp, fact, mean, na.rm = TRUE)
    names(proba) <- paste("proba in group ", levels(fact), sep = "")
    nval <- 0
    names(nval) <- "difference in probabilities"
    tab.length <-
      tapply(resp, list(block, fact), function(x)
        length(na.omit(x)))
    if (any(tab.length != 1) | any(is.na(tab.length))) {
      stop(paste(
        "there must be 1 observation per level of '",
        names(mf)[2],
        "' in each block",
        sep = ""
      ))
    }
    tab <- tapply(resp, list(block, fact), function(x)
      sum(x))
    k <- ncol(tab)
    b <- nrow(tab)
    X.j <- colSums(tab)
    Xi. <- rowSums(tab)
    N <- sum(X.j)
    Q <- k * (k - 1) * sum((X.j - N / k) ^ 2) / sum(Xi. * (k - Xi.))
    names(Q) <- "Q"
    p <- pchisq(Q, k - 1, lower.tail = FALSE)
    names(p) <- NULL
    result <-
      list(
        method.test = "Cochran's Q test",
        data.name = dname,
        statistic = Q,
        parameter = c(df = k - 1),
        alternative = "two.sided",
        null.value = nval,
        p.value = p,
        estimate = proba,
        alpha = alpha
      )
    if (p < alpha & nlevels(fact) > 2) {
      fun.p <- function(i, j) {
        signs <- apply(tab[, c(i, j)], 1, diff)
        if (length(signs[signs != 0]) > 0) {
          binom.test(length(signs[signs > 0]), length(signs[signs !=
                                                              0]), 0.5)$p.value
        }
        else {
          1
        }
      }
      result$method.multcomp <- "Wilcoxon sign test"
      result$p.adjust.method <- p.method
      result$p.value.multcomp <- pairwise.table(fun.p, levels(fact),
                                                p.adjust.method = p.method)
    }
    class(result) <- "RVtest"
    return(result)
  }
