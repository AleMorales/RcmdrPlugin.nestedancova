
#' ANOVA for an aovlist object
#'
#' @param object Model of class \code{aovlist}
#' @param ... Further arguments to \code{stats:::summary.aovlist}
#'
#' @export
anova.aovlist <- function(object, ...) {
  stats:::summary.aovlist(object, ...)
}

#' Residual values for an aovlist object
#'
#' @param object Model of class \code{aovlist}
#' @param ... Further arguments to \code{dae::residuals.aovlist}
#' @export
residuals.aovlist <- function(object, ...) {
  dae::residuals.aovlist(object, ...)
}

#' Fitted values for an aovlist object
#'
#' @param object Model of class \code{aovlist}
#' @param ... Further arguments to \code{dae::fitted.aovlist}
#' @export
fitted.aovlist <- function(object, ...) {
  dae::fitted.aovlist(object, ...)
}

#' Diagnostic plots for an aovlist object
#'
#' @param x Model of class \code{aovlist}
#' @param ... Further arguments to \code{plot}
#' @export
plot.aovlist <- function(x, ...) {
  par(mfrow = c(1,2))
  N = length(x)
  res = residuals(x)
  fit = fitted(x)
  sd_error = sqrt(deviance(x[[N]])/df.residual(x[[N]]))
  print(c(sd_error, sd(res)))
  sres = res/sd_error
  plot(fit, res, xlab = "Fitted values", ylab = "Residuals",
       main = "Residuals vs Fitted", ...)
  abline(h = 0, lty = 2)
  qqnorm(sres, ylab = "Std residuals", main = "Residual Q-Q Norm")
  qqline(sres)
  par(mfrow = c(1,1))
}
