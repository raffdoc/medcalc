# mean and sd for latex documents.
m.sd <- function (x, na.rm = TRUE) {
  if (na.rm) 
    x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) 
    return(c(mean = NA, sd = NA))
  xbar <- sum(x)/n
  sd <- sqrt(sum((x - xbar)^2)/(n - 1))
  c(mean = xbar, sem = sd)
  return(paste(format(xbar),"$\\pm$",format(sd)))
}
