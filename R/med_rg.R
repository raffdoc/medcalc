# median and range for latex documents.
m.rg <- function (x, na.rm = TRUE) {
        if (na.rm) 
                x <- x[!is.na(x)]
        #n <- length(x)
        #if (n == 0) 
        #return(c(median = NA, range = NA);
        #median(x);range(x)
        return(paste0(format(median(x))," ","(","range, ",format(min(x))," ","$-$ ",format(max(x)),")"))
}