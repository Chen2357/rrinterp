#' @include Generics.R

#' @title An S4 Class to Represent a Polynomial
#' 
#' @details
#' Use \code{coef(object)} to access the coefficients.
#' Use \code{degree(object)} to access the degree of the polynomial.
#' Use \code{predict(object, newdata)} to evaluate the polynomial at \code{newdata} (polynomial is allowed). Alternatively, use \code{as.function(x)} to turn the polynomial into a function.
#' Use \code{differentiate(x)} to take the derivative of the polynomial.
#' 
#' @slot coef A vector of coefficients from the lowest degree term to the highest degree term. For example, \code{coef[1]} correspond to the constant term in the polynomial.
polynomial <- setClass("polynomial",
    slots = c(
        coef = "vector"
    )
)

setValidity("polynomial", function(object) {
    for (i in seq_len(length(object))) {
        if (!is.numeric(object@coef[i]) || is.na(object@coef[i])) {
            return(paste("Polynomial coefficients must be numeric, found", paste(object@coef, collapse = ", ")))
        }
    }
    return(TRUE)
})

setMethod("coef", "polynomial", function(object) object@coef)
setMethod("coef<-", "polynomial", function(object,value) {
    object@coef <- value
    validObject(object)
    object
})

setMethod("degree", "polynomial", function(x) length(x@coef)-1)
setMethod("degree<-", "polynomial", function(x,value) {
    length(x@coef) <- value + 1
    validObject(x)
    x
})

setMethod("length", "polynomial", function(x) length(x@coef))
setMethod("length<-", "polynomial", function(x,value) {
    length(x@coef) <- value
    validObject(x)
    x
})

setMethod("initialize", "polynomial",
    function(.Object, coef, degree) {
        if (missing(coef)) {
            .Object@coef <- rep(0,ifelse(missing(degree), 1, degree+1))
        } else if (missing(degree)) {
            .Object@coef <- coef
        } else {
            .Object@coef <- c(coef, rep(0, degree + 1 - length(coef)))
        }

        validObject(.Object)
        return(.Object)
    }
)

setMethod("differentiate", "polynomial", function(x) polynomial(coef(x)[-1] * 1:degree(x)))

setMethod("predict", signature(object="polynomial"),
    function(object, newdata) {
        if (class(newdata) == "numeric") {
            result <- rep(0, length(newdata))
            for (i in seq_len(length(object))) {
                result <- result + object@coef[i] * newdata ^ (i-1)
            }
            return(result)
        } else if (class(newdata) == "polynomial") {
            result <- polynomial(c(0))
            p <- 1
            for (i in seq_len(length(object))) {
                result <- result + object@coef[i] * p
                p <- p * newdata
            }
            return(result)
        } else if (class(newdata) == "dual") {
            result <- dual(0, degree = degree(newdata), length=length(newdata))
            p <- 1
            for (i in seq_len(length(object))) {
                result <- result + object@coef[i] * p
                p <- p * newdata
            }
            return(result)
        } else {
            stop("Unsupprted newdata class in predict where object is polynomial")
        }
    }
)

setMethod("as.function", "polynomial", function(x) function(xx) predict(x,xx))

setMethod("as.character", "polynomial",
    function(x, xlab="x", digits = getOption("digits")) {
        eq <- ""
        for (i in seq_len(length(x))) {
            if (x@coef[i] != 0) {
                if (i==1) {
                    eq <- paste(eq, signif(x@coef[i], digits), sep = "")
                }
                else {
                    eq <- paste(eq,ifelse(eq!=""," + ", ""),signif(x@coef[i], digits),"*",xlab,ifelse(i==2,"",paste("^",i-1,sep="")), sep = "")
                }
            }
        }
        return(ifelse(eq=="","0",eq))
    }
)

setMethod("show", "polynomial",
    function(object) {
        print(noquote(as.character(object)))
    }
)

#' Finding the x-values of Extrema of a Polynomial
#' 
#' @param poly A `polynomial` type.
#' @param tol Tolerance.
#' @return A vector containing the x-values of the extrema.
polynomial.extrema.x <- function(poly, tol = sqrt(.Machine$double.eps)) {
    r <- polyroot(coef(differentiate(poly)))
    r <- Re(r[abs(Im(r)) < tol])
    i <- 1
    while(i <= length(r)) {
        I <- (abs(r - r[i]) < tol)
        if (sum(I) %% 2 == 1) {
            r <- r[!I | (seq_len(length(r)) == i)]
            i <- i + 1
        } else {
            r <- r[!I]
        }
    }
    r <- sort(r)
    return(r)
}

#' Finding the y-values of Extrema of a Polynomial
#' 
#' @param poly A `polynomial` type.
#' @param tol Tolerance.
#' @return A vector containing the y-values of the extrema.
polynomial.extrema.y <- function(poly, tol = sqrt(.Machine$double.eps)) {
    predict(poly, polynomial.extrema.x(poly, tol))
}

#' Finding the Extrema of a Polynomial
#' 
#' @param poly A `polynomial` type.
#' @param tol Tolerance.
#' @return A `pointData` type containing the extrema points.
polynomial.extrema <- function(poly, tol = sqrt(.Machine$double.eps)) {
    x <- polynomial.extrema.x(poly, tol)
    y <- predict(poly, x)
    return(pointData(x, y))
}