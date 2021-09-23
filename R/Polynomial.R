#' @include Generics.R

polynomial <- setClass("polynomial",
    slots = c(
        coef = "vector",
        shift = "numeric"
    )
)

setValidity("polynomial", function(object) {
    if (length(object@coef) == 0) return("Polynomial coef cannot have length of 0")

    for (i in seq_len(length(object@coef))) {
        if (!is.numeric(object@coef[i]) || is.na(object@coef[i])) {
            return(paste("Polynomial coefficients must be numeric, found", paste(object@coef, collapse = ", ")))
        }
    }

    if (!is.numeric(object@shift) || is.na(object@shift)) {
        return(paste("Polynomial shift must be numeric, found", object@shift))
    }
    return(TRUE)
})

setMethod("coef", "polynomial", function(object) Coef(object, 0))
# setMethod("coef<-", "polynomial", function(object,value) {
#     object@coef <- value
#     validObject(object)
#     return(object)
# })

setMethod("shift", "polynomial", function(x) x@shift)
setMethod("shift<-", "polynomial", function(x,value) {
    x@shift <- shift
    validObject(x)
    return(x)
})

setMethod("degree", "polynomial", function(x) length(x@coef)-1)
setMethod("degree<-", "polynomial", function(x,value) {
    length(x@coef) <- value + 1
    validObject(x)
    return(x)
})

setMethod("[", "polynomial", function(x,i,...) x@coef[i+1])
setMethod("[<-", "polynomial", function(x,i,...,value) {
    x@coef[i+1] <- value
    validObject(x)
    return(x)
})

setMethod("Coef", "polynomial", function(x, shift) {
    if (missing(shift)) shift <- x@shift
    n <- degree(x) + 1
    result <- rep(0, n)
    p <- x
    f <- 1
    for(i in 1:n) {
        result[i] <- predict(p, shift) / f
        p <- differentiate(p)
        f <- f * i
    }
    return(result)
})

setMethod("Shift<-", "polynomial", function(x, value) {
    polynomial(Coef(x, value), shift = value)
})

setMethod("initialize", "polynomial",
    function(.Object, coef, shift, degree) {
        if (missing(shift)) shift <- 0
        .Object@shift <- shift

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

setMethod("differentiate", "polynomial", function(x) {
    if (degree(x) > 0) {
        x@coef <- x@coef[-1] * seq_len(degree(x))
    } else {
        x@coef <- 0
    }
    return(x)
})

setMethod("predict", signature(object="polynomial"),
    function(object, newdata) {
        if (class(newdata) == "numeric") {
            result <- rep(0, length(newdata))
        } else if (class(newdata) == "polynomial") {
            result <- polynomial(degree = degree(object) * degree(newdata))
        } else if (class(newdata) == "dual") {
            result <- dual(degree = degree(newdata), length = length(newdata))
        } else {
            stop("Unsupprted newdata class in predict where object is polynomial")
        }

        newdata <- newdata - object@shift
        p <- 1
        for (i in 0:degree(object)) {
            result <- result + object[i] * p
            p <- p * newdata
        }
        return(result)
    }
)

setMethod("as.function", "polynomial", function(x) function(xx) predict(x,xx))

setMethod("as.character", "polynomial",
    function(x, xlab="x", digits = getOption("digits")) {
        eq <- ""
        for (i in 0:degree(x)) {
            if (x[i] != 0) {
                if (i==0) {
                    eq <- paste(eq, signif(x[i], digits), sep = "")
                }
                else {
                    eq <- paste(eq,ifelse(eq!=""," + ", ""),signif(x[i], digits),"*",ifelse(x@shift == 0, xlab, paste("(",xlab,"-",x@shift,")",sep = "")),ifelse(i==1,"",paste("^",i,sep="")), sep = "")
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

# #' @title An S4 Class to Represent a Polynomial
# #' 
# #' @details
# #' Use \code{coef(object)} to access the coefficients.
# #' Use \code{degree(object)} to access the degree of the polynomial.
# #' Use \code{predict(object, newdata)} to evaluate the polynomial at \code{newdata} (polynomial is allowed). Alternatively, use \code{as.function(x)} to turn the polynomial into a function.
# #' Use \code{differentiate(x)} to take the derivative of the polynomial.
# #' 
# #' @slot coef A vector of coefficients from the lowest degree term to the highest degree term. For example, \code{coef[1]} correspond to the constant term in the polynomial.
# polynomial <- setClass("polynomial",
#     slots = c(
#         coef = "vector"
#     )
# )

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
    r <- sort(r) - poly@shift
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