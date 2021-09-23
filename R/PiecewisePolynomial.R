#' @include Generics.R
#' @include Polynomial.R

#' @title An S4 Class to Represent a Piecewise Polynomial
#' 
#' @details
#' Use \code{length(object)} to get the number of piecewise ranges.
#' Use \code{predict(object, newdata)} to evaluate at \code{newdata}. Alternatively, use \code{as.function(x)} to turn the piecewise polynomial into a function.
#' Use \code{differentiate(x)} to take the derivative of each polynomial.
#' Use \code{plot(x)} to \code{lines{x}} to plot the piecewise polynomial.
#' 
#' @slot leftBound A vector that stores the left bounds of the piecewise ranges.
#' @slot rightBound A vector that stores the right bounds of the piecewise ranges.
#' @slot polynomial A list that stores the polynomials at each piecewise ranges.
piecewisePolynomial <- setClass("piecewisePolynomial",
    slots = c(
        leftBound = "vector",
        rightBound = "vector",
        polynomial = "list"
    )
)

setValidity("piecewisePolynomial", function(object) {
    if (length(object@leftBound) != length(object@rightBound)) {
        "Numbers of leftBound and rightBound are mismatched"
    } else if (length(object@leftBound) != length(object@polynomial)) {
        "Numbers of ranges and polynomials are mismatched"
    } else {
        for (i in seq_len(length(object@leftBound))) {
            if (object@leftBound[i] >= object@rightBound[i]) {
                return("Left bound must be less than right bound")
            }
            else if (object@leftBound[i] < object@leftBound && object@leftBound < object@rightBound[i]) {
                return("Ranges cannot overlap")
            }
        }
        return(TRUE)
    }
})

setMethod("length", "piecewisePolynomial", function(x) length(x@leftBound))

setMethod("predict", signature(object="piecewisePolynomial"),
    function(object,newdata) {
        if (class(newdata) == "numeric") {
            y <- rep(NA, length(newdata))
            for(i in seq_len(length(object))) {
                indices <- which(object@leftBound[i] <= newdata & newdata <= object@rightBound[i])
                y[indices] <- predict(object@polynomial[[i]],newdata[indices])
            }
            return(y)
        } else if (class(newdata) == "dual") {
            y <- dual(0, degree = degree(newdata), length=length(newdata))
            for(i in seq_len(length(object))) {
                I <- which(object@leftBound[i] <= newdata & newdata <= object@rightBound[i])
                y[I] <- predict(object@polynomial[[i]],newdata[I])
            }
            return(y)
        } else {
            stop("Unsupprted newdata class in predict where object is piecewisePolynomial")
        }
    }
)

setMethod("as.function", "piecewisePolynomial", function(x) function(xx) predict(x,xx))

setMethod("differentiate", "piecewisePolynomial", function(x) piecewisePolynomial(x@leftBound, x@rightBound, lapply(x@polynomial, differentiate)))

setMethod("plot", "piecewisePolynomial",
    function(x, interval=seq(min(x@leftBound),max(x@rightBound),0.05),type="l", ...) {
        plot(interval, predict(x, interval), type=type, ...)
    }
)

setMethod("lines", "piecewisePolynomial",
    function(x, interval=seq(min(x@leftBound),max(x@rightBound),0.05), ...) {
        lines(interval, predict(x, interval), ...)
    }
)

setMethod("initialize", "piecewisePolynomial",
    function(.Object, leftBound = numeric(0), rightBound = numeric(0), polynomial = list()) {
        .Object@leftBound <- leftBound
        .Object@rightBound <- rightBound
        .Object@polynomial <- polynomial

        validObject(.Object)

        for (i in seq_len(length(leftBound))) {
            if (is.finite(leftBound[i])) {
                Shift(.Object@polynomial[[i]]) <- leftBound[i]
            } else if (is.finite(rightBound[i])) {
                Shift(.Object@polynomial[[i]]) <- rightBound[i]
            }
        }

        return(.Object)
    }
)

defaultRangeFormatter <- function(min, max, xlab="x", digits = getOption("digits")) paste("(",xlab,">",signif(min,digits)," & ",xlab,"<",signif(max,digits),")", sep = "")

setMethod("as.character", "piecewisePolynomial",
    function(x, xlab="x", rangeFormatter = defaultRangeFormatter, digits = getOption("digits")) {
        eq <- ""
        for (i in seq_len(length(x))) {
            eq <- paste(eq,ifelse(eq=="",""," + "),rangeFormatter(x@leftBound[i],x@rightBound[i],xlab=xlab,digits=digits),"*(",as.character(x@polynomial[[i]],xlab=xlab,digits=digits),")", sep = "")
        }
        return(ifelse(eq=="","0",eq))
    }
)

setMethod("degree", "piecewisePolynomial", function(x) max(unlist(lapply(x@polynomial, degree), use.names=FALSE)))

setMethod("as.data.frame", "piecewisePolynomial", function(x, xlab="x", rangeFormatter = defaultRangeFormatter, digits = getOption("digits"), ...) {
    interval <- NULL
    equation <- NULL
    for (i in seq_len(length(x))) {
        interval <- append(interval, rangeFormatter(x@leftBound[i],x@rightBound[i],xlab=xlab, digits = digits))
        equation <- append(equation, as.character(x@polynomial[[i]],xlab=xlab,digits = digits))
    }
    return(data.frame(interval, equation, ...))
})

setMethod("show", "piecewisePolynomial",
    function(object) {
        print(noquote(as.character(object)))
    }
)

setMethod("as.piecewisePolynomial", "polynomial", function(object, leftBound, rightBound) {
    if (missing(leftBound)) leftBound <- -Inf
    if (missing(rightBound)) rightBound <- Inf
    return(piecewisePolynomial(leftBound, rightBound, list(object)))
})

setMethod("as.piecewisePolynomial", "numeric", function(object, leftBound, rightBound) {
    if (missing(leftBound)) leftBound <- -Inf
    if (missing(rightBound)) rightBound <- Inf
    piecewisePolynomial(leftBound, rightBound, list(polynomial(c(object))))
})

setMethod("as.piecewisePolynomial", "piecewisePolynomial", function(object, leftBound, rightBound) {
    if (missing(leftBound)) leftBound <- min(object@leftBound)
    if (missing(rightBound)) rightBound <- max(object@rightBound)
    leftBounds <- c()
    rightBounds <- c()
    polynomial <- list()
    
    for (i in seq_len(length(object))) {
        if (object@leftBound[i] < rightBound & object@rightBound[i] > leftBound) {
            leftBounds <- c(leftBounds, max(object@leftBound[i], leftBound))
            rightBounds <- c(rightBounds, min(object@rightBound[i], rightBound))
            polynomial <- c(polynomial, object@polynomial[[i]])
        }
    }

    return(piecewisePolynomial(leftBounds, rightBounds, polynomial))
})

#' Finding the x-values of Extrema of a Piecewise Polynomial
#' 
#' @param poly A `piecewisePolynomial` type.
#' @param tol Tolerance.
#' @return A vector containing the x-values of the extrema.
piecewisePolynomial.extrema.x <- function(poly, tol = sqrt(.Machine$double.eps)) {
    point.x(piecewisePolynomial.extrema(poly, tol))
}

#' Finding the y-values of Extrema of a Piecewise Polynomial
#' 
#' @param poly A `piecewisePolynomial` type.
#' @param tol Tolerance.
#' @return A vector containing the y-values of the extrema.
polynomial.extrema.y <- function(poly, tol = sqrt(.Machine$double.eps)) {
    point.y(piecewisePolynomial.extrema(poly, tol))
}

#' Finding the Extrema of a Piecewise Polynomial
#' 
#' The ranges in this Piecewise Polynomial must be ordered.
#' The boundary points are also includede in the result.
#' 
#' @param poly A `piecewisePolynomial` type.
#' @param tol Tolerance.
#' @return A `pointData` type containing the extrema points.
piecewisePolynomial.extrema <- function(poly, tol = sqrt(.Machine$double.eps)) {
    x <- poly@leftBound[1]
    y <- predict(poly@polynomial[[1]], poly@leftBound[1])

    for (i in seq_len(length(poly))) {
        extrema <- polynomial.extrema(poly@polynomial[[i]])
        I <- ((point.x(extrema) > poly@leftBound[i] + tol) & (point.x(extrema) < poly@rightBound[i] - tol))
        x <- c(x, point.x(extrema)[I])
        y <- c(y, point.y(extrema)[I])

        if (i < length(poly)) {
            x1 <- poly@rightBound[i]
            x2 <- poly@leftBound[i+1]
            y1 <- predict(poly@polynomial[[i]], poly@rightBound[i])
            y2 <- predict(poly@polynomial[[i+1]], poly@leftBound[i+1])
            if ((x2 - x1 < tol) & (abs(y2 - y1) < tol)) {
                x <- c(x, x1)
                y <- c(y, y1)
            } else {
                x <- c(x, x1, x2)
                y <- c(y, y1, y2)
            }
        } else {
            x <- c(x, poly@rightBound[i])
            y <- c(y, predict(poly@polynomial[[i]], poly@rightBound[i]))
        }
    }

    return(pointData(x, y))
}