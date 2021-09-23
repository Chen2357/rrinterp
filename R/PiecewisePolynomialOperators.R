#' @include PiecewisePolynomial.R

combinePiecewisePolynomial <- function(FUN, e1, e2, tol = sqrt(.Machine$double.eps)) {
    I1 <- sort.list(e1@leftBound)
    I2 <- sort.list(e2@leftBound)
    j1 <- 1
    j2 <- 1

    marker <- NULL
    resetMarker <- TRUE

    leftBound <- c()
    rightBound <- c()
    polynomial <- list()

    while((j1 <= length(I1)) & (j2 <= length(I2))) {
        i1 <- I1[j1]
        i2 <- I2[j2]

        if (resetMarker) {
            marker <- max(min(e1@leftBound[i1], e2@leftBound[i2]), marker)
            isMarker1 <- (e1@leftBound[i1] < e2@leftBound[i2])
            resetMarker <- FALSE
        }

        if (isMarker1) {
            leftBound2 <- e2@leftBound[i2]
            rightBound1 <- e1@rightBound[i1]
            rightBound2 <- e2@rightBound[i2]
            poly1 <- e1@polynomial[[i1]]
            poly2 <- e2@polynomial[[i2]]
        } else {
            leftBound2 <- e1@leftBound[i1]
            rightBound1 <- e2@rightBound[i2]
            rightBound2 <- e1@rightBound[i1]
            poly1 <- e2@polynomial[[i2]]
            poly2 <- e1@polynomial[[i1]]
        }
        
        if (rightBound1 < leftBound2 + tol) {
            leftBound <- c(leftBound, marker)
            rightBound <- c(rightBound, rightBound1)
            polynomial <- c(polynomial, poly1)

            if (isMarker1) j1 <- j1 + 1
            else j2 <- j2 + 1
            resetMarker <- TRUE
        } else {
            if (isMarker1) {
                result <- FUN(poly1, poly2)
            } else {
                result <- FUN(poly2, poly1)
            }

            if (abs(rightBound1 - rightBound2) < tol) {
                j1 <- j1 + 1
                j2 <- j2 + 1
                newMarker <- rightBound1
                resetMarker <- TRUE
            } else if (rightBound1 < rightBound2) {
                if (isMarker1) j1 <- j1 + 1
                else j2 <- j2 + 1
                newMarker <- rightBound1
                isMarker1 <- !isMarker1
            } else {
                if (!isMarker1) j1 <- j1 + 1
                else j2 <- j2 + 1
                newMarker <- rightBound2
            }

            if (abs(marker - leftBound2) > tol) {
                leftBound <- c(leftBound, marker, leftBound2)
                rightBound <- c(rightBound, leftBound2, newMarker)
                polynomial <- c(polynomial, poly1, result)
            } else {
                leftBound <- c(leftBound, leftBound2)
                rightBound <- c(rightBound, newMarker)
                polynomial <- c(polynomial, result)
            }

            marker <- newMarker
        }
    }

    if (!resetMarker) {
        leftBound <- c(leftBound, marker)
        if (isMarker1) {
            rightBound <- c(rightBound, e1@rightBound[I1[j1]])
            polynomial <- c(polynomial, e1@polynomial[I1[j1]])
            j1 <- j1 + 1
        } else {
            rightBound <- c(rightBound, e2@rightBound[I2[j2]])
            polynomial <- c(polynomial, e2@polynomial[I2[j2]])
            j2 <- j2 + 1
        }
    }

    if (j1 <= length(I1)) {
        I <- I1[j1:length(I1)]
        leftBound <- c(leftBound, e1@leftBound[I])
        rightBound <- c(rightBound, e1@rightBound[I])
        polynomial <- c(polynomial, e1@polynomial[I])
    } else if (j2 <= length(I2)) {
        I <- I2[j2:length(I2)]
        leftBound <- c(leftBound, e2@leftBound[I])
        rightBound <- c(rightBound, e2@rightBound[I])
        polynomial <- c(polynomial, e2@polynomial[I])
    }

    return(piecewisePolynomial(leftBound, rightBound, polynomial))
}

setMethod("+", signature(e1 = "piecewisePolynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    combinePiecewisePolynomial(`+`, e1, e2)
})
setMethod("+", signature(e1 = "piecewisePolynomial", e2 = "polynomial"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x + e2)
    return(e1)
})
setMethod("+", signature(e1 = "piecewisePolynomial", e2 = "numeric"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x + e2)
    return(e1)
})
setMethod("+", signature(e1 = "polynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) e1 + x)
    return(e2)
})
setMethod("+", signature(e1 = "numeric", e2 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) e1 + x)
    return(e2)
})

setMethod("*", signature(e1 = "piecewisePolynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    combinePiecewisePolynomial(`*`, e1, e2)
})
setMethod("*", signature(e1 = "piecewisePolynomial", e2 = "polynomial"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x * e2)
    return(e1)
})
setMethod("*", signature(e1 = "piecewisePolynomial", e2 = "numeric"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x * e2)
    return(e1)
})
setMethod("*", signature(e1 = "polynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) e1 * x)
    return(e2)
})
setMethod("*", signature(e1 = "numeric", e2 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) e1 * x)
    return(e2)
})

setMethod("-", signature(e1 = "piecewisePolynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    combinePiecewisePolynomial(`-`, e1, e2)
})
setMethod("-", signature(e1 = "piecewisePolynomial", e2 = "polynomial"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x - e2)
    return(e1)
})
setMethod("-", signature(e1 = "piecewisePolynomial", e2 = "numeric"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x - e2)
    return(e1)
})
setMethod("-", signature(e1 = "polynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) e1 - x)
    return(e2)
})
setMethod("-", signature(e1 = "numeric", e2 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) e1 - x)
    return(e2)
})
setMethod("-", signature(e1 = "piecewisePolynomial"), function(e1, e2) {
    e2@polynomial <- lapply(e2@polynomial, function(x) -x)
    return(e1)
})

setMethod("/", signature(e1 = "piecewisePolynomial", e2 = "numeric"), function(e1, e2) {
    e1@polynomial <- lapply(e1@polynomial, function(x) x / e2)
    return(e1)
})

setMethod("%+%", signature(e1 = "piecewisePolynomial", e2 = "piecewisePolynomial"), function(e1, e2) {
    result <- piecewisePolynomial(c(e1@leftBound, e2@leftBound), c(e1@rightBound, e2@rightBound), c(e1@polynomial, e2@polynomial))
    validObject(result)
    return(result)
})