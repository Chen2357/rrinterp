#' @include Polynomial.R

setMethod("+", signature(e1 = "polynomial", e2 = "polynomial"), function(e1, e2) {
    n <- max(degree(e1), degree(e2))
    if (abs(e1@shift) < abs(e2@shift)) {
        shift <- e1@shift
        c1 <- c(e1@coef, rep(0, n-degree(e1)))
        c2 <- c(Coef(e2, shift), rep(0, n-degree(e2)))
    } else {
        shift <- e2@shift
        c1 <- c(Coef(e1, shift), rep(0, n-degree(e1)))
        c2 <- c(e2@coef, rep(0, n-degree(e2)))
    }
    return(polynomial(c1 + c2, shift = shift))
})

setMethod("+", signature(e1 = "numeric", e2 = "polynomial"), function(e1, e2) {
    e2[0] <- e1 + e2[0]
    return(e2)
})

setMethod("+", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    e1[0] <- e1[0] + e2
    return(e1)
})

setMethod("-", signature(e1 = "polynomial", e2 = "polynomial"), function(e1, e2) {
    n <- max(degree(e1), degree(e2))
    if (abs(e1@shift) < abs(e2@shift)) {
        shift <- e1@shift
        c1 <- c(e1@coef, rep(0, n-degree(e1)))
        c2 <- c(Coef(e2, shift), rep(0, n-degree(e2)))
    } else {
        shift <- e2@shift
        c1 <- c(Coef(e1, shift), rep(0, n-degree(e1)))
        c2 <- c(e2@coef, rep(0, n-degree(e2)))
    }
    return(polynomial(c1 - c2, shift = shift))
})

setMethod("-", signature(e1 = "numeric", e2 = "polynomial"), function(e1, e2) {
    e2@coef <- -e2@coef
    e2[0] <- e1 + e2[0]
    return(e2)
})

setMethod("-", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    e1[0] <- e1[0] - e2
    return(e1)
})

setMethod("-", signature(e1 = "polynomial"), function(e1, e2) {
    e1@coef <- -e1@coef
    return(e1)
})

setMethod("*", signature(e1 = "polynomial", e2 = "polynomial"), function(e1, e2) {
    if (abs(e1@shift) < abs(e2@shift)) {
        shift <- e1@shift
        Shift(e2) <- shift
    } else {
        shift <- e2@shift
        Shift(e1) <- shift
    }

    product <- polynomial(shift = shift, degree = (degree(e1) + degree(e2)))

    for (i in 0:degree(e1)) {
        for (j in 0:degree(e2)) {
            product[i+j] <- product[i+j] + e1[i] * e2[j]
        }
    }
    return(product)
})

setMethod("*", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    e1@coef <- e1@coef * e2
    return(e1)
})

setMethod("*", signature(e1 = "numeric", e2 = "polynomial"), function(e1, e2) {
    e2@coef <- e1 * e2@coef
    return(e2)
})

setMethod("/", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    e1@coef <- e1@coef / e2
    return(e1)
})

setMethod("^", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    if (e2%%1 != 0) stop("exponent must be an integer, caught ", e2)

    result <- polynomial(1, shift = e1@shift)

    while (e2 > 0) {
        if (e2 %% 2 == 1) {
            result <- result * e1
            e2 <- (e2-1)/2
        } else {
            e2 <- e2/2
        }
        e1 <- e1 * e1
    }

    return(result)
})