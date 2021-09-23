#' @include Polynomial.R

setMethod("+", signature(e1 = "polynomial", e2 = "polynomial"), function(e1, e2) {
    n <- max(degree(e1), degree(e2))
    sum <- polynomial(degree = n)
    sum@coef <- c(coef(e1), rep(0, n-degree(e1))) + c(coef(e2), rep(0, n-degree(e2)))
    return(sum)
})

setMethod("+", signature(e1 = "numeric", e2 = "polynomial"), function(e1, e2) {
    coef(e2)[1] <- e1 + coef(e2)[1]
    return(e2)
})

setMethod("+", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    coef(e1)[1] <- coef(e1)[1] + e2
    return(e1)
})

setMethod("-", signature(e1 = "polynomial", e2 = "polynomial"), function(e1, e2) {
    n <- max(degree(e1), degree(e2))
    result <- polynomial(degree = n)
    result@coef <- c(coef(e1), rep(0, n-degree(e1))) - c(coef(e2), rep(0, n-degree(e2)))
    return(result)
})

setMethod("-", signature(e1 = "numeric", e2 = "polynomial"), function(e1, e2) {
    coef(e2) <- -coef(e2)
    coef(e2)[1] <- e1 + coef(e2)[1]
    return(e2)
})

setMethod("-", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    coef(e1)[1] <- coef(e1)[1] - e2
    return(e1)
})

setMethod("-", signature(e1 = "polynomial"), function(e1, e2) {
    coef(e1) <- -coef(e1)
    return(e1)
})

setMethod("*", signature(e1 = "polynomial", e2 = "polynomial"), function(e1, e2) {
    product <- polynomial(degree = (degree(e1) + degree(e2)))

    for (i in seq_len(length(e1))) {
        for (j in seq_len(length(e2))) {
            product@coef[i+j-1] <- coef(product)[i+j-1] + coef(e1)[i] * coef(e2)[j]
        }
    }
    return(product)
})

setMethod("*", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) return(polynomial(coef(e1) * e2)))

setMethod("*", signature(e1 = "numeric", e2 = "polynomial"), function(e1, e2) return(polynomial(e1 * coef(e2))))

setMethod("/", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) return(polynomial(coef(e1) / e2)))

setMethod("^", signature(e1 = "polynomial", e2 = "numeric"), function(e1, e2) {
    if (e2%%1 != 0) {
        warning("exponent must be an integer, caught ", e2)
        return(NULL)
    }
    result <- polynomial(c(1))

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