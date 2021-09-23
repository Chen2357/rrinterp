#' @include Patching.R

setMethod("+", signature(e1 = "patching", e2 = "numeric"), function(e1, e2) {
    e1@func <- lapply(e1@func, function(x) x + e2)
    return(e1)
})
setMethod("+", signature(e1 = "numeric", e2 = "patching"), function(e1, e2) {
    e2@func <- lapply(e2@func, function(x) e1 + x)
    return(e2)
})

setMethod("-", signature(e1 = "patching", e2 = "numeric"), function(e1, e2) {
    e1@func <- lapply(e1@func, function(x) x - e2)
    return(e1)
})
setMethod("-", signature(e1 = "numeric", e2 = "patching"), function(e1, e2) {
    e2@func <- lapply(e2@func, function(x) e1 - x)
    return(e2)
})
setMethod("-", signature(e1 = "patching"), function(e1, e2) {
    e1@func <- lapply(e1@func, function(x) -x)
    return(e2)
})

setMethod("*", signature(e1 = "patching", e2 = "numeric"), function(e1, e2) {
    e1@func <- lapply(e1@func, function(x) x * e2)
    return(e1)
})
setMethod("*", signature(e1 = "numeric", e2 = "patching"), function(e1, e2) {
    e2@func <- lapply(e2@func, function(x) e1 * x)
    return(e2)
})

setMethod("/", signature(e1 = "patching", e2 = "numeric"), function(e1, e2) {
    e1@func <- lapply(e1@func, function(x) x / e2)
    return(e1)
})