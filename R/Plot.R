#' @include Patching.R

#' @title Plot Interpolation
#' 
#' Perform plotting of function of the `patching` S4 class. It will plot the function along with its first and second derivatives.
#' 
#' @param interpolation A `patching` class.
#' @param interval A vector of x-values to be plotted.
#' @param x.point A vector of x-values of the points to be plotted.
#' @param y.point A vector of y-values of the points to be plotted.
#' @param limits A vector y-values of the horizontal lines to be plotted.
#' @param res The number of points are used to calculate the function when `interval` is not specified.
#' @param autodiff Whther to use automatic differentiation to calculate the derivatives. When the `patching` class can be collapsed into a polynomial, this parameter can be set to `FALSE` to improve algorithm efficiency. When this parameter is missing, it determines automatically whether the algorithm can be collapsed into apolynomial.
#' 
#' @details Packages "reshape2" and "ggplot2" are required for this action.
#' 
#' @export
rrplot <- function(interpolation, interval, x.point, y.point, limits, res = 1000, autodiff) {
    if (!requireNamespace("reshape2", quietly = TRUE)) stop("Package \"reshape2\" needed for this function to work. Please install it.", call. = FALSE)
    if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
    if (missing(interval)) {
        if (!missing(x.point)) {
            padding <- 0.5
            interval <- seq(min(x.point)-padding, max(x.point)+padding, (max(x.point)-min(x.point)+padding*2)/res)
        } else {
            stop("Unable to infer the interval to plot")
        }
    }

    if (!missing(x.point)) {
        if(!missing(y.point)) {
            data <- pointData(x, y)
        } else {
            data <- pointData(x, predict(interpolation, x))
        }
    }

    if (missing(autodiff)) {
        f <- as.piecewisePolynomial(interpolation)
        if (is.null(f)) {
            usedual <- TRUE
        } else {
            interpolation <- f
            usedual <- FALSE
        }
    }

    if (autodiff) {
        int <- dual(c(interval, rep(1, length(interval))), degree=2, length=length(interval), bydegree=TRUE)
        out <- predict(interpolation, int)
        df <- data.frame(
            x = interval,
            y = out[[,0]],
            y_prime = out[[,1]],
            y_prime2 = 2 * out[[,2]]
        )
    } else {
        d <- differentiate(interpolation)
        df <- data.frame(
            x = interval,
            y = predict(interpolation, interval),
            y_prime = predict(d, interval),
            y_prime2 = predict(differentiate(d), interval)
        )
    }
    df.melt <- reshape2::melt(df, id = "x")
    p <- ggplot2::ggplot(df.melt, ggplot2::aes(x = x, y = value)) + 
        ggplot2::geom_line(ggplot2::aes(color = variable)) + 
        ggplot2::facet_grid(rows = variable ~ ., scales = "free_y")
    if (!missing(data)) {
        p <- p + ggplot2::geom_point(data = cbind(as.data.frame(data), variable="y"), 
             mapping = ggplot2::aes(x = x, y = y), 
             size = 1)
    }
    if (!missing(limits)) {
        p <- p + ggplot2::geom_hline(
            data = cbind(data.frame(limits), variable="y"),
            ggplot2::aes(yintercept = limits),
            color = "blue",
            linetype = "dashed"
        )
    }
    plot(p)
}

#' @export
rrinterpolate.plot <- function(x, y, min, max, res = 1000, usedual) {
    if (!requireNamespace("reshape2", quietly = TRUE)) stop("Package \"reshape2\" needed for this function to work. Please install it.", call. = FALSE)
    if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package \"ggplot2\" needed for this function to work. Please install it.", call. = FALSE)
    interpolation <- rrinterpolate(x, y, min, max)
    interval <- seq(min(x), max(x), (max(x) - min(x)) / res)
    rrplot(interpolation, interval, pointData(x, y), usedual)
}