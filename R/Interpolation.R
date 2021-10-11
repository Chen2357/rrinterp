#' @include Patching.R

#' @title Percentage Polynomial
#' 
#' The linear function that is 0 when evaluated at \code{min} and 1 when evaluated at \code{max}
#' 
#' @param min The value at which the function is 0.
#' @param max The value at which the function is 1.
#' @return \code{(x-min)/(max-min)}
percentagePolynomial <- function(min,max) {
    (1/(max-min)) * polynomial(c(-min,1))
}

#' Linear Polynomial Given Point and Slope
#' 
#' @param x A `pointData` type. It will be converted into `x` and `y`. Only the first three values will be used.
#' @param x The vector of x-coordinates. May be assigned directly.
#' @param y The vector of y-coordinates. May be assigned directly.
#' @param slope The slope at the point.
#' @return The linear polynomial that goes through the point with the given slope.
linearPolynomial <- function(data, x = point.x(data), y = point.y(data), slope) {
    if (missing(slope)) {
        slope <- (y[2] - y[1]) / (x[2] - x[1])
    }
    return(polynomial(c(y[1] - slope * x[1], slope)))
}

#' Quadratic Function Through Three Points
#' 
#' @param x A `pointData` type. It will be converted into `x` and `y`. Only the first three values will be used.
#' @param x The vector of x-coordinates. May be assigned directly.
#' @param y The vector of y-coordinates. May be assigned directly.
#' @return The quadratic polynomial that goes through \code{(x[1],y[1])}, \code{(x[2],y[2])}, and \code{(x[3],y[3])}.
quadraticPolynomial <- function(data, x = point.x(data), y = point.y(data)) {
    y[1] + ( ((y[2]-y[1])/(x[2]-x[1])) * polynomial(c(-x[1],1)) ) + ( (((y[2]-y[1])/(x[2]-x[1])-(y[3]-y[2])/(x[3]-x[2]))/(x[1]-x[3])) * polynomial(c(-x[1],1)) * polynomial(c(-x[2],1)) )
}

#' Quadratic Function Through Three Points
#' 
#' @param x A `pointData` type. It will be converted into `x` and `y`. Only the first three values will be used.
#' @param x The vector of x-coordinates. May be assigned directly.
#' @param y The vector of y-coordinates. May be assigned directly.
#' @return The quadratic polynomial that goes through \code{(x[1],y[1])}, \code{(x[2],y[2])}, and \code{(x[3],y[3])}.
interpolate.oneJet <- function(data, slope, tau, patch = getOption("defaultPatching", default = patch.fifthDegree), tol = sqrt(.Machine$double.eps)) {
    x0 <- point.x(data)
    k <- slope
    b <- point.y(data)

    p <- polynomial(c(b-k*x0, k))

    mu <- k^2 / (tau - abs(b))
    delta <- (tau - abs(b)) / abs(k)

    if (abs(k) < tol | delta >= 1) {
        result <- patching(
            list(
                polynomial(0),
                p,
                polynomial(0)
            ),
            x0 + c(-1,0,1),
            patch
        )
    } else {
        q <- mu / 4 * polynomial(c(x0^2,-2*x0,1))
        result <- patching(
            list(
                polynomial(0),
                polynomial(ifelse(k > 0, -tau, tau)),
                p + q * ifelse(k > 0, 1, -1),
                p,
                p + q * ifelse(k > 0, -1, 1),
                polynomial(ifelse(k > 0, tau, -tau)),
                polynomial(0)
            ), 
            x0 + delta * c(-2*sqrt(2), -2*sqrt(2),-1, 0, 1, 2*sqrt(2), 2*sqrt(2)) + c(-1,0,0,0,0,0,1),
            patch
        )
    }
    return(result)
}

#' Interpolation by Patching Three Points Segments
#' 
#' @param data A `pointData` type that stores all the points to be interpolated.
#' @param solver Function that returns a polynomial that interpolate 3 points, must be in the form of `function(data)`.
#' @param patch The function used for patching, uses `defaultPatchPolynomial` by default. Must be in the form of `function(a,b,p)`.
#' @return A piecewise polynomial.
interpolate.patch.threePoint <- function(data, solver, patch = patch.fifthDegree) {
    if (length(data) < 3) stop("`data` must have length of at least 3")

    n <- length(data)
    x <- point.x(data)

    func <- list()
    breaks <- x[2:(n-1)]

    for (i in 2:(n-1)) {
        func <- c(func, solver(data=data[(i-1):(i+1)]))
    }

    return(patching(func, breaks, patch))
}

interpolate.patch.threePoint.indexed <- function(data, solver, patch = patch.fifthDegree) {
    if (length(data) < 3) stop("`data` must have length of at least 3")

    n <- length(data)
    x <- point.x(data)

    func <- list()
    breaks <- x[2:(n-1)]

    for (i in 2:(n-1)) {
        func <- c(func, solver(data=data[(i-1):(i+1)], i=i))
    }

    return(patching(func, breaks, patch))
}

#' Interpolation by Patching Functions Generated at Each Point
#' 
#' @param data A `pointData` type that stores all the points to be interpolated.
#' @param slopes A `vector` type that stores all the slopes correspond to each point.
#' @param solver Function that returns a polynomial that is generated at a point with slope. Must be in the form of `function(data, slope)`.
#' @param patch The function used for patching, uses `defaultPatchPolynomial` by default. Must be in the form of `function(a,b,p)`.
#' @return A piecewise polynomial.
#' @export
interpolate.patch.onePointSlope <- function(data, slopes, solver, patch = getOption("defaultPatching", default = patch.fifthDegree)) {
    n <- length(data)
    x <- point.x(data)

    func <- list()

    for (i in seq_len(n)) {
        func <- c(func, solver(data = data[i], slope = slopes[i]))
    }

    return(patching(func, x, patch))
}

#' Interpolation with Boundaries
#' 
#' The algorithm produces a C^2 curve that interpolates the specified points while staying between `min` and `max` boundaries. The resulting function will have absolute second derivative within some constant multiplied by the optimal (the optimal being the theoretical minimal second derivative possible for a C^2 curve interpolating through the points).
#' 
#' @param x A vector of x-values of the points to be interpolated over.
#' @param y A vector of y-values of the points to be interpolated over.
#' @param min Optional lower boundary of the interpolation.
#' @param max Optional upper boundary of the interpolation.
#' @return A `patching` S4 class.
#' 
#' @export
rrinterpolate <- function(x, y, min = NA, max = NA) {
    if (length(x) != length(y)) stop("Incompatible lengths of x and y")
    if (any(y < ifelse(is.na(min), -Inf, min) | y > ifelse(is.na(max), Inf, max))) stop("Some y values are not in the range specified")
    shift <- 0
    scale <- 1

    od <- order(x)
    x <- x[od]
    y <- y[od]

    if (is.na(min) | is.na(max)) {
        if (is.na(min) & is.na(max)) {
            data <- pointData(x, (y - shift)*scale)
            solver <- quadraticPolynomial
            interpolation <- interpolate.patch.threePoint(data, solver)
        } else {
            if (is.na(max)) {
                shift <- min
            } else {
                shift <- max
                scale <- -1
            }
            data <- pointData(x, (y - shift)*scale)
            solver <- function(data, i, n. = length(x)) {
                if (i != 2 & i != n.-1) {
                    result <- quadraticPolynomial(data)
                    ce <- coef(result)
                    if (ce[2] * ce[2] - 4 * ce[1] * ce[3] < 0) return(result)
                }

                slopes <- findSlope.beta.threePoints(data)
                result <- interpolate.patch.onePointSlope(data, slopes, quadratic.point.slope.extrema)
                return(result)
            }
            interpolation <- interpolate.patch.threePoint.indexed(data, solver)
        }
    } else {
        tau <- (max - min) / 2
        shift <- (max + min) / 2

        data <- pointData(x, (y - shift)*scale)

        solver <- function(data, i, tau. = tau, n. = length(x)) {
            if (i != 2 & i != n.-1) {
                result <- quadraticPolynomial(data)
                ce <- coef(result)
                ex <- -ce[2]/(2*ce[3])
                if (ex < point.x(data)[1] | point.x(data)[3] < ex) return(result)
                if (abs(ce[1]-ce[2]*ce[2]/(4*ce[3])) < tau) return(result)
            }

            slopes <- findSlope.beta.threePoints.restricted(data, tau = tau.)
            result <- interpolate.patch.onePointSlope(data, slopes, function(data, slope, tau.. = tau.) interpolate.oneJet(data, slope, tau..))
            
            return(result)
        }
        interpolation <- interpolate.patch.threePoint.indexed(data, solver)
    }

    interpolation <- interpolation / scale + shift
    return(interpolation)
}

#' Quadratic Function Through a Point and with a Given Slope and with Given y-value of Extrema
#' 
#' @param data A `pointData` type containing a signle point. It will be converted to `x` and `y`
#' @param x The x-coordinate of a point. May be assigned directly.
#' @param y The y-coordinate of a point. May be assigned directly.
#' @param slope The slope at point `(x,y)``
#' @param extreme The y-value of the extrema.
#' @return The quadratic that goes through `(x,y)` with slope `k` and tangent to the `y=extrema`.
quadratic.point.slope.extrema <- function(data, x = point.x(data), y = point.y(data), slope, extrema = 0, tol = sqrt(.Machine$double.eps)) {
    if(abs(y-extrema) < tol) return(polynomial(y))
    slope^2/(4*(y-extrema))*polynomial(c(x^2,-2*x,1)) + slope * polynomial(c(-x,1)) + y
}

#' Interpolation with Boundaries Given Prescribed Slopes
#' 
#' 
#' @param x A vector of x-values of the points to be interpolated over.
#' @param y A vector of y-values of the points to be interpolated over.
#' @param k A vector that describes the slopes at each point.
#' @param min Optional lower boundary of the interpolation.
#' @param max Optional upper boundary of the interpolation.
#' @return A `patching` S4 class.
#' 
#' @seealso [rrinterpolate()] for interpolation details.
#' 
#' @export
rrinterpolate.slope <- function(x, y, k, min = NA, max = NA) {
    if (length(x) != length(y)) stop("Incompatible lengths of x and y")
    if (any(y < ifelse(is.na(min), -Inf, min) | y > ifelse(is.na(max), Inf, max))) stop("Some y values are not in the range specified")
    shift <- 0
    scale <- 1

    if (is.na(min) | is.na(max)) {
        if (is.na(min) & is.na(max)) {
            solver <- linearPolynomial
        } else {
            if (is.na(max)) {
                shift <- min
            } else {
                shift <- max
                scale <- -1
            }
            solver <- quadratic.point.slope.extrema
        }
    } else {
        tau <- (max - min) / 2
        shift <- (max + min) / 2
        solver <- function(data, slope) {
            interpolate.oneJet(data, slope, tau)
        }
    }

    od <- order(x)
    data <- pointData(x[od], (y[od] - shift)*scale)

    interpolation <- interpolate.patch.onePointSlope(data, k, solver)
    interpolation <- interpolation / scale + shift
    return(interpolation)
}