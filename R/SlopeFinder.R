#' @include PointData.R
#' @include QuadraticProgramming.R

#' @title Slope Finding using Beta Method (3 points)
#' 
#' @param data A `pointData` type that stores the 3 points. It will be converted into `x` and `y`.
#' @param x A vector of x-coordinates of the 3 points. May be assigned directly.
#' @param y A vector of y-coordinates of the 3 points. May be assigned directly.
#' @param betaSolver A function to solve beta in the form of \code{function(A, B, b)} that returns a list containing beta. Can be `solve.beta` (default) or `solve.beta.cholesky`.
#' @return The slopes at the points of interest.
findSlope.beta.threePoints <- function(data, x = point.x(data), y = point.y(data), betaSolver = solve.beta) {
    delta21 <- x[2] - x[1]
    delta32 <- x[3] - x[2]

    # L_cluster matrix
    # d21^(-2)  0           -d21^(-2)   d21^(-1)    0           0
    # 0         0           d32^(-2)    0           -d32^(-2)   d32^(-1)
    # 0         0           0           0           1           0
    # 0         d21^(-1)    0           -d21^(-1)   0           0
    # 0         0           0           d32^(-1)    0           -d32^(-1)
    # 0         0           0           0           0           1
    L <- matrix(c(
        delta21^(-2),0,0,0,0,0,
        0,0,0,delta21^(-1),0,0,
        -delta21^(-2),delta32^(-2),0,0,0,0,
        delta21^(-1),0,0,-delta21^(-1),delta32^(-1),0,
        0,-delta32^(-2),1,0,0,0,
        0,delta32^(-1),0,0,-delta32^(-1),1),
        nrow = 6, ncol = 6
    )

    L_inv <- limSolve::Solve(L)

    temp <- ifelse(y==0, 0, 1/(4*y))
    A <- t(L_inv) %*% diag(c(0, temp[1], 0, temp[2], 0, temp[3])) %*% L_inv
    
    temp <- ifelse(y==0, 1, 0)
    B <- diag(c(1, temp[1], 1, temp[2], 1, temp[3])) %*% L_inv
    b <- c(y[1], 0, y[2], 0, y[3], 0)

    beta <- betaSolver(A = A, B = B, b = b)
    p <- L_inv %*% beta

    return(p[c(2,4,6)])
}

findSlope.beta.threePoints.restricted <- function(data, x = point.x(data), y = point.y(data), tau, betaSolver = solve.beta) {
    delta21 <- x[2] - x[1]
    delta32 <- x[3] - x[2]

    # L_cluster matrix
    # d21^(-2)  0           -d21^(-2)   d21^(-1)    0           0
    # 0         0           d32^(-2)    0           -d32^(-2)   d32^(-1)
    # 0         0           0           0           1           0
    # 0         d21^(-1)    0           -d21^(-1)   0           0
    # 0         0           0           d32^(-1)    0           -d32^(-1)
    # 0         0           0           0           0           1
    L <- matrix(c(
        delta21^(-2),0,0,0,0,0,
        0,0,0,delta21^(-1),0,0,
        -delta21^(-2),delta32^(-2),0,0,0,0,
        delta21^(-1),0,0,-delta21^(-1),delta32^(-1),0,
        0,-delta32^(-2),1,0,0,0,
        0,delta32^(-1),0,0,-delta32^(-1),1),
        nrow = 6, ncol = 6
    )

    L_inv <- limSolve::Solve(L)

    temp <- ifelse(abs(y)==tau, 0, 1/(2*(tau-abs(y))))
    A <- t(L_inv) %*% diag(c(0, temp[1], 0, temp[2], 0, temp[3])) %*% L_inv
    
    temp <- ifelse(abs(y)==tau, 1, 0)
    B <- diag(c(1, temp[1], 1, temp[2], 1, temp[3])) %*% L_inv
    b <- c(y[1], 0, y[2], 0, y[3], 0)

    beta <- betaSolver(A = A, B = B, b = b)
    p <- L_inv %*% beta

    return(p[c(2,4,6)])
}