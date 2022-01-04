devtools::load_all(export_all = FALSE)

x <- runif(10, 0, 1)
y <- 10 * x^2

interp1 <- rrinterpolate(x, y, 0, 10)
interp2 <- rrinterpolate(x, y, 0, 20)
interp3 <- rrinterpolate(x, y, -20, 20)

xrange <- c(-0.5, 1.5)
yrange <- c(-10, 20)
res <- 1000

xinterval <- seq(xrange[1], xrange[2], (xrange[2] - xrange[1])/res)
y1 <- predict(interp1, xinterval)
y2 <- predict(interp2, xinterval)
y3 <- predict(interp3, xinterval)

plot(xrange, yrange, type = "n", xlab = "x", ylab = "y")
points(x, y, col = "red")
lines(xinterval, y1, type = "l", col = "cyan")
lines(xinterval, y2, type = "l", col = "blue")
lines(xinterval, y3, type = "l", col = "purple")
xlegend <- c(xrange[1], xrange[1]+(xrange[2]-xrange[1])/2.1)
ylegend <- c(yrange[2], yrange[2]-(yrange[2]-yrange[1])/4.5)
legend(xlegend, ylegend, legend = c("Bounded by [0,10]", "Bounded by [0,20]", "Bounded by [-20,20]"), y.intersp = 2, col = c("cyan", "blue", "purple"), lty=rep(1, 3), cex = 1.2)

print(x)