# ANCHOR Data Point
setGeneric("point.x", function(object) standardGeneric("point.x"))
setGeneric("point.x<-", function(object,value) standardGeneric("point.x<-"))

setGeneric("point.y", function(object) standardGeneric("point.y"))
setGeneric("point.y<-", function(object,value) standardGeneric("point.y<-"))

# ANCHOR Polynomial
setGeneric("coef", function(object) standardGeneric("coef"))
setGeneric("coef<-", function(object,value) standardGeneric("coef<-"))

setGeneric("degree", function(x) standardGeneric("degree"))
setGeneric("degree<-", function(x,value) standardGeneric("degree<-"))

# ANCHOR Piecewise functions
setGeneric("as.piecewisePolynomial", function(object, ...) standardGeneric("as.piecewisePolynomial"))
setGeneric("%+%", function(e1, e2) standardGeneric("%+%"))

# ANCHOR Differentiation
setGeneric("differentiate", function(x) standardGeneric("differentiate"))