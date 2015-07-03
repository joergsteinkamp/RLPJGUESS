# A series of values (len) is transformed to vectors
# with origin (0,0) and a cumulative angle of wgt.
# As plot this would result in a windrose like picture.
# These vectors are appended one after the other, leading
# to a vector as result in direction of the months with
# the highest precipitation. The sum of len divided by
# the length of the vector sum is the seasonality.
#
# Usage is not limited to rainfall seasonality.
# wgt can be defined as anything you like, which 
# can be arranged circular and appended afterwards.
#
# This calculation is based on
#
# Charles G. Markhan (1970) Seasonality of precipitation
# in the United States, Annals of the Association of 
# American Geographers, 60:3, 593-597,
# DOI: 10.1111/j.1467-8306.1970.tb00743.x

setClass("rf_seasonality_v",
         representation(angle="numeric",
                        vx="numeric", vy="numeric",
                        seasonality="numeric"),
         prototype=list(angle=numeric(),
             vx=numeric(), vy=numeric(),
             seasonality=numeric()))
setClass("rf_seasonality_a",
         representation(angle="numeric",
                        vx="array", vy="array",
                        seasonality="array"),
         prototype=list(angle=numeric(),
             vx=array(), vy=array(),
             seasonality=array()))

rainfall_seasonality <- function(
    len=c(10,  8,  6,  5,  4,  3,  1,  1,  2,  5,  7,  9),
    wgt=c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
    debug=FALSE) {
  len.dim <- dim(len)
  if (!is.vector(wgt)) {
      warning("wgt is not a vector")
      return
  }
  if (length(len.dim) <= 1) {
    if (length(wgt) != length(len)) {
      warning(paste("Input vector length differ(len: ", length(len), "; wgt: ", length(wgt), sep=""))
      return
    }
  } else {
    if (len.dim[length(len.dim)] != length(wgt)) {
      warning(paste("Length of wgt and last dimension of len differ (len: ", len.dim[length(len.dim)], "; wgt: ", length(wgt), sep=""))
      return
    }
  }

  if (is.vector(len)) {
    res <- new("rf_seasonality_v")
  } else {
    res <- new("rf_seasonality_a")
    res@vx = array(NA, len.dim)
    res@vy = array(NA, len.dim)
    res@seasonality=array(NA, len.dim[1:(length(len.dim)-1)])
  }

  # calculate the wheighted angles based on wgt
  cwgt <- cumsum(wgt) - wgt/2
  cwgt <- append(cwgt, cwgt[length(cwgt)] + wgt[length(cwgt)]/2)
  angle <- 2 * pi * cwgt / cwgt[length(cwgt)]
  res@angle = angle

  # calculate the x and y component of each vector
  if (is.vector(len)) {
    vx <- sin(angle[1:(length(angle)-1)]) * len
    vy <- cos(angle[1:(length(angle)-1)]) * len
    # plot(cumsum(vx),cumsum(vy), type="l")
    # lines(c(0,cumsum(vx)[12]), c(0,cumsum(vy)[12]), col="red")
  } else {
    angle <- array(rep(angle[1:(length(angle)-1)], each=prod(len.dim[1:(length(len.dim)-1)])), c(len.dim))
    vx <- sin(angle) * len
    vy <- cos(angle) * len
  }
  res@vx = vx
  res@vy = vy

  # calculate the "seasonality" by summing up the x and y components
  # of the vectors and divivion by the total precipitation
  if (is.vector(len)) {
    res@seasonality = (sqrt(cumsum(vx)[12]^2 + cumsum(vy)[12]^2)) / sum(len)
  } else {
    cs.vx <- apply(vx, 1:(length(len.dim)-1), cumsum)
    cs.vy <- apply(vy, 1:(length(len.dim)-1), cumsum)
    s.len <- apply(len, 1:(length(len.dim)-1), sum)

    dim.str <- paste(len.dim[length(len.dim)])
    for (i in 1:(length(len.dim)-1)) {
      dim.str <- paste(dim.str, ",")
    }
    res@seasonality = as.array(eval(parse(text=paste("(sqrt(cs.vx[", dim.str, "]^2 + cs.vy[", dim.str, "]^2)) / s.len"))))

  }
  if (debug) {
    return(res)
  } else {
    return(res@seasonality)
  }
}
