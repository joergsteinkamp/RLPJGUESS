lpj.full <- function(infile=NULL, lon.extent=c(-180, 360), lat.extent=c(-90, 90), time.extent=c(0, 99999), use.time=FALSE) {
  # read the data
  data <- read.table(infile, header=TRUE)

  # choose the spatial subset
  data <- subset(data, Lat<=max(lat.extent) & Lat>=min(lat.extent) & Lon>=min(lon.extent) & Lon<=max(lon.extent))

  if (use.time) {
    data <- subset(data, Year>=min(time.extent) & Year<=max(time.extent))
  } else {
    data <- subset(data, Year-min(Year)>=min(time.extent) & Year-min(Year)<=max(time.extent))
  }
  data <- subset(data, Year >= min(time.extent) & Year <= max(time.extent))
  return(data)
}

lpj.timeseries <- function(infile=NULL, lon.extent=c(-180, 360), lat.extent=c(-90, 90), area.weighted=FALSE, year.offset=0) {
  zoo.avail <- TRUE
  if (!require(zoo, quietly=TRUE)) {
    warning("Can't load required library 'zoo',\nUsing a simple ts() object instead.")
    zoo.avail = FALSE
  }

  # this is set further down if the 4th column is named "Jan"
  annual <- TRUE

  # read the data
  data <- read.table(infile, header=TRUE)
  if (colnames(data)[4] == "Jan") annual = FALSE

  # choose the spatial subset
  data <- subset(data, Lat<=max(lat.extent) & Lat>=min(lat.extent) & Lon>=min(lon.extent) & Lon<=max(lon.extent))
  data$Year = data$Year + year.offset

  # create the area weight if desired
  data$area = 1.
  if (area.weighted) {
    data$area = NA

    uniq.lon <- sort(unique(data$Lon))
    uniq.lat <- sort(unique(data$Lat), decreasing = TRUE)

    uniq.lon <- seq(min(uniq.lon), max(uniq.lon),
                    min(uniq.lon[2:length(uniq.lon)] - uniq.lon[1:(length(uniq.lon)-1)]))

    uniq.lat <- seq(max(uniq.lat), min(uniq.lat),
                    max(uniq.lat[2:length(uniq.lat)] - uniq.lat[1:(length(uniq.lat)-1)]))

    area1d <- gridarea1d(uniq.lat, abs(uniq.lon[2]-uniq.lon[1]))*1.e-6

    for (i in 1:length(uniq.lat)) 
      data$area[data$Lat == uniq.lat[i]] = area1d[i]
  }

  uniq.year <- sort(unique(data$Year))
  cnames <- colnames(data)

  data.tmp <- NULL
  for (i in uniq.year)
    data.tmp = rbind(data.tmp, data.frame(t(colMeans(data[data$Year == i, ]))))

  # remove the unused columns
  data = data.tmp[, !(cnames=="Lon" | cnames=="Lat" | cnames=="Year" | cnames=="area")]
  rm(data.tmp)

  if (annual) {
    keep <- rep(TRUE, ncol(data))
    # remove columns with unique values
    for (i in 1:ncol(data)) 
      if (min(data[,i]) == max (data[,i])) keep[i] = FALSE
    data <-  data[, keep]
    data.ts <- ts(data, start=min(uniq.year), frequency=1)
  } else {
    data.ts <- ts(as.vector(t(as.matrix(data))), start=min(uniq.year), frequency=12)
  }
  if (zoo.avail)
    data.ts <- zoo(data.ts)

  return(data.ts)
}

lpj.spatial <- function(infile=NULL, lon.extent=c(-180, 360), lat.extent=c(-90, 90), time.extent=c(0, 99999), use.time=FALSE, na.cond=NA, na.mask=FALSE) {
##  # this is set further down if the 4th column is named "Jan"
##  annual <- TRUE
  data <- read.table(infile, header=TRUE)
##  if (colnames(data)[4] == "Jan") annual = FALSE

  # choose the spatial subset
  data <- subset(data, Lat<=max(lat.extent) & Lat>=min(lat.extent) & Lon>=min(lon.extent) & Lon<=max(lon.extent))

  # set values to NA, based on supplied condition
  if (!is.na(na.cond)) {
    for (i in na.cond) {
      if (!grepl("^[!=<>]", i)) {
        warning(paste("wrong format for condition, needs to start with '!', '=', '<' or '>':",i))
        next
      }
      tmp <- data[, 4:ncol(data)]
      eval(parse(text=paste("tmp[tmp",i,"] = NA",sep="")))
      data[, 4:ncol(data)] = tmp
    }
    rm(tmp)
  }
  # create a mask, set valid values to 1 and not finite (infinite and NA) to 0
  if (na.mask) {
    tmp <- data[, 4:ncol(data)]
    tmp[is.finite(as.matrix(tmp))]  = 1
    tmp[!is.finite(as.matrix(tmp))] = 0
    data[, 4:ncol(data)] = tmp
  }

  # choose the time subset
  if (use.time) {
    data <- subset(data, Year>=min(time.extent) & Year<=max(time.extent))
  } else {
    data <- subset(data, Year-min(Year)>=min(time.extent) & Year-min(Year)<=max(time.extent))
  }

  uniq.year <- sort(unique(data$Year))
  cnames <- colnames(data)

  data.tmp = subset(data, Year==uniq.year[1])
  if (length(uniq.year)>1) {
    for (i in 2:(length(uniq.year)))
      data.tmp = data.tmp + subset(data, Year==uniq.year[i])
    data.tmp = data.tmp / length(uniq.year)
  }
  data <- data.tmp[, !cnames=="Year"]

  return(data)
}
