lpj2nc.dim.save <- function(ncout, name, data, time.start=c(1901, 1, 1), time.offset=0, time.unit=c(years=TRUE, months=FALSE, days=FALSE)) {
  if (sum(time.unit) != 1) {
    message(paste("Wrong unit for time dimension:", paste(names(time.unit), time.unit, collapse=", ")))
    return(FALSE)
  }

  if (tolower(name)=="time") {
    dim.def.nc(ncout, name, unlim=TRUE)
    var.def.nc(ncout, name, "NC_INT", name)
    att.put.nc(ncout, name, "calendar", "NC_CHAR", "365_day")
    if (time.unit["years"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "years")
      att.put.nc(ncout, name, "units", "NC_CHAR",
                 paste("years since ", sprintf("%04i-%02i-%02i", time.start[1], time.start[2], time.start[3]), " 00:00:00",sep=""))
      var.put.nc(ncout, name, data+time.offset)
    } else if (time.unit["months"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "months")
      att.put.nc(ncout, name, "units", "NC_CHAR",
                 paste("months since ", sprintf("%04i-%02i-%02i", time.start[1], time.start[2], time.start[3]), " 00:00:00",sep=""))
      var.put.nc(ncout, name, data+time.offset)
    } else if (time.unit["days"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "days")
      att.put.nc(ncout, name, "units", "NC_CHAR",
                 paste("days since ", sprintf("%04i-%02i-%02i", time.start[1], time.start[2], time.start[3]), " 00:00:00",sep=""))
      var.put.nc(ncout, name, data+time.offset)
    } else {
      message(paste("Undefined time.unit: ",  paste(names(time.unit), time.unit, collapse=", ")))
      return(FALSE)
    }
  } else {
    dim.def.nc(ncout, name, dimlength=length(data))
    var.def.nc(ncout, name, "NC_FLOAT", name)
    if (tolower(name)=="lon" || tolower(name)=="long" || tolower(name)=="longitude"){
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "longitude")
      att.put.nc(ncout, name, "units", "NC_CHAR", "degrees_east")
    } else if (tolower(name)=="lat" || tolower(name)=="latitude"){
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "latitude")
      att.put.nc(ncout, name, "units", "NC_CHAR", "degrees_north")
    }
    var.put.nc(ncout, name, data)
  }
}

lpj2nc.var.save <- function(ncout, data, dims, attr) {
  name <- attr[["name"]]
  attr[names(attr)!= "name"]
  var.def.nc(ncout, name, "NC_FLOAT", dims)
  for (n in names(attr)) {
    if (is.integer(attr[[n]]))
      att.put.nc(ncout, name, n, "NC_INT", attr[[n]])
    else if (is.numeric(attr[[n]]))
      att.put.nc(ncout, name, n, "NC_FLOAT", attr[[n]])
    else
      att.put.nc(ncout, name, n, "NC_CHAR", attr[[n]])
  }
  
  if (is.array(data)) {
    var.put.nc(ncout, name, data)
  } else if (is.data.frame(data)) {
    lon <- extract.seq(data$Lon)
    lat <- extract.seq(data$Lat)
    if (any(colnames(data)=="Year"))
      time <- extract.seq(data$Year)

    if (exists("time")) {
      out <- array(NA, c(ncol(data)-3, length(lon), length(lat), length(time)))
      for (i in 4:ncol(data))
        out[i-3,,,] = lpj.df2array(data, colnames(data)[i])
    } else {
      out <- array(NA, c(ncol(data)-3, length(lon), length(lat)))
      for (i in 3:ncol(data))
        out[i-3,,] = lpj.df2array(data, colnames(data)[i])
    }
    var.put.nc(ncout, name, out)
  }
}

lpj2nc <- function(df, file="test.nc", attr=list(name="values"), overwrite=TRUE, as.flux=FALSE, scale=1.0, time.start=NA, time.offset=0, flat=FALSE) {

  ## make sure attr is a list and the netcdf variable name is not empty
  if (!is.list(attr)) {
    message(paste('"attr" is not a list ("', typeof(attr),'" instead)',sep=""))
    return(FALSE)
  }
  if (is.null(attr[['name']])) {
    attr[['name']]="values"
  } else {
    if (is.na(attr[['name']])) {
      attr[['name']]="values"
    } else if (!is.character(attr[['name']])) {
      attr[['name']]="values"
    } else if (attr[['name']] == "") {
      attr[['name']]="values"
    }
  }

  if (colnames(df)[1] != "Lon" && colnames(df)[1] != "Lat" && colnames(df)[1] != "Year") {
    message('Error: First 3 columns must be named "Lon", "Lat", "Year".')
    return(FALSE)
  }

  lon        <- extract.seq(df$Lon)
  lat        <- extract.seq(df$Lat, descending=TRUE)
  time       <- extract.seq(df$Year)
  start.year <- min(time)
  time = time - start.year

  if (any(is.na(time.start)))
    time.start <- c(start.year, 1, 1)
  
  monthly <- FALSE
  if (any(colnames(df)=="Jan"))
    monthly <- TRUE

  if (overwrite) {
    ncout <- create.nc(file, large=TRUE)
  } else {
    if (!(file.access(file, mode=2)+1)) {
      message(paste("Cannot open '", file, "' for appending data", sep=""))
      return(FALSE)
    }
    ncout <- open.nc(file, write=TRUE)
  }

  ## assume correct lat/lon definitions in append mode (!overwrite)
  if (overwrite) {
    lpj2nc.dim.save(ncout, "lon", lon)
    lpj2nc.dim.save(ncout, "lat", lat)
  }
  
  if (monthly) {
    dpm <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    ## TODO: 1.) check if dim is alredy defined if not overwrite
    ##           make time axis definition more flexible.
    ##       2.) check if it needs extention, or if a new variable
    ##           is added.
    ##       so far only new variables with equal dimension definitions
    ##       are acepted.
    if (overwrite)
      lpj2nc.dim.save(ncout, "time", 1:(length(time)*12)-1,  time.start=time.start, time.offset=time.offset, time.unit=c(years=FALSE, months=TRUE, days=FALSE))

    data.out <- array(NA, c(length(lon), length(lat), length(time)*12))

    dummy <- apply(df, 1, function(x) {
                     data.out[(x[1] - min(lon)) * 2 + 1, (max(lat) - x[2]) * 2 + 1, ((x[3]-start.year)*12+1):((x[3]-start.year+1)*12)] <<- x[4:15]
                   })

    ## if needs to be multiplied or divided (e.g. for flux calculations)
    if (as.flux)
      data.out <- aperm(aperm(data.out, c(3, 1, 2)) / dpm, c(2, 3, 1))

    lpj2nc.var.save(ncout, scale * data.out, c("lon", "lat", "time"), attr) 

  } else {
    ## TODO: see above
    if (overwrite) 
      lpj2nc.dim.save(ncout, "time", time-1, time.start=time.start, time.offset=time.offset, time.unit=c(years=TRUE, months=FALSE, days=FALSE))

    data.out <- array(NA, c(length(colnames(df))-3, length(lon), length(lat), length(time)))

    dummy <- apply(df, 1, function(x) {
                     for (i in 1:(length(x)-3)) {
                       data.out[i, (x[1] - min(lon)) * 2 + 1, (max(lat) - x[2]) * 2 + 1, x[3]-start.year+1] <<- x[i+3]
                     }
                   })

    if (as.flux)
      data.out <- data.out / 365

    basename <- attr[['name']]
    for (i in 4:length(colnames(df))) {
      if (basename!="values") {
        attr[["name"]] = paste(basename, colnames(df)[i], sep="_")
        lpj2nc.var.save(ncout, scale * data.out[i-3,,,], c("lon", "lat", "time"), attr)
      } else {
        attr[["name"]] = colnames(df)[i]
        lpj2nc.var.save(ncout, scale * data.out[i-3,,,], c("lon", "lat", "time"), attr)
      }
    }
  }
  if (exists("TITLE")) 
    att.put.nc(ncout, "NC_GLOBAL", "title", "NC_CHAR", TITLE)
  if (exists("CONTACT")) 
    att.put.nc(ncout, "NC_GLOBAL", "contact", "NC_CHAR", CONTACT)
  if (exists("ADDRESS")) 
    att.put.nc(ncout, "NC_GLOBAL", "address", "NC_CHAR", ADDRESS)
  if (exists("INSTITUTION")) 
    att.put.nc(ncout, "NC_GLOBAL", "institution", "NC_CHAR", INSTITUTION)
  if (exists("CLIMATE.FORCING"))
    att.put.nc(ncout, "NC_GLOBAL", "climate_forcing", "NC_CHAR", CLIMATE.FORCING)
  RNetCDF.info <- library(help = RNetCDF)
  RLPJGUESS.info <- library(help = RLPJGUESS)
  if (exists("LPJGUESS.VERSION")) {
    att.put.nc(ncout, "NC_GLOBAL", "software", "NC_CHAR",
               paste("LPJ-GUESS ", LPJGUESS.Version,
                     ", ", R.version$version.string,
                     ", ", sub(".* ", "RLPJGUESS ", RLPJGUESS.info$info[[1]][2]), 
                     ", ", sub(".* ", "RNetCDF ", RNetCDF.info$info[[1]][2]), 
                     sep = ""))
  } else {
    att.put.nc(ncout, "NC_GLOBAL", "software", "NC_CHAR",
               paste(R.version$version.string,
                     ", ", sub(".* ", "RLPJGUESS ", RLPJGUESS.info$info[[1]][2]), 
                     ", ", sub(".* ", "RNetCDF ", RNetCDF.info$info[[1]][2]), 
                     sep = ""))
  }
  att.put.nc(ncout, "NC_GLOBAL", "platform", "NC_CHAR", Sys.getenv("R_PLATFORM"))
  #att.put.nc(ncout, "NC_GLOBAL", "user", "NC_CHAR", Sys.getenv("USER"))
  att.put.nc(ncout, "NC_GLOBAL", "date", "NC_CHAR", format(Sys.time(), "%d-%m-%Y %H:%M:%S %Z"))
  sync.nc(ncout)
  close.nc(ncout)
  return(TRUE)
}
