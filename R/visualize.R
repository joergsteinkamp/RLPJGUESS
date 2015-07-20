lpj.map <- function(d, variable=NA, cols=NA, title=NA, sym.col=FALSE, wrap.variable=NA, wrap=1) {
  # check for compatibility
  if (!is.data.frame(d)) 
    stop("No data.frame given!")
  if (all(colnames(d) != "Lon"))
    stop("No column named 'Lon' present!")
  if (all(colnames(d) != "Lat"))
    stop("No column named 'Lat' present!")
  if (is.na(variable) && all(colnames(d) != "value"))
    stop("No column name given and no column named 'value' present!")

  if (!exists("worldmap")) {
    data(worldmap)
  }

  # calculate the map resolution
  lon <- sort(unique(d$Lon))
  lat <- sort(unique(d$Lat))
  res <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)],
             lat[2:length(lat)] - lat[1:(length(lat)-1)])

  # define plot region
  lon.limit <- c(min(lon) - res/2, max(lon) + res/2)
  lat.limit <- c(min(lat) - res/2, max(lat) + res/2)

  # define axis labels
  lon <- seq(lon.limit[1], lon.limit[2], res)
  lat <- seq(lat.limit[1], lat.limit[2], res)

  # exclude outer most longitude labels
  lon <- lon[2:(length(lon)-1)]
  
  if (lon.limit[2] - lon.limit[1] >= 360) {
    lon <- sort(unique(floor(abs(lon/60)) * sign(lon) * 60))
  } else if (lon.limit[2] - lon.limit[1] >= 180) {
    lon <- sort(unique(floor(abs(lon/45)) * sign(lon) * 45))
  } else {
    lon <- sort(unique(floor(abs(lon/30)) * sign(lon) * 30))
  }
  if (lat.limit[2] - lat.limit[1] >= 180) {
    lat <- sort(unique(floor(abs(lat/45)) * sign(lat) * 45))
  } else if (lat.limit[2] - lat.limit[1] >= 90) {
    lat <- sort(unique(floor(abs(lat/30)) * sign(lat) * 30))
  } else {
    lat <- sort(unique(floor(abs(lat/15)) * sign(lat) * 15))
  }

  # sprintf("%X", as.integer(charToRaw("°"))) => [1] "C2" "B0"
  # paste0("\u00B0") => "°"
  lon.lab <- lon
  lon.lab[lon < 0]  = paste(abs(lon[lon < 0]), "\u00B0W", sep="")
  lon.lab[lon >= 0] = paste(abs(lon[lon >= 0]), "\u00B0E", sep="")
  lat.lab <- lat
  lat.lab[lat < 0]  = paste(abs(lat[lat < 0]), "\u00B0S", sep="")
  lat.lab[lat >= 0] = paste(abs(lat[lat >= 0]), "\u00B0N", sep="")

  # create plot
  p <- ggplot(d, aes(y=Lat, x=Lon))
  p <- p + lpj.map_theme

  if (is.na(variable)) {
    p <- p + geom_raster(aes(fill=value))
  } else {
    p <- eval(parse(text=paste("p + geom_raster(aes(fill=",variable,"))", sep="")))
  }

  if (sym.col && is.na(variable)) {
    p <- p + expand_limits(fill = c(-max(abs(d$value), na.rm=TRUE), max(abs(d$value), na.rm=TRUE)))
  } else if (sym.col) {
    p <- eval(parse(text=paste('p + expand_limits(fill = c(-max(abs(d$', variable, '), na.rm=TRUE), max(abs(d$',variable,'), na.rm=TRUE)))',sep="")))
  }

  if (!any(is.na(cols))) {
    if (any(colnames(cols)=="colour") && any(colnames(cols)=="value")) {
      p <- p + scale_fill_gradientn(colours=cols$colour, values=cols$value, expand=c(0,0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0,0)))
    } else if (any(colnames(cols)=="colour") && any(colnames(cols)=="name")) {
      p <- p + scale_fill_manual(values=cols$colour, na.value="grey",guide=guide_legend(ncol=4))
      p <- p + theme(legend.key.width  = unit(0.03, "npc"))
    } else {
      p <- p + scale_fill_gradientn(colours=cols, expand=c(0,0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0,0)))
      #warning(paste("cols has wrong column names:", paste(colnames(cols))))
    }
  }
  p <- p + geom_path(data=worldmap, size=0.1, colour = "black", aes(x=long, y=lat, group=group))
  p <- p + scale_x_continuous(breaks=lon, labels=lon.lab)
  p <- p + scale_y_continuous(breaks=lat, labels=lat.lab) 
  p <- p + coord_fixed(xlim=lon.limit, ylim=lat.limit)
  p <- p + xlab("Longitude")
  p <- p + ylab("Latitude")

  if (!is.na(title))
    p <- p + labs(title=title)
  if (!is.na(wrap.variable))
    p <- eval(parse(text=paste("p + facet_wrap(~", wrap.variable, ", ncol=",wrap,")", sep="")))
#  if (is.na(variable))
#    p <- p + facet_wrap(~variable, ncol=wrap)

  return(p)
}


lpj.scatter <- function(d, x.variable="x", y.variable="y", wrap.variable=NA, col.variable=NA, cols=NA, alpha=0.7, lines=NA, labels="", lab.pos="bottomright", title=NA, sym.col=FALSE, equal.axis=FALSE, wrap=0) {
  # check for compatibility
  if (!is.data.frame(d)) 
    stop("No data.frame given!")
  if (all(colnames(d) != x.variable))
    stop(paste("No column named '", x.variable, "' present!", sep=""))
  if (all(colnames(d) != y.variable))
    stop(paste("No column named '", y.variable, "' present!", sep=""))
  if (!is.na(col.variable))
    if (all(colnames(d) != col.variable))
      stop(paste("No column named '", col.variable, "' present!", sep=""))
  if (!is.na(wrap.variable))
    if (all(colnames(d) != wrap.variable))
      stop(paste("No column named '", wrap.variable, "' present!", sep=""))
  if (!all(is.na(lines))) {
    if (is.data.frame(lines)) {
      if (all(colnames(lines) != "method"))
        stop(paste("No column named 'method' present in data.frame 'lines'!", sep=""))
      if (all(colnames(lines) != "se"))
        lines$se=FALSE
      if (all(colnames(lines) != "col"))
        lines$col="black"
      if (all(colnames(lines) != "type"))
        lines$type="solid"
      if (all(colnames(lines) != "size"))
        lines$size=1
    } else if (is.vector(lines) && typeof(lines) == "character") {
      lines <- data.frame(method=lines, se=FALSE, col="black", type="solid", size=1)
    }
  }

  # create a new data.frame 'd'
  if (is.na(col.variable)) {
    if (is.na(wrap.variable)) {
      d <- eval(parse(text=paste('data.frame(x=d$', x.variable, ', y=d$', y.variable, ', stringsAsFactors=FALSE)', sep="")))
    } else {
      d <- eval(parse(text=paste('data.frame(x=d$', x.variable, ', y=d$', y.variable, ', ', wrap.variable, '=d$', wrap.variable, ', stringsAsFactors=FALSE)', sep="")))
    }
  } else {
    if (is.na(wrap.variable)) {
      d <- eval(parse(text=paste('data.frame(x=d$', x.variable, ', y=d$', y.variable, ', col=d$', col.variable, ', stringsAsFactors=FALSE)', sep="")))
    } else {
      d <- eval(parse(text=paste('data.frame(x=d$', x.variable, ', y=d$', y.variable, ', col=d$', col.variable, ', ', wrap.variable, '=d$',wrap.variable, ', stringsAsFactors=FALSE)', sep="")))
    }
  }

  lm.d <- lm(d$y ~ d$x)
  sum.lm <- summary(lm.d)

  if (equal.axis) {
    extent <- data.frame(x=c(min(d$x, d$y, na.rm=TRUE), max(d$x, d$y, na.rm=TRUE)),
                         y=c(min(d$x, d$y, na.rm=TRUE), max(d$x, d$y, na.rm=TRUE)))
  } else {
    extent <- data.frame(x=c(min(d$x, na.rm=TRUE), max(d$x, na.rm=TRUE)),
                         y=c(min(d$y, na.rm=TRUE), max(d$y, na.rm=TRUE)))
  }

  # create plot
  if (is.na(col.variable)) {
    p <- ggplot(d, aes(x=x, y=y))
  } else {
    p <- ggplot(d, aes(x=x, y=y, col=col))
  }
  p <- p + lpj.scatter_theme

  if (!is.na(title[4]))
    p <- p + theme(legend.title=element_text(size=10, face="bold"))

#  message("JS_BUG: color set to grey for EGU 2015")
#  p <- p + geom_point(alpha=alpha, size=1.5, shape=16, col="grey")
  if (is.atomic(cols)) {
    p <- p + geom_point(alpha=alpha, size=1.5, shape=16, col=cols)
  } else {
    p <- p + geom_point(alpha=alpha, size=1.5, shape=16)
  }
  if (!is.na(col.variable) && all(!is.na(cols))) {
    if (any(colnames(cols)=="value") && any(colnames(cols)=="colour")) {
      p <- p + scale_colour_gradientn(colours=cols$colour, values=cols$value)
      if (!is.na(sym.col))
        if (sym.col)
          p <- p + expand_limits(colour = c(-max(abs(d$col)), max(abs(d$col))))
    } else if (any(colnames(cols)=="name") && any(colnames(cols)=="colour")) {
      #message("JS_DEBUG: error with wrapping occurs here somtimes.")
      #print(str(d))
      #print(str(cols))
      man.cols <- eval(parse(text=paste("c('",paste(cols$name, cols$colour, sep="'='", collapse="','"),"')", sep="")))
      p <- p + scale_fill_manual(values=man.cols, na.value="grey", guide=guide_legend(ncol=4))
      p <- p + scale_colour_manual(values=man.cols, na.value="grey", guide=guide_legend(ncol=4))
      p <- p + theme(legend.key.width  = unit(0.03, "npc"))
    } else {
      warning(paste("cols has wrong column names:", paste(colnames(cols))))
    }
  }

  if (!all(is.na(lines))) {
    if (any(lines$method=="1:1")) {
      i=which(lines$method=="1:1")
      ldat <- data.frame(x=c(min(d$x, d$y, na.rm=TRUE), max(d$x, d$y, na.rm=TRUE)),
                         y=c(min(d$x, d$y, na.rm=TRUE), max(d$x, d$y, na.rm=TRUE)))
      p <- eval(parse(text=paste('p + geom_line(data=ldat, aes(x=x, y=y), col="', lines$col[i],
                          '", linetype="', lines$type[i],
                          '", size=', lines$size[i],')', sep="")))
    }

    for (i in 1:nrow(lines)) {
      if (lines$method[i]=="1:1") next
      if (lines$method[i]=="gam") {
        p <- eval(parse(text=paste('p + geom_smooth(method="', lines$method[i],
                            '", formula=y ~ s(x, bs = "cs")',
                            ', se=', lines$se[i],
                            ', col="', lines$col[i],
                            '", linetype="', lines$type[i],
                            '", size=', lines$size[i], ")", sep="")))
      } else {
        p <- eval(parse(text=paste('p + geom_smooth(method="', lines$method[i],
                            '", se=', lines$se[i],
                            ', col="', lines$col[i],
                            '", linetype="', lines$type[i],
                            '", size=', lines$size[i], ")", sep="")))
      }
    }
  }

  if (lab.pos == "topleft" || lab.pos == "tl") {
    x.pos  <- extent$x[1]
    y.pos  <- extent$y[2]
    h.just <- -0.1
    v.just <- 1.75
    v.diff <- 1.5
  } else if (lab.pos == "topright" || lab.pos == "tr") {
    x.pos  <- extent$x[2]
    y.pos  <- extent$y[2]
    h.just <- 1.1
    v.just <- 1.75
    v.diff <- 1.5
  } else if (lab.pos == "bottomright" || lab.pos == "br") {
    x.pos  <- extent$x[2]
    y.pos  <- extent$y[1]
    h.just <- 1.1
    v.just <- -0.1
    v.diff <- -1.7
  } else if (lab.pos == "bottomleft" || lab.pos == "bl") {
    x.pos  <- extent$x[1]
    y.pos  <- extent$y[1]
    h.just <- -0.1
    v.just <- -0.1
    v.diff <- -1.7
  }

  if (any(colnames(cols)=="name") && wrap) {
    p <- p + theme(legend.position = "none")
  } else {
    for (i in 1:length(labels)) {
      if (labels[i]=="eq") {
        label <- paste("y == ", signif(sum.lm$coefficients[2,1], 3), "* x +", signif(sum.lm$coefficients[1,1], 3))
        p <- eval(parse(text=paste('p + annotate("text", x=', x.pos,
                            ', y=', y.pos,
                            ', hjust=', h.just,
                            ', vjust=', v.just,
                            ', label="', label,
                            '", col="black", parse=TRUE)', sep="")))
        v.just <- v.just + v.diff
      } else if (labels[i]=="rsq") {
        v.just <- v.just - v.diff/2.0
        if (grepl("^b", lab.pos))
          v.just <- v.just + v.diff/8.0
        label <- paste("R^2 == ", signif(sum.lm$r.squared, 3))
        p <- eval(parse(text=paste('p + annotate("text", x=', x.pos,
                            ', y=', y.pos,
                            ', hjust=', h.just,
                            ', vjust=', v.just,
                            ', label="', label,
                            '", col="black", parse=TRUE)', sep="")))
        
        v.just <- v.just + 1.5*v.diff
        if (grepl("^b", lab.pos))
          v.just <- v.just - v.diff/8.0
      } else if (labels[i]=="rmse") {
        label <- paste("RMSE == ", signif(mean(sqrt((residuals(lm.d))^2), na.rm=TRUE), 3))
        p <- eval(parse(text=paste('p + annotate("text", x=', x.pos,
                            ', y=', y.pos,
                            ', hjust=', h.just,
                            ', vjust=', v.just,
                            ', label="', label,
                            '", col="black", parse=TRUE)', sep="")))
        v.just <- v.just + v.diff
      } else if (labels[i]=="me") {
        label <- paste("ME == ", signif(1 - sum((d$y-d$x)^2, na.rm=TRUE) / sum((d$y-mean(d$y))^2, na.rm=TRUE), 3))
        p <- eval(parse(text=paste('p + annotate("text", x=', x.pos,
                            ', y=', y.pos,
                            ', hjust=', h.just,
                            ', vjust=', v.just,
                            ', label="', label,
                            '", col="black", parse=TRUE)', sep="")))
        v.just <- v.just + v.diff
      }
    }
  }

  if (equal.axis)
    p <- p + coord_fixed(xlim=extent$x, ylim=extent$y)

  if (!is.na(title[1]))
    p <- p + labs(title=title[1])
  if (!is.na(title[2]))
    p <- p + xlab(title[2])
  if (!is.na(title[3]))
    p <- p + ylab(title[3])

  if (any(colnames(cols)=="value") && any(colnames(cols)=="colour")) {
    if (!is.na(title[4])) {
      p <- p + guides(colour = guide_colourbar(title=title[4], nbin=101, override.aes = list(alpha = 1)))
    } else {
      p <- p + guides(colour = guide_colourbar(title=NULL, nbin=101, override.aes = list(alpha = 1)))
    }
  } else if (any(colnames(cols)=="name") && any(colnames(cols)=="colour")) {
    if (!is.na(title[4])) {
      p <- p + guides(colour = guide_legend(title=title[4], ncol=3, override.aes = list(size=4, alpha = 1)))
    } else {
      p <- p + guides(colour = guide_legend(title=NULL, ncol=3, override.aes = list(size=4, alpha = 1)))
    }
    if (!is.na(col.variable) && wrap)
      p <- eval(parse(text=paste("p + facet_wrap(~", wrap.variable, ", ncol=",wrap,")", sep="")))
  }
  return(p)
}

