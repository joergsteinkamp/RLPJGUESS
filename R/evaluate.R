lpj.eval_scatter <- function(d, x.variable="x", y.variable="y", col.variable=NA, wrap.variable=NA, wrap.cols=3, alpha=0.2, line="1:1", title=NA, err.bar=TRUE) {

  if (!is.na(col.variable)) {
    col.levels <- eval(parse(text=paste("unique(d$", col.variable, ")", sep="")))
    sens.legend <- data.frame(name=sort(col.levels), colour=brewer.pal(length(col.levels), "Set1"), stringsAsFactors=FALSE)
  } else {
    sens.legend <- NA
  }

  p <- lpj.scatter(d, x.variable=x.variable, y.variable=y.variable, col.variable=col.variable, wrap.variable=wrap.variable,
                   equal.axis=TRUE, alpha=alpha, lines=line, cols=sens.legend, title=title)

  if (err.bar) {

    if (!is.na(col.variable) && !is.na(wrap.variable)) {
      eval.str <- paste("ddply(d, .(", col.variable , ",", wrap.variable, "), summarise, ",
                        "x.mn=mean(", x.variable, ", na.rm=TRUE),",
                        "x.sd=sd(", x.variable, ", na.rm=TRUE),",
                        "y.mn=mean(", y.variable, ", na.rm=TRUE),",
                        "y.sd=sd(", y.variable, ", na.rm=TRUE))", sep="")
    } else if  (!is.na(col.variable)) {
      eval.str <- paste("ddply(d, .(", col.variable, "), summarise, ",
                        "x.mn=mean(", x.variable, ", na.rm=TRUE),",
                        "x.sd=sd(", x.variable, ", na.rm=TRUE),",
                        "y.mn=mean(", y.variable, ", na.rm=TRUE),",
                        "y.sd=sd(", y.variable, ", na.rm=TRUE))", sep="")
    } else if  (!is.na(wrap.variable)) {
      eval.str <- paste("ddply(d, .(", wrap.variable, "), summarise, ",
                        "x.mn=mean(", x.variable, ", na.rm=TRUE),",
                        "x.sd=sd(", x.variable, ", na.rm=TRUE),",
                        "y.mn=mean(", y.variable, ", na.rm=TRUE),",
                        "y.sd=sd(", y.variable, ", na.rm=TRUE))", sep="")
    } else {
      eval.str <- paste("ddply(d, .(), summarise, ",
                        "x.mn=mean(", x.variable, ", na.rm=TRUE),",
                        "x.sd=sd(", x.variable, ", na.rm=TRUE),",
                        "y.mn=mean(", y.variable, ", na.rm=TRUE),",
                        "y.sd=sd(", y.variable, ", na.rm=TRUE))", sep="")

    }
    stat <- eval(parse(text=eval.str))
      #ddply(d, .(name), summarise,
      #            lpj.mn=mean(lpj, na.rm=TRUE),
      #            lpj.sd=sd(lpj, na.rm=TRUE),
      #            beer.mn=mean(beer, na.rm=TRUE),
      #            beer.sd=sd(beer, na.rm=TRUE))
    if (is.na(col.variable)) {
      err.v <- aes(y=y.mn, x=x.mn, ymin=y.mn-y.sd, ymax=y.mn+y.sd) 
      err.h <- aes(y=y.mn, x=x.mn, xmin=x.mn-x.sd, xmax=x.mn+x.sd)
    } else {
      err.v <- eval(parse(text=paste("aes(y=y.mn, x=x.mn, ymin=y.mn-y.sd, ymax=y.mn+y.sd, col=", col.variable, ")", sep=""))) 
      err.h <- eval(parse(text=paste("aes(y=y.mn, x=x.mn, xmin=x.mn-x.sd, xmax=x.mn+x.sd, col=", col.variable, ")", sep=""))) 
    }
    p <- p + geom_errorbar(data=stat, mapping=err.v, alpha=1, size=1, width=0.1)
    p <- p + geom_errorbarh(data=stat, mapping=err.h, alpha=1, size=1, height=0.1)
  }

  if (!is.na(wrap.variable)) 
    p <- eval(parse(text=paste("p + facet_wrap(~", wrap.variable, ", ncol=", wrap.cols, ")", sep="")))
  return(p)
}











lpj.eval_group_sum <- function(d, sum.variable="area", wgt.variable=NA, name="name", sens="sens", scale=1, col=TRUE, shape=NA, jitter=0.1, title=c(NA, "area", "name")) {
  if (!is.na(shape))
    if (typeof(shape) == "logical") {
      if (!shape) shape=NA
    } else if (!is.na(sens)) {
      if (shape != "open" || shape != "solid") shape=NA
    }

  if (is.na(sens) && as.character(col)=="TRUE") col=NA
  
  if (is.na(sens)) {
    if (is.na(wgt.variable)) {
      eval.str <- paste("ddply(d,.(", name, "),summarize,sum=", scale, "*sum(", sum.variable, "))")
    } else {
      eval.str <- paste("ddply(d,.(", name, "),summarize,sum=sum(" , sum.variable, " * ", wgt.variable, "))")
    }
  } else {
    if (is.na(wgt.variable)) {
      eval.str <- paste("ddply(d,.(", name, ",", sens, "),summarize,sum=", scale, "*sum(", sum.variable, "))")
    } else {
      eval.str <- paste("ddply(d,.(", name, ",", sens, "),summarize,sum=", scale, "*sum(" , sum.variable, "*", wgt.variable, "))")
    }
  }
  d.sum <- eval(parse(text=eval.str))

  if(is.na(sens)) {
    p <- eval(parse(text=paste("ggplot(d.sum, aes(x=sum, y=", name, "))", sep="")))
  } else {
    if (!col && is.na(shape)) {
      p <- eval(parse(text=paste("ggplot(d.sum, aes(x=sum, y=", name, "))", sep="")))
    } else if (col && is.na(shape)) {
      p <- eval(parse(text=paste("ggplot(d.sum, aes(x=sum, y=", name, ", col=", sens, "))", sep="")))
    } else if (!is.na(shape)) {
      if (!col) {
        p <- eval(parse(text=paste("ggplot(d.sum, aes(x=sum, y=", name, ", shape=", sens, "))", sep="")))
      } else {
        p <- eval(parse(text=paste("ggplot(d.sum, aes(x=sum, y=", name, ", col=", sens, ", shape=", sens, "))", sep="")))
      }
      if (shape == "solid") {
        p <- p + scale_shape(solid = TRUE)
      } else if (shape == "open") {
        p <- p + scale_shape(solid = FALSE)
      }
    }
    p <- p + scale_color_manual(values=brewer.pal(length(unique(d.sum$sens)),"Set1"),
                                na.value="grey", guide=guide_legend(ncol=2))
  }

  p <- p + lpj.scatter_theme

  if (is.na(sens)) {
    if(is.character(shape))
      shape = paste('"', shape, '"', sep="")
    if(is.character(col))
      col = paste('"', col, '"', sep="")
    
    if (is.na(col) && is.na(shape)) {
      p <- p + geom_point(size=3, position = position_jitter(width = 0, height = jitter))
    } else if (is.na(shape)) {
      p <- eval(parse(text=paste('p + geom_point(size=3, col=', col, ', position = position_jitter(width = 0, height = jitter))',sep="")))
    } else if (is.na(col) && !is.na(shape)) {
      p <- eval(parse(text=paste('p + geom_point(size=3, shape=', shape, ', position = position_jitter(width = 0, height = jitter))', sep="")))
    } else {
      p <- eval(parse(text=paste('p + geom_point(size=3, col=', col, ', shape=', shape, ', position = position_jitter(width = 0, height = jitter))', sep="")))
    }
  } else {
    p <- p + geom_point(size=3, position = position_jitter(width = 0, height = jitter))
  }

  if (!is.na(title[1]))
    p <- p + labs(title=title[1])
  if (!is.na(title[2]))
    p <- p + xlab(title[2])
  if (!is.na(title[3]))
    p <- p + ylab(title[3])

  return(p)
}

lpj.eval_biome.area <- function(d, area.scale=1, includeRefMap=TRUE, col=TRUE, shape=NA, jitter=0.1, title=c("", "area [km^2]","")) {
  if(includeRefMap) {
    data(hickler)

    lon <- extract.seq(hickler$Lon)
    lat <- extract.seq(hickler$Lat)
    area <- gridarea2d(lon=lon, lat=lat, scale=area.scale)
    area$area = area$area
    hickler <- merge(hickler, area, by=c("Lon", "Lat"))
    hickler <- rename(hickler, c("hickler.biome"="name"))
    hickler$sens = "Hickler et al. (2006)"

    biome <- rbind(d, hickler)
  } else {
    biome <- d
    rm(d)
  }

  return(lpj.eval_group_sum(biome, col=col, shape=shape, jitter=jitter, title=title))

  ## if (!col && is.na(shape)) {
  ##   p <- ggplot(biome.sum, aes(x=area, y=name))
  ## } else if (col && is.na(shape)) {
  ##   p <- ggplot(biome.sum, aes(x=area, y=name, col=sens))
  ## } else if (!is.na(shape)) {
  ##   if (!col) {
  ##     p <- ggplot(biome.sum, aes(x=area, y=name, shape=sens))
  ##   } else {
  ##     p <- ggplot(biome.sum, aes(x=area, y=name, col=sens, shape=sens))
  ##   }
  ##   if (shape == "solid") {
  ##     p <- p + scale_shape(solid = TRUE)
  ##   } else if (shape == "open") {
  ##     p <- p + scale_shape(solid = FALSE)
  ##   }
  ## }

  ## p <- p + lpj.scatter_theme
  ## p <- p + geom_point(size=3)
  ## p <- p + scale_color_manual(values=brewer.pal(length(unique(biome.sum$sens)),"Set1"),
  ##                             na.value="grey", guide=guide_legend(ncol=2))
  ## return(p)
}

lpj.eval_biome.map <- function(d, includeRefMap=TRUE, map.cols=3, legend.cols=3) {
  if(includeRefMap) {
    data(hickler)
    # plyr: hickler <- rename(hickler, c("hickler.biome"="name"))
    names(hickler)[names(hickler)=="hickler.biome"] <- "name"
    hickler$sens = "Hickler et al. (2006)"
    biome <- rbind(d, hickler)
  } else {
    biome <- d
    rm(d)
  }

  avail.biomes <- data.frame(name=levels(as.factor(biome$name)))
  avail.biomes$name = as.character(avail.biomes$name)
  biomes.legend <- data.frame(name=c("arctic/alpine tundra",
                                  "desert",
                                  "arid shrubland/steppe",
                                  "xeric woodland/shrubland",
                                  "dry grassland",
                                  "tall grassland",
                                  "dry savannah",
                                  "moist savannah",
                                  "tropical deciduous forest",
                                  "tropical rain forest",
                                  "tropical seasonal forest",
                                  "temperate conifer forest",
                                  "temperate mixed forest",
                                  "temperate broadleaved evergreen forest",
                                  "temperate deciduous forest",
                                  "temperate/boreal mixed forest",
                                  "boreal evergreen forest/woodland",
                                  "boreal deciduous forest/woodland"),
                              colour=c("#CECBE6", "#FAF7B3", "#F0F7E4", "#EE4D9A",
                                  "#FCE06B", "#FAC60F", "#D5B229", "#CCDB29",
                                  "#CA862A", "#108742", "#4DB848", "#CCE8CC", "#8AB53F",
                                  "#127ABF", "#8DC63F", "#77C799", "#169D97",
                                  "#97D8E5"))
  biomes.legend$name = as.character(biomes.legend$name)
  biomes.legend$colour = as.character(biomes.legend$colour)
  biomes.legend <- merge(biomes.legend, avail.biomes, all=TRUE, by="name")

  p <- lpj.map(biome, variable="name", cols=biomes.legend)
  p <- p + facet_wrap(~sens, ncol=map.cols)
  p <- p + guides(fill = guide_legend(ncol = legend.cols))

  return(p)
}
