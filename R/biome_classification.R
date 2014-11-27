.get.dom.pft <- function(x) {
  if (any(is.na(x[3:length(x)]))) {
    warning("NAs present is LAI data!")
    return(NA)
  }
  if (all(x[3:length(x)] < 5.e-3)) return(NA)
  dom <- which(x[3:length(x)] == max(x[3:length(x)]))
  if (length(dom)>1) {
    warning("More than one dominant PFT. 1st choosen:")
    print(x)
    dom = dom[1]
  }
  return(dom)
}

biome_classification <- function(lai) {
  biomes.legend <- data.frame(name=c("Arctic/alpine tundra",
                                  "Desert",
                                  "Arid shrubland/steppe",
                                  "Xeric woodland/shrubland",
                                  "Dry grassland",
                                  "Tall grassland",
                                  "Dry Savannah",
                                  "Moist Savannah",
                                "Tropical deciduous forest",
                                  "Tropical rainforest",
                                  "Tropical seasonal forest",
                                  "Temperate mixed forest",
                                  "Temperate broadleaved evergreen forest",
                                  "Temperate deciduous forest",
                                  "Temperate/boreal mixed forest",
                                  "Boreal evergreen forest/woodland",
                                  "Boreal deciduous forest/woodland"),
                              colour=c("#CECBE6", "#FAF7B3", "#F0F7E4", "#EE4D9A",
                                  "#FCE06B", "#FAC60F", "#D5B229", "#CCDB29",
                                  "#CA862A", "#108742", "#4DB848", "#8AB53F",
                                  "#127ABF", "#8DC63F", "#77C799", "#169D97",
                                  "#97D8E5"))

  pft.names <- colnames(lai)
  pft.names <- pft.names[pft.names != "Lon"]
  pft.names <- pft.names[pft.names != "Lat"]
  pft.names <- pft.names[pft.names != "Year"]
  pft.names <- pft.names[pft.names != "Total"]

  grass.lai <- lai[, c("Lon", "Lat", "C3G", "C4G")]
  
  tree.pft.names <- pft.names[pft.names != "C3G" & pft.names != "C4G"]
  
  tree.lai       <- lai[, c("Lon", "Lat", tree.pft.names)]
  tree.lai$TrBE  <- tree.lai$TrBE + tree.lai$TrIBE
  tree.lai$BNE   <- tree.lai$BNE + tree.lai$BINE
  tree.pft.names <- tree.pft.names[tree.pft.names != "TrIBE" & tree.pft.names != "BINE"]
  if (any(tree.pft.names=="TeIBS")) {
    tree.lai$TeBS  <- tree.lai$TeBS + tree.lai$TeIBS
    tree.pft.names <- tree.pft.names[tree.pft.names != "TeIBS"]
  }
  tree.lai <- tree.lai[, c("Lon", "Lat", tree.pft.names)]

  dom.pft <- apply(tree.lai, 1, .get.dom.pft)
  tree.lai$dom.pft = dom.pft
  tree.pft.names <- colnames(tree.lai)
  tree.pft.names <- tree.pft.names[3:(length(tree.pft.names)-1)]

  tree.lai$dom.pft.name = tree.pft.names[tree.lai$dom.pft]
  tree.lai$total = apply(tree.lai[, tree.pft.names], 1, sum)

  tree.lai$trop = tree.lai$TrBE + tree.lai$TrBR
  if (any(tree.pft.names=="BIBS")) {
    tree.lai$boreal = tree.lai$BNE + tree.lai$BIBS + tree.lai$BNS
  } else {
    tree.lai$boreal = tree.lai$BNE + tree.lai$IBS + tree.lai$BNS
  }
  
  biomes <- data.frame(Lon=tree.lai$Lon, Lat=tree.lai$Lat, name=NA)
  # tropical biomes
  biomes$name[is.na(biomes$name) & tree.lai$total > 2.5 &
                 tree.lai$TrBE > 0.6 * tree.lai$total &
                 tree.lai$dom.pft.name=="TrBE"] = "Tropical rainforest"
  biomes$name[is.na(biomes$name) & tree.lai$total > 2.5 &
                 tree.lai$TrBR > 0.6 * tree.lai$total &
                 tree.lai$dom.pft.name=="TrBR"] = "Tropical deciduous forest"
  biomes$name[is.na(biomes$name) & tree.lai$total > 2.5 &
                 tree.lai$trop > 0.5 * tree.lai$total &
                 (tree.lai$dom.pft.name=="TrBE" | tree.lai$dom.pft.name=="TrBR")] = "Tropical seasonal forest"
  # boreal biomes
  biomes$name[is.na(biomes$name) & tree.lai$total > 0.5 & 
                 tree.lai$boreal > 0.8 * tree.lai$total &
                 tree.lai$dom.pft.name=="BNE"] =
                     "Boreal evergreen forest/woodland"
  biomes$name[is.na(biomes$name) & tree.lai$total > 0.5 & 
                 tree.lai$boreal > 0.8 * tree.lai$total &
                 (tree.lai$dom.pft.name=="BNS" | tree.lai$dom.pft.name=="BIBS" | tree.lai$dom.pft.name=="IBS")] = "Boreal deciduous forest/woodland"
  # temperate
  biomes$name[is.na(biomes$name) & tree.lai$total > 2.5 & 
                 (tree.lai$TeBS > 0.5 * tree.lai$total | tree.lai$TeBE > 0.5 * tree.lai$total) &
                 tree.lai$dom.pft.name=="TeBE"] = "Temperate broadleaved evergreen forest"
  biomes$name[is.na(biomes$name) & tree.lai$total > 2.5 & 
                 (tree.lai$TeBS > 0.5 * tree.lai$total | tree.lai$TeBE > 0.5 * tree.lai$total) &
                 tree.lai$dom.pft.name=="TeBS"] = "Temperate deciduous forest"
  biomes$name[is.na(biomes$name) & tree.lai$total > 2.5]  = "Temperate mixed forest"
  # NO Temperate mixed forest
  ## biomes$name[is.na(biomes$name) & tree.lai$total > 2.5]  = "Temperate/boreal mixed forest"
  # grass biomes
  biomes$name[is.na(biomes$name) & tree.lai$total >= 0.5 & tree.lai$total <= 2.5 &
                 (grass.lai$C3G + grass.lai$C4G) / lai$Total < 0.2]  = "Xeric woodland/shrubland"
  biomes$name[is.na(biomes$name) & tree.lai$total >= 0.5 & tree.lai$total <= 2.5 &
                 lai$Total > 2.5]  = "Moist Savannah"
  biomes$name[is.na(biomes$name) & tree.lai$total >= 0.5 & tree.lai$total <= 2.5 &
                 lai$Total <= 2.5]  = "Dry Savannah"
  biomes$name[is.na(biomes$name) & abs(tree.lai$Lat) >= 54 & tree.lai$total < 0.5 &
                 lai$Total > 0.2]  = "Arctic/alpine tundra"
  biomes$name[is.na(biomes$name) & grass.lai$C3G + grass.lai$C4G > 2.0]  = "Tall grassland"
  biomes$name[is.na(biomes$name) & tree.lai$total > 0.2 & 
                 grass.lai$C3G + grass.lai$C4G < 1.0]  = "Arid shrubland/steppe"
  biomes$name[is.na(biomes$name) & grass.lai$C3G + grass.lai$C4G > 0.2]  = "Dry grassland"
  biomes$name[is.na(biomes$name) & lai$Total > 0.2]  = "Arid shrubland/steppe"
  biomes$name[is.na(biomes$name) & lai$Total <= 0.2]  = "Desert"

  avail.biomes <- data.frame(name=levels(as.factor(biomes$name)))

  biomes.legend <- merge(biomes.legend, avail.biomes, by="name")
  biomes.legend$name = as.character(biomes.legend$name)
  biomes.legend$colour = as.character(biomes.legend$colour)
  biomes.legend <<- biomes.legend

  return(biomes)
}
