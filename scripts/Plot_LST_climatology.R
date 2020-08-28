#' @title
#' Plot average temperature map of july, august, september, ovtober and november
#'
#' @description
#' this script plot average temperature map of july, august, september, october
#'   and november from climatology of average and maximum monthly data
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "sf", "lattice", "extrafont",
  "cptcity", "latticeExtra", "rasterVis", "maptools", "grid"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(lattice)
library(extrafont)
library(cptcity)
library(latticeExtra)
library(rasterVis)
library(maptools)
library(grid)
library(magick)

#' CONSTANTS
k.dep <- c(
  "Piura", "Cajamarca", "La Libertad", "Ancash", "Loreto", "Huancavelica",
  "Amazonas", "Madre de Dios", "Cusco", "Apurimac", "Puno", "Huanuco", "Pasco",
  "Junin"
)
k.months <- c(7:11)
k.data <- "mod11a2" # mod11a2 or pisco data
k.cond <- "normal" # dry or normal conditions

if (k.data == "mod11a2") {
  k.per.str <- 2000
  k.per.end <- 2019
}
if (k.data == "pisco") {
  k.per.str <- 1981
  k.per.end <- 2016
}

#' LOAD RASTER DATA OF GRIDDED TEMPERATURE
ref <- raster(
  nrow = 2221, ncol = 1589, xmn = -81.50833,
  xmx = -68.26667, ymn = -18.41667, ymx = 0.09166667
) %>%
  "res<-"(0.008333333) %>%
  "values<-"(0)

if (k.data == "mod11a2") {
  input <- list.files(
    "data/raster/mod11a2/climatology",
    sprintf(
      "Day_1km_%1$s", toupper(k.cond) %>% str_sub(1, 3)
    ),
    full.names = T
  )
  
  grd.avr <- brick(input)[[k.months]] %>%
    mean(na.rm = T) %>%
    crop(ref) %>%
    resample(ref) %>%
    "*"(0.02) %>%
    "+"(-273.15)
}

if (k.data == "pisco") {
  input <- list.files(
    "data/raster/pisco/climatology",
    sprintf(
      "MEANtemp_clim%1$s", toupper(k.cond) %>% str_sub(1, 3)
    ),
    full.names = T
  )
  
  grd.avr <- brick(input)[[k.months]] %>%
    mean(na.rm = T) %>%
    crop(ref) %>%
    resample(ref)
}

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)
#'     load sp data
sp.world <- as(st_geometry(sf.world), Class = "Spatial")

#'   load Peru departaments points
#'     load sf data
sf.peru <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
) %>%
  st_centroid(of_largest_polygon = FALSE) %>%
  filter(Departamen %in% k.dep) %>%
  mutate(Departamen = as.character(Departamen))
#'     load sp data
sp.peru <- as(st_geometry(sf.peru), Class = "Spatial")

#'   load Peru departaments polygon
#'     load sf data
sf.dep <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
)
#'     load sp data
sp.dep <- as(st_geometry(sf.dep), Class = "Spatial")

#' MASKING RASTER
grd.avr <- grd.avr %>%
  mask(sf.world %>% filter(COUNTRY == "Peru"))
grd.avr[grd.avr < 0] <- 0
grd.avr[grd.avr > 50] <- 50

#' SAVE PLOT
name <- sprintf(
  "exports/%1$s_%2$s_conditions_%3$s-%4$s.png",
  tolower(k.data), k.cond, k.per.str, k.per.end
)

title <- sprintf(
  "Average maximum temperature (jul - nov) of %1$s data\nfor %2$s conditions from %3$s to %4$s",
  toupper(k.data), k.cond, k.per.str, k.per.end
)

png(name, res = 500, height = 28, width = 20, units = "cm")

levelplot(grd.avr,
  main = list(title,
    side = 1, line = 0.5, fontfamily = "Source Sans Pro"
  ),
  scales = list(
    x = list(limits = c(-81.8, -68.2)),
    y = list(limits = c(-18.7, .4))
  ),
  col.regions = cpt(
    pal = "grass_bcyr", n = 100, colorRampPalette = FALSE
  ),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = seq(0, 50, 5),
  colorkey = list(
    at = seq(0, 50, 5),
    space = "right", # location of legend
    labels = list(at = seq(0, 50, 10), cex = 1.1),
    font = list(family = "Source Sans Pro")
  ),
  xlab = NULL,
  ylab = NULL,
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro"),
    par.sub.text = list(fontfamily = "Source Sans Pro")
  )
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = 2),
    # sp.lines(sp.dep, col = "black", lwd = .8),
    sp.points(sp.peru, pch = 20, cex = 1, col = "black"),
    sp.text(
      coordinates(sp.peru),
      txt = sf.peru$Departamen, pos = 1, cex = 1.2,
      fontfamily = "Source Sans Pro"
    )
  )

grid::grid.text(
  "[°C]",
  x = unit(.935, "npc"),
  y = unit(.93, "npc"),
  rot = 0,
  gp = gpar(
    fontsize = 18,
    fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

#' CLOSE THE SAVED OF PLOT
dev.off()

#' TRIM FIGURE
img <- magick::image_read(name, strip = TRUE) %>%
  image_trim() %>%
  image_border("white", "50x50")

#' SAVE FIGURE
image_write(img, path = name, format = "png")