#' @title
#' quality filter for mod09a1 dataset
#'
#' @description
#' this script filter mod09a1 dataset
#'
#' @author Fernando Prudencio
#'
#' @data
#'

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "rgdal", "foreach", "doParallel", "DescTools",
  "filesstrings", "stringr"
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

#' LOAD PACKAGES
library(doParallel)
library(foreach)
library(tidyverse)
library(filesstrings)

#' LOAD FUNCTIONS
source("scripts/functions.R")

#' FILE DATASET LINKS (INPUT AND OUTPUT)
rut.in <- "data/raster/mod11a2/withoutFILTER"
rut.out <- "data/raster/mod11a2/withFILTER"

#' DESCRIPTION OF QUALITY BAND
#'   This is the order of 16 bits of the quality band
#'     (07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
#'     (01)(02)(03)(04)(05)(06)(07)(08) - R NOMENCLATURE
#'
#'   Mandatory QA flags -------------------------> bit 00-01
#'   Data quality flag --------------------------> bit 02-03
#'   Emis Error flag ----------------------------> bit 04-05
#'   LST LST Error flag -------------------------> bit 06-07

filter <- list(
  c("10", "11"), 
)

#' LIST OF QA BANDS
qa <- list.files(rut.in, pattern = "QC", full.names = T)

#' LIST OF THERMAL BANDS
band <- list.files(rut.in, pattern = "LST", full.names = T)

#' DEFINE HOW MANY CLUSTER YOU WANT TO USE
use.cores <- detectCores() - 2

#' make and register cluster
cluster <- makeCluster(use.cores)
registerDoParallel(cluster)

#' use foreach() loop and %dopar% command to run in parallel
foreach(i = 1:length(qa)) %dopar% {
  #' load packages
  library(DescTools)
  library(tidyverse)
  library(raster)
  library(rgdal)
  library(stringr)
  library(filesstrings)

  file <- qaFilter(
    raster(band[i]),
    raster(qa[i]),
    "mxd11a2", filter
  )
  
  name <- band[i] %>%
    basename() %>%
    str_sub(1, -13) %>%
    sprintf(fmt = "%s_qafilter.tif")

  k.yr <- str_sub(name, -20, -17)

  if (!sprintf("%s/%s", rut.in, k.yr) %>% dir.exists()) {
    dir.create(sprintf("%s/%s", rut.in, k.yr))
  }

  if (!sprintf("%s/%s", rut.out, k.yr) %>% dir.exists()) {
    dir.create(sprintf("%s/%s", rut.out, k.yr))
  }

  writeRaster(file, sprintf("%s/%s", rut.out, name), overwrite = T)
  file.move(band[i], sprintf("%s/%s", rut.in, k.yr))
}

#' end cluster
stopCluster(cluster)