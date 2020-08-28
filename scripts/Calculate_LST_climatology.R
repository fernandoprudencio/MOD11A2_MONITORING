#' @title
#' Temperature climatology data
#'
#' @description
#' this script calculates the Temperature climatology data
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "ncdf4", "Hmisc")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x, dependencies = T)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(ncdf4)
library(Hmisc)

#' CONSTANTS
k.omit <- c(2005, 2010, 2016)
k.prdo <- 2000:2020
k.data <- "mod11a2" # mod11a2 or PISCO data
k.type <- "Day" # Day or Night monthly data

#' LIST OF DATA 
lst.temp <- list.files(
  sprintf("data/raster/%s/monthly", k.data),
  pattern = k.type, full.names = T
)

#' BUILD DATE DATAFRAME
df <- tibble(
  date = seq(as.Date("2000-02-01"), as.Date("2020-08-01"), by = "1 month")
) %>%
  mutate(id = 1:n())

#' Build a function to calculate climatology
fun.clim <- function(month, years.omit, data) {
  grd.mt <- df %>%
    filter(
      str_sub(date, 6, 7) == month &
        str_sub(date, 1, 4) %nin% years.omit
    )
  
  data[grd.mt$id] %>%
    stack() %>%
    "*"(1) %>%
    mean(na.rm = T) %>%
    return()
}

#' Apply fun.clim function with sapply()
grd.norm <- sapply(
  sprintf("%02d", 1:12),
  FUN = fun.clim,
  years.omit = c(k.omit, 2020),
  data = lst.temp
) %>% stack() %>% "*"(1) %>%
  "names<-"(sprintf(month.abb))

grd.dry <- sapply(
  sprintf("%02d", 1:12),
  FUN = fun.clim,
  years.omit = k.prdo[k.prdo %nin% k.omit],
  data = lst.temp
) %>% stack() %>% "*"(1) %>%
  "names<-"(sprintf(month.abb))

#' Save climatology raster as .grd format
name <- basename(lst.temp[1]) %>% str_sub(1, -14)
writeRaster(
  grd.norm, sprintf("data/raster/mod11a2/climatology/%sNORMclim.nc", name),
  overwrite = T, datatype = "INT2S"
)

writeRaster(
  grd.dry, sprintf("data/raster/mod11a2/climatology/%sDRYclim.nc", name),
  overwrite = T, datatype = "INT2S"
)