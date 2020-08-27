#' INSTALL PACKAGES
pkg <- c("tidyverse", "raster", "DescTools")

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD LIBRARIES
library(tidyverse)
library(raster)
library(DescTools)

#' change months from english language to spanish language
english.months <- c(
  "january", "february", "march", "april", "may", "june", "july", "august",
  "september", "october", "november", "december"
)

spanish.months <- c(
  "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto",
  "Septiembre", "Octubre", "Noviembre", "Diciembre"
)

to.spanish <- spanish.months
names(to.spanish) <- english.months

translate.date <- function(date, output.lang = "es") {
  if (output.lang == "es") {
    str_replace_all(tolower(date), to.spanish)
  }
}

#' this function filters MODIS dataset by quality band
#' this is the order of 8 bits of the quality band
# (07)(06)(05)(04)(03)(02)(01)(00) - MODIS NOMENCLATURE
# (01)(02)(03)(04)(05)(06)(07)(08) - R NOMENCLATURE
#'
qaFilter <- function(band, qaband, type, filter) {
  if (type == "mxd11a2") {
    dataBIN <- sprintf("%08d", DecToBin(1:255) %>% as.numeric())
    df.bin <- tibble(bin = dataBIN) %>%
      mutate(dec = 1:n()) %>%
      filter(
        str_sub(bin, 7, 8) %in% filter[[1]] | # Mandatory QA flags
          str_sub(bin, 5, 6) %in% filter[[2]] | # Data quality flag
          str_sub(bin, 3, 4) %in% filter[[3]] | # Emiss Error flag
          str_sub(bin, 1, 2) %in% filter[[4]] #  LST Error flag
      )
  }
  #' changing the values of the quality band to NA and 1
  qaband[is.na(qaband)] <- 256
  qaband[qaband %in% df.bin$dec] <- NA
  qaband[!is.na(qaband)] <- 1
  return(band * qaband)
}

#' this function extrats average value of raster by polygon vector
extract_data <- function(file, st) {
  return(file %>% mask(st) %>% getValues() %>% mean(na.rm = T))
}

#' this function return a logic value if it is an outlier vlaue or no
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}