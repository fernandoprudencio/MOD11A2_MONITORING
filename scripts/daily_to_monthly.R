rm(list = ls())

library(tidyverse)
library(Hmisc)
library(raster)
library(ncdf4)
library(tmap)
library(cptcity)

#' CREATE DATE VECTOR
date <- Sys.Date()
year <- str_sub(date, 1, 4) %>% as.numeric()
month <- str_sub(date, 6, 7) %>% as.numeric()
day <- str_sub(date, 9, 10) %>% as.numeric()

for (i in 2000:year) {
  if (i == 2000) {
    ts <- c(
      seq(as.Date("2000-02-18"), as.Date("2000-02-29"), by = "8 day"),
      seq(as.Date("2000-03-06"), as.Date("2000-12-31"), by = "8 day")
    )
  }
  if (i >= 2001 & i <= year - 1) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
          as.Date(sprintf("%s-02-28", i)),
          by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
          as.Date(sprintf("%s-12-31", i)),
          by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) <= 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
          as.Date(sprintf("%s-%s-%s", i, month, day)),
          by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) > 2) {
    ts <- c(
      ts,
      seq(as.Date(sprintf("%s-01-01", i)),
          as.Date(sprintf("%s-02-28", i)),
          by = "8 day"
      ),
      seq(as.Date(sprintf("%s-03-06", i)),
          as.Date(sprintf("%s-%s-%s", i, month, day)),
          by = "8 day"
      )
    )
  }
}

ls.files <- list.files("data/raster/mod11a2/withFILTER", "Night_1km", full.names = T)
df <- tibble(date = ts[1:length(ls.files)]) %>%
  mutate(id = 1:n())

rut.out <- "data/raster/mod11a2/monthly"

for (i in 2001:2019) {
  print(i)
  df.yr <- df %>% filter(str_sub(date, 1 , 4) == i)
  name <- ls.files[df.yr$id[1]] %>% basename() %>% str_sub(1, -17)
  for (j in sprintf("%02d", 1:12)) {
    print(j)
    df.month <- df.yr %>% filter(str_sub(date, 6 , 7) == j)
    img <- stack(ls.files[df.month$id]) %>% mean(na.rm = T)
    writeRaster(
      img, sprintf("%1$s/%2$s%3$s.tif", rut.out, name, j),
      overwrite = T, datatype = "INT2S"
    )
  }
}


df.yr <- df %>% filter(str_sub(date, 1, 4) == 2000)
name <- ls.files[df.yr$id[1]] %>%
  basename() %>%
  str_sub(1, -17)
for (j in sprintf("%02d", 2:12)) {
  print(j)
  df.month <- df.yr %>% filter(str_sub(date, 6, 7) == j)
  img <- stack(ls.files[df.month$id]) %>% mean(na.rm = T)
  writeRaster(
    img, sprintf("%1$s/%2$s%3$s.tif", rut.out, name, j),
    overwrite = T, datatype = "INT2S"
  )
}