#' @title
#' Plot the seasonal evolution of Hot Day Frequency (HDF OF LAND SURFACE)
#'
#' @description
#' this script plots the seasonal evolution of Hot Day Frequency
#'   (HDF OF LAND SURFACE) by region (vectorial data)
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c("raster", "tidyverse", "sf", "Hmisc", "extrafont", "grid", "stringr")

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
library(raster)
library(tidyverse)
library(sf)
library(Hmisc)
library(extrafont)
library(grid)
library(stringr)

#' LOAD FUNCTIONS
# source("scripts/functions.R")

#' LOAD CONSTANTS
k.data <- "mod11a2"
# k.thrhld <- .9
k.dry.yr <- c(2005, 2010, 2016) # dry years
# link.rgns <- "data/vector/cluster_region.gpkg"
link.basin <- "data/vector/basins.gpkg"
# k.region <- c(6,8)
k.region <- "ucayali"
k.elev.thrshl <- 1500

#' CREATE DATE VECTOR
date <- Sys.Date()
year <- str_sub(date, 1, 4) %>% as.numeric()
month <- str_sub(date, 6, 7) %>% as.numeric()
day <- str_sub(date, 9, 10) %>% as.numeric()

for (i in 2000:year) {
  if (i == 2000) {
    k.prd <- c(
      seq(as.Date("2000-02-18"), as.Date("2000-02-29"), by = "8 day"),
      seq(as.Date("2000-03-06"), as.Date("2000-12-31"), by = "8 day")
    )
  }
  if (i >= 2001 & i <= year - 1) {
    k.prd <- c(
      k.prd,
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
    k.prd <- c(
      k.prd,
      seq(as.Date(sprintf("%s-01-01", i)),
        as.Date(sprintf("%s-%s-%s", i, month, day)),
        by = "8 day"
      )
    )
  }
  if (i == year & as.numeric(month) > 2) {
    k.prd <- c(
      k.prd,
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

#' READ LIST OF RASTER DATA
lst.data <- list.files(
  sprintf("data/raster/%s/withFILTER", k.data),
  pattern = "LST_Day", full.names = T
)

#' READ DEM
dem <- raster("data/raster/dem/srtm_500m.tif") %>%
  resample(raster(lst.data[1])) %>%
  "names<-"("dem")

#' READ VECTORIAL DATA
#'   load cluster region
# sf.region <- st_read(
#   dsn = link.rgns,
#   layer = "pam_k8_eucl_woPCA", quiet = T, as_tibble = T
# ) %>%
#   filter(gridcode %in% k.region) %>%
#   mutate(id = 1) %>%
#   group_by(id) %>%
#   summarise()
#'   load basin region
sf.region <- st_read(
  dsn = link.basin, layer = k.region, quiet = T, as_tibble = T
)

#' EXTRACT RASTER VALUES FROM VECTORIAL DATA
if (length(k.region) == 1) {
  file <- sprintf(
    "data/rdata/%1$s_avg_Day_clus_%2$s_andes.RData", k.data, k.region
  )
} else {
  file <- sprintf(
    "data/rdata/%1$s_avg_Day_clus_%2$s_andes.RData", k.data,
    str_flatten(k.region, collapse = "-")
  )
}

if (file.exists(file)) {
  load(file)

  if (length(vls.stk) < length(k.prd)) {
    strt <- length(vls.stk) + 1
    end <- length(k.prd)

    for (i in strt:end) {
      print(i)
      vls.day <- raster(lst.data[i]) %>%
        stack(dem) %>%
        crop(sf.region) %>%
        raster::mask(sf.region) %>%
        getValues() %>%
        as_tibble() %>%
        dplyr::filter(dem > k.elev.thrshl) %>%
        dplyr::select(-dem) %>%
        apply(2, FUN = function(x) mean(x, na.rm = T))

      if (i == strt) vls.mn <- vls.day else vls.mn <- c(vls.mn, vls.day)
    }

    vls.stk <- c(vls.stk, vls.mn)
    save(vls.stk, file = file)
  }
} else {
  for (i in length(lst.data) %>% seq_len()) {
    print(i)
    vls.day <- raster(lst.data[i]) %>%
      stack(dem) %>%
      crop(sf.region) %>%
      raster::mask(sf.region) %>%
      getValues() %>%
      as_tibble() %>%
      dplyr::filter(dem > k.elev.thrshl) %>%
      dplyr::select(-dem) %>%
      apply(2, FUN = function(x) mean(x, na.rm = T))

    if (i == 1) vls.stk <- vls.day else vls.stk <- c(vls.stk, vls.day)
  }

  save(vls.stk, file = file)
}

#' BUILD A DATAFRAME OF LST DATA AND LST DATE
#'   yearly data
# df.rgn.data <- tibble(
#   date = k.prd[1:length(vls.stk)],
#   values = (vls.stk * .02) + (-273.15)
# ) %>%
#   group_by(date = as.integer(str_sub(date, 1, 4))) %>%
#   summarise(values = mean(values, na.rm = T))

#' LOAD RDATA
load("data/rdata/mod11a2_avg_Day_clus_6-8.RData")
load("data/rdata/mod11a2_avg_Day_clus_8.RData")
load("data/rdata/mod11a2_avg_Day_clus_6.RData")

#' BUILD A DATAFRAME OF LST DATA AND LST DATE
#'   each 8 days
df.rgn.data <- tibble(
  date = k.prd[1:length(vls.stk)],
  values = (vls.stk * .02) + (-273.15)
) %>%
  mutate(month = str_sub(date, 6, 7)) %>%
  group_by(month) %>%
  mutate(
    #decil = quantile(values, .9, na.rm = T),
    decil = mean(values, na.rm = T) + sd(values, na.rm = T)
  ) %>% # k.thrhld
  ungroup() %>%
  mutate(
    hdf = ifelse(values >= decil, 1, 0),
    hdf = ifelse(is.na(hdf), 0, hdf)
  ) %>% # 26.02289
  filter(
    str_sub(date, 6, 7) %nin% c("01", "02")
  )

clus68 <- df.rgn.data$decil %>% unique()
clus8 <- df.rgn.data$decil %>% unique()
clus6 <- df.rgn.data$decil %>% unique()
x <- tibble(clus68, clus8, clus6)
save(x, file = "data/rdata/threshol_clus68.RData")

#' BUILD DATAFRAME WITH AVERAGE ACCUMULATED HDF
yr.str <- str_sub(first(k.prd), 1, 4) %>% as.numeric()
yr.end <- str_sub(last(k.prd), 1, 4) %>% as.numeric()
k.years <- yr.str:yr.end

for (i in k.years) {
  if (i == k.years[1]) {
    df.hdf.ac <- df.rgn.data %>%
      filter(substr(date, 1, 4) == i) %>%
      mutate(hdf.ac = cumsum(hdf)) %>%
      dplyr::select(hdf.ac)

    names(df.hdf.ac) <- sprintf("yr.%s", i)
  } else {
    df.hdf.ac <- df.hdf.ac %>%
      mutate(id = 1:n()) %>%
      left_join(
        df.rgn.data %>%
          filter(substr(date, 1, 4) == i) %>%
          mutate(hdf.ac = cumsum(hdf)) %>%
          dplyr::select(hdf.ac) %>%
          mutate(id = 1:n()),
        by = "id"
      ) %>%
      dplyr::select(-id)

    names(df.hdf.ac)[i - (k.years[1] - 1)] <- sprintf("yr.%s", i)
  }
}

#' DATAFRAME WITH AVERAGE ACCUMULATED HDF, DURING DRY YEARS
#'   AND NORMAL YEARS
df.hdf.ac.norm <- df.hdf.ac %>%
  dplyr::select(
    sprintf("yr.%s", k.years[k.years %nin% c(k.dry.yr, yr.end)])
  )

df.hdf <- df.hdf.ac %>%
  mutate(
    hdf.max = df.hdf.ac.norm %>% apply(1, max, na.rm = T),
    hdf.min = df.hdf.ac.norm %>% apply(1, min, na.rm = T),
    hdf.mean = df.hdf.ac.norm %>% apply(1, mean, na.rm = T),
    date = seq(as.Date("2020-03-06"), as.Date("2020-12-31"), by = "8 day")
  ) %>%
  dplyr::select(
    sprintf("yr.%s", c(k.dry.yr, yr.end)),
    hdf.max, hdf.min, hdf.mean, date
  ) %>%
  gather(key = "type", value = "value", -date, -hdf.max, -hdf.min)

#' DEFINE LABELS FOR LEGEND
lbls <- c(
  "average years under\nnormal conditions",
  "year 2005", "year 2010", "year 2016", sprintf("year %s", yr.end)
)

#' PLOT TEMPORAL EVOLUTION ACCUMULATED HDF
plt.hdf <- ggplot(df.hdf, aes(date, value, group = type)) +
  labs(
    title = "SEASONAL EVOLUTION OF\nHOT DAY FREQUENCY (HDF)",
    y = "accumulated from hot days"
  ) +
  geom_ribbon(
    aes(ymin = hdf.min, ymax = hdf.max),
    size = .2, fill = "gray", color = "black", alpha = .1
  ) +
  geom_line(aes(linetype = type, color = type, size = type)) +
  scale_linetype_manual(
    values = c("dashed", "solid", "solid", "solid", "solid"), labels = lbls
  ) +
  scale_color_manual(
    values = c("gray", "blue", "black", "green", "red"),
    labels = lbls
  ) +
  scale_size_manual(values = rep(.8, 5), labels = lbls) +
  scale_x_date(
    limits = c(as.Date("2020-03-01"), as.Date("2020-12-31")),
    breaks = seq(as.Date("2020-03-01"), as.Date("2020-12-31"), by = "1 month"),
    date_labels = "%b", expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    breaks = seq(0, 20, 2),
    limits = c(-.003, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  # annotation_ticks(
  #  sides = "l",
  #  ticklength = 1 * unit(0.1, "cm"),
  #  color = "black"
  # ) +
  # coord_cartesian(clip = "off") +
  theme_bw() +
  theme(
    legend.background = element_rect(fill = "white", color = "black", size = .3),
    legend.margin = margin(.25, 4, 6, 4),
    legend.key.width = unit(.35, "cm"),
    legend.key.height = unit(.25, "cm"),
    legend.position = c(0.25, 0.85),
    legend.title = element_blank(),
    legend.text = element_text(size = 8, family = "Source Sans Pro"),
    plot.title = element_text(size = 12, hjust = 1, family = "Source Sans Pro"),
    axis.text.x = element_text(
      size = 10, colour = "black", family = "Source Sans Pro",
      face = "bold", angle = 45, hjust = -.3, vjust = .1
    ),
    axis.text.y = element_text(
      size = 12, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      size = 12, face = "bold", family = "Source Sans Pro", color = "black"
    ),
    axis.ticks.x = element_line(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    panel.grid = element_blank(),
    panel.border = element_rect(size = .5, color = "black"),
    plot.margin = margin(1.5, .1, 1, 1, "cm"),
    axis.line.y = element_line(
      size = .5, color = "black"
    )
  )

ggsave(
  plot = plt.hdf,
  "exports/mod11a2_clus_6-8_hdf_from_2000_to_2020.png",
  width = 10, height = 12, units = "cm", dpi = 1000
)

ggsave(
  plot = plt.hdf,
  sprintf(
    "exports/%s_clus_%s_hdf_from_%s_to_%s.png",
    k.data, k.region, 2000, 2020
  ),
  width = 10, height = 12, units = "cm", dpi = 1000
)