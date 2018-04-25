# set working directory----------------------------------------------------------------------
RPROJ <- list(PROJHOME = normalizePath(getwd()))
attach(RPROJ)
rm(RPROJ)
setwd(PROJHOME)

# load libraries----------------------------------------------------------------------
library(lubridate)
library(stringr)
library(sp)
library(RCurl)
library(raster)
d_shp <- getData('GADM', country='DEu', level=1)

source("R/download-dwd-data.R")

# distance function ----------------------------------------------------------------------
minDist <- function(points, p) which.min(colSums((t(points) - p)^2))

# read file with coordinates of the sampling sites
coordinates <- read.table (file = "data/coordinates.csv",
                           row.names=1, header=TRUE, sep=";", fill=T)

eg<- dwd_down(dwd_var = "air_temperature",
         x_coordinates = c(9.000461),
         y_coordinates = c(50.13213),
         ids = c("A"),
         from_date = "2017-03-01",
         to_date = "2017-10-31")

write.table(eg[[1]], paste("output/", "air_temperature", "_GPS.csv"),sep=";")

# plot map with sampling sites-weather stations connected
png(paste("figs/", dwd_var, "_map.png"),width = 6, height=5, units = 'in', res = 1000)
plot(d_shp, main = paste(dwd_var))
points(gps_info_station[,2:3], pch = 19)
points(gps_info_station[,5:6], pch = 19, col = "red")

for(z in 1:nrow(gps_info_station)){
  cc <- Lines(list(Line(rbind(as.numeric(gps_info_station[z,2:3]),
                              as.numeric(gps_info_station[z,5:6])))), ID ="a")
  m.sl = SpatialLines(list(cc))
  plot(m.sl, add = T)
}
dev.off()


dwd(dwd_var = "solar")
dwd(dwd_var = "wind")
dwd(dwd_var = "precipitation")