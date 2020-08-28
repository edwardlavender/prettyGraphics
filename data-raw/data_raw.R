###############################
###############################
#### data_raw.R

#### This code:
# 1) Loads and add raw data to the package.

#### Load data
rm(list = ls())
list.files()
setwd("~/Documents/PhD/Academic_PhD_Work/Data and Modelling/Scripts/Utils/prettyGraphics/prettyGraphics/data-raw/")
load("dat_flapper.rda")
load("dat_coast_around_oban.rda")
gebco <- raster::raster("dat_gebco.tif")

#### Process raster to force source to 'memory' for correct usage
dat_gebco <- raster(gebco)
dat_gebco[] <- gebco[]
raster::plot(dat_gebco)
dat_gebco

#### Use data
usethis::use_data(dat_flapper, overwrite = TRUE)
usethis::use_data(dat_coast_around_oban, overwrite = TRUE)
usethis::use_data(dat_gebco, overwrite = TRUE)
