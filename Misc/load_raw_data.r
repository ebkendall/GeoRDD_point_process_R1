# This is an example of how we obtained the raw data. The crime and arrest 
# were provided to us, but the other information can be obtained online.
# Additionally, further data cleaning was involved to make many of these shapefiles
# work in our context. The final data used from these initial raw data sources 
# are already saved and exist in the /Data folders for each respective data analysis.

library(tidyverse)
library(sp)
library(sf)
library(rgdal)
library(maps)
library(ggplot2)
library(rgeos)
library(raster)
library(spdep)
library(spatstat)
library(geosphere)
library(spatial)
library(ggmap)
library(units)
library(cleangeo)
library(LearnGeom)

#Large Data Set
dataArr <- read.csv("../InfoFiles/IncidentLevelData/2019.09.17 Incident-Level Arrest and Census Data.csv")
dataOff <- read.csv("../InfoFiles/IncidentLevelData/2018.09.17 Incident-Level Offense & Census data.csv")
dataNegControl <- read.csv("../../311_Service_Requests_from_2010_to_Present.csv")

#****STILL NEED TO CLEAN FOR "JAIL HOUSE"****
dataArrF <- dataArr %>% 
  dplyr::filter(year >= 2010) %>% 
  dplyr::filter(jurisdiction_code == 0)

dataOffFiltered <- dataOff %>% 
  dplyr::filter(year >= 2010) %>% 
  dplyr::filter(jurisdiction_code == 0) %>% 
  dplyr::filter(law_cat_cd == "FELONY")

#Loading the precinct shapefile (CITE: How to map New York City using map function in R, hrbrmstr)
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nypp_19b.zip"
precinctFile <- basename(url)
if (!file.exists(precinctFile))
  download.file(url, precinctFile)
rawData <- unzip(precinctFile)
nyc <- readOGR(rawData[1], ogrListLayers(rawData[1])[1], stringsAsFactors=FALSE)
nyc <- clgeo_Clean(nyc)

#Loading the Census (water included) shapefile (CITE: How to map New York City using map function in R, hrbrmstr)
url3 <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2010wi_19c.zip"
censusFile2 <- basename(url3)
if (!file.exists(censusFile2))
  download.file(url3, censusFile2)
rawData3 <- unzip(censusFile2)
censusNYCWater <- readOGR(rawData3[1], ogrListLayers(rawData3[1])[1], stringsAsFactors=FALSE)
censusNYCWater <- clgeo_Clean(censusNYCWater)

#Loading NYC Street Centerline (CSCL) data
url5 <- "https://data.cityofnewyork.us/api/geospatial/exjm-f27b?method=export&format=Shapefile"
streetFile <- basename(url5)
if (!file.exists(streetFile))
  download.file(url5, streetFile)
rawData5 <- unzip(streetFile)
streetNYC <- readOGR(rawData5[1], ogrListLayers(rawData5[1])[1], stringsAsFactors=FALSE)
streetNYC <- spTransform(streetNYC, nyc@proj4string)
streetNYC@data$precList <- streetANDprecint

#Census List
tractPop <- read.csv("tractPopulations.csv")
burrCode <- data.frame(censusNYCWater@data$BoroCT2010)
names(tractPop) <- c("burrow", "tract", "pop", "burTract")
names(burrCode) <- c("burTract")
  #order tractPop in the order of burrCode
tractPop <- tractPop %>% 
  slice(match(burrCode$burTract, burTract))
tractPop <- add_row(tractPop, burrow = 2, tract = 100, 
                    pop = as.factor(11091), burTract = 2000100, .before = 1684)
tractPop$pop <- as.character(tractPop$pop)
tractPop$pop <- as.numeric(gsub(",", "", tractPop$pop))
censusNYCWater@data$POP <- tractPop$pop
