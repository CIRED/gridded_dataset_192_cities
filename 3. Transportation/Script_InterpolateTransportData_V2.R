# ------------------------------------------------------------------------------------------------------------------ #
# CONCATENATE AND INTERPOLATE TRANSPORT DATA
# ------------------------------------------------------------------------------------------------------------------ #

# Clean workspace
rm(list = ls())
gc()

# Import libraries
library(tidyverse)
library(plotly)
library(dplyr)
library(ggplot2)
library(moments)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(akima)
library(rgdal)
library(tcltk)
library(sf)
library(raster)

setwd('/home/quentin/Documents/City_dataStudy');

# Import files
source('./R_Script/ProcessingTools/DataTools.R')
source('./R_Script/ProcessingTools/PlottingTools.R')
source('./R_Script/ProcessingTools/gdal_polygonizeR.R')

# Import city database
cityDatabaseDf = read.csv('./CityDatabases/cityDatabase_221NewCities.csv',
                          stringsAsFactors = FALSE)

cityDatabaseDf = cityDatabaseDf %>% filter(TransportSource == '') %>%
  distinct(City, Country, Continent, GridEPSG, RushHour)

# Transport mode (Driving / Transit)
transportMode = 'Transit';
# Keep data up to this threshold (seconds)
durationThreshold = 24000;


# --------------------------------------------------------------------------------------------------------------------- #
# Load transport data and gather it inside a single dataframe
# --------------------------------------------------------------------------------------------------------------------- #

LoadAndConcatenateData = function(cityDataDf, transportMode)
{
  # Find all files in city directory
  filesListDf = as.data.frame(list.files(paste('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country, '/', cityDataDf$City, '/', sep = '')));
  names(filesListDf)[1] = 'Files';
  
  # Keep only files which contains transport mode and Google source, that are .csv
  filesListDf = filesListDf %>% filter(grepl(paste(transportMode, 'TimesGoogle', sep = ''), Files)) %>%
    filter(grepl('.csv', Files)) %>%
    filter(!grepl('RANDOMV1', Files)) %>%
    filter(!grepl('_90_V1', Files))
  
  if(transportMode == 'Driving')
  {
    transportDataDf = data.frame(matrix(nrow = 0, ncol = 6));
    colnames(transportDataDf) = c('X', 'Y', 'distance', 'duration', 'durationInTraffic', 'status');
  } else
  {
    transportDataDf = data.frame(matrix(nrow = 0, ncol = 5));
    colnames(transportDataDf) = c('X', 'Y', 'distance', 'duration', 'status');
  }
  
  # Loop over found files
  for(file in filesListDf$Files)
  {
    trTmpDf = read.csv(paste('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country, '/', cityDataDf$City, '/',
                            file,
                            sep = ''),
                       stringsAsFactors = FALSE);
    
    transportDataDf = rbind(transportDataDf, trTmpDf);
    rm(trTmpDf);
  }
  
  return(transportDataDf)
}


# --------------------------------------------------------------------------------------------------------------------- #
# Function to remove point inside water area
# --------------------------------------------------------------------------------------------------------------------- #

RemovePointFromWater = function(cityDataDf, transportDataDf)
{
  # Import and crop raster data (ESACCI - 2015)
  # Import grid
  gridSf = st_read(dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, '.shp',
                               sep = ''))
  # Convert to 4326
  gridSf = st_transform(gridSf, 4326);
  
  # Get grid extension
  gridBbox = st_bbox(gridSf)
  minLng = as.numeric(gridBbox$xmin - 0.05)
  maxLng = as.numeric(gridBbox$xmax + 0.05)
  minLat = as.numeric(gridBbox$ymin - 0.05)
  maxLat = as.numeric(gridBbox$ymax + 0.05)
  
  # Create extent sp polygon
  xExt = c(minLng - 0.2 * (maxLng - minLng), minLng - 0.2 * (maxLng - minLng), maxLng - 0.2 * (maxLng - minLng), maxLng + 0.2 * (maxLng - minLng))
  yExt = c(minLat - 0.2 * (maxLat - minLat), maxLat + 0.2 * (maxLat - minLat), maxLat + 0.2 * (maxLat - minLat), minLat - 0.2 * (maxLat - minLat))
  coordExt = cbind(xExt, yExt)
  extentSp = Polygon(coordExt)
  extentSp = Polygons(list(extentSp), 1)
  extentSp = SpatialPolygons(list(extentSp))
  proj4string(extentSp) = CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # Import raster
  landCoverRaster = raster('/home/quentin/Documents/NEDUM/data_World/ESACCI_LandCover/2015_LandCover/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.tif')
  # Reproject extent
  extentSp = spTransform(extentSp, crs(landCoverRaster))
  # Crop it
  croppedRaster = raster::crop(x = landCoverRaster, y = extentSp)
  localLandCoverRaster = raster::mask(croppedRaster, extentSp);
  # Reproject raster
  utmCrs = st_crs(paste('+init=epsg:', as.character(cityDataDf$GridEPSG), sep = ''))
  localLandCoverRaster = projectRaster(localLandCoverRaster, crs = utmCrs$proj4string)
  # Polygonize raster
  localLandCoverSf = gdal_polygonizeR(localLandCoverRaster);
  names(localLandCoverSf)[1] = 'cat';
  localLandCoverSf = st_as_sf(localLandCoverSf);
  
  # Clear memory
  rm(landCoverRaster, extentSp, croppedRaster, localLandCoverRaster)
  
  # Keep only water area
  waterAreaSf = localLandCoverSf %>% filter(cat >= 209 & cat <= 211)
  # Convert transport data to sf
  transportDataSf = st_as_sf(transportDataDf, coords = c('X', 'Y'), crs = cityDataDf$GridEPSG);
  
  # Intersect sf
  intersection = st_intersection(st_buffer(waterAreaSf, 0), transportDataSf)
  
  # Filter intersected IDs
  transportDataSf$Distance[intersection$ID] = NA;
  transportDataSf$Duration[intersection$ID] = NA;
  transportDataDf = transportDataSf %>% st_drop_geometry()
  
  return(transportDataDf)
}


#########################################################################################################################
# MAIN SCRIPT
#########################################################################################################################

# Initialize GUI progress bar
progressBar = tkProgressBar(title = 'Interpolating transport data...',
                            min = 0,
                            max = nrow(cityDatabaseDf),
                            width = 300);

barCount = 0;

# Loop over cities
for(i in (1:length(cityDatabaseDf$City)))
{
  # Get city
  cityDataDf = cityDatabaseDf[i, ];
  
  # Update bar count
  barCount = barCount + 1;
  
  # Update progress bar
  setTkProgressBar(progressBar, barCount, label = paste(round((barCount/nrow(cityDatabaseDf)) * 100, 1), '% completed [', cityDataDf$City, ']', sep = ''));
  
  # Import grid
  gridDf = read.csv(paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale.csv', sep = ''),
                    sep = ',',
                    stringsAsFactors = FALSE)
  
  # Concatenate and import transport data
  transportDataDf = LoadAndConcatenateData(cityDataDf, transportMode);
  # Save raw transport data in csv
  write.csv(transportDataDf,
            paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Transport/raw', 
                  transportMode, 'TimesGoogle_', cityDataDf$City, '_', cityDataDf$RushHour, '_', as.character(cityDataDf$GridEPSG), '.csv', sep = ''),
            row.names = FALSE)
  
  # Erase NA rows
  transportDataDf = na.omit(transportDataDf);
  # # Replace NA rows by high values
  # transportDataDf = transportDataDf %>% mutate(duration = replace(duration, is.na(duration), 50000),
  #                                              distance = replace(distance, is.na(distance), 1000000))
  # 
  # if(transportMode == 'Driving')
  # {
  #   transportDataDf = transportDataDf %>% mutate(durationInTraffic = replace(durationInTraffic, is.na(durationInTraffic), 50000))
  # }
  
  # If is empty transportDataDf
  if(nrow(transportDataDf) == 0)
  {
    # Create a dataframe with NA
    interpTransportDataDf = data.frame(ID = gridDf$ID,
                                       X = gridDf$XCOORD,
                                       Y = gridDf$YCOORD,
                                       Duration = NA,
                                       Distance = NA);
  } else
  {
    # Interpolation
    
    # Distance
    interpDistance = interpp(x = transportDataDf$X, y = transportDataDf$Y, z = transportDataDf$distance, xo = gridDf$XCOORD, yo = gridDf$YCOORD)
    interpDistanceDf = as.data.frame(interpDistance)
    
    # Duration and binding both
    if(transportMode == 'Driving')
    {
      transportDataDf = transportDataDf %>% filter(durationInTraffic <= durationThreshold)
      interpTransportData = interpp(x = transportDataDf$X, y = transportDataDf$Y, z = transportDataDf$durationInTraffic, xo = gridDf$XCOORD, yo = gridDf$YCOORD)
    } else
    {
      transportDataDf = transportDataDf %>% filter(duration <= durationThreshold);
      interpTransportData = interpp(x = transportDataDf$X, y = transportDataDf$Y, z = transportDataDf$duration, xo = gridDf$XCOORD, yo = gridDf$YCOORD);
    }
    
    interpTransportDataDf = as.data.frame(interpTransportData)
    
    interpTransportDataDf = cbind(interpTransportDataDf, interpDistanceDf$z)
    interpTransportDataDf = cbind(gridDf$ID, interpTransportDataDf)
    
    # Rename columns
    names(interpTransportDataDf)[1] = 'ID'
    names(interpTransportDataDf)[2] = 'X'
    names(interpTransportDataDf)[3] = 'Y'
    names(interpTransportDataDf)[4] = 'Duration'
    names(interpTransportDataDf)[5] = 'Distance'
  }
  
  # Remove point inside water area
  interpTransportDataDf = RemovePointFromWater(cityDataDf, interpTransportDataDf)
  
  # Erase useless elements
  rm(gridDf, transportDataDf, interpDistance, interpDistanceDf, interpTransportData)
  
  
  ####################
  ## SAVING RESULTS ##
  
  ######################
  # Join and save to shp
  # Load shp
  gridShp = readOGR(dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_',
                                as.character(cityDataDf$GridEPSG), '.shp', sep = ''),
                    layer = paste('grille_', toupper(cityDataDf$City), '_finale_', as.character(cityDataDf$GridEPSG), sep = ''))
  # Convert attributes from factor to numeric
  gridShp@data$ID = as.integer(levels(gridShp@data$ID))[gridShp@data$ID]
  
  # Copy and join GIS data
  gridJoinedGIS = gridShp
  gridJoinedGIS@data = suppressWarnings(left_join(gridJoinedGIS@data, interpTransportDataDf, by = "ID"))
  
  # Save shp
  writeOGR(obj = gridJoinedGIS,
           dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Transport/interp', 
                       transportMode, 'TimesGoogle_', cityDataDf$City, '_', cityDataDf$RushHour, '_', as.character(cityDataDf$GridEPSG), '.shp', sep = ''),
           layer = paste(transportMode, 'TimesGoogle_', cityDataDf$City, '_', cityDataDf$RushHour, '_', as.character(cityDataDf$GridEPSG), sep = ''),
           driver = "ESRI Shapefile")
  
  # Save csv
  write.csv(gridJoinedGIS,
            paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Transport/interp', 
                  transportMode, 'TimesGoogle_', cityDataDf$City, '_', cityDataDf$RushHour, '_', as.character(cityDataDf$GridEPSG), '.csv', sep = ''),
            row.names = FALSE)
}

close(progressBar)



















