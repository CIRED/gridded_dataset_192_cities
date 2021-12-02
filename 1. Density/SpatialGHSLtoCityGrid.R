###############################################################################
# AGGREGATE GHSL SPATIAL DATA IN CITY GRID
###############################################################################

# Clean workspace
rm(list = ls())
gc()

print('***** PREPARING GIS DATA FOR GHSL *****\n')
print(' ')
print('-> Loading libraries...')
print(' ')

# Loading libraries
library(tidyverse)
library(sf)
library(raster)
library(tcltk)

# Setting working directory
setwd('/home/quentin/Documents/City_dataStudy')

# Importing R files
source('./R_Script/ProcessingTools/Aggregate_GHSLToGrid/ProcessGHSLData.R');
source('./R_Script/ProcessingTools/Aggregate_GHSLToGrid/gdal_polygonizeR.R');

# Project caracteristics
countryName = 'Turkey';
cityName = 'Izmir';
EPSG = '32635';
ghslYear = list('1975', '1990', '2000', '2015');

pathToGrid = paste('./Data/', countryName, '/', cityName, '/Grid/grille_', toupper(cityName), '_finale_', EPSG, sep = '');

# World database EPSG
databaseEPSG = '54009'

# Load city grid shp
gridShp = st_read(paste(pathToGrid, '.shp', sep = ''));


###############################################################################
# LOOPS OVER GHSL YEARS
###############################################################################

for(year in ghslYear)
{
  # Screen output
  print(paste('-> Processing GIS data for ', cityName, ' in ', year, sep = ''));
  
  
  ###############################################################################
  # Clip, reproject and polygonize raster
  ###############################################################################
  
  print('     >> Clipping and reprojecting rasters...');
  
  # World GHSL raster paths
  worldDatabasePath = paste('/home/quentin/Documents/NEDUM/data_World/GHSL/Population_density/GHS_POP_GPW4', year, '_GLOBE_R2015A_54009_250_v1_0/', sep = '');
  worldDatabaseRaster = paste('GHS_POP_GPW4', year, '_GLOBE_R2015A_54009_250_v1_0', '.tif', sep = '');
  
  # Path to local GHSL folder
  pathToGHSL = paste('./Data/', countryName, '/', cityName, '/Population_Density/', sep = '');
  
  # Check if folder already exists
  if(dir.exists(paste(pathToGHSL, '/', year, sep = '')))
  {
    # Screen output
    print(paste('        Folder for year ', year, ' already exists', sep = ''));
    # User input
    bErase = readline(prompt = '        Do you want to erase it ? (Y/N) ');
    
    if(tolower(bErase) == 'y')
    {
      # Erase folder
      unlink(paste(pathToGHSL, sep = ''), recursive = TRUE);
      print('        Folder erased');
      # Create a new one
      dir.create(paste(pathToGHSL, sep = ''));
      print('        Folder recreated');
    }
    else if(tolower(bErase) == 'n')
    {
      # Keep folder and skip to next year
      print('        Folder not erased. Skipping to next year')
      next;
    }
    else
    {
      # Error
      stop('        /!\ Answer must be Y or N');
    }
  }
  else
  {
    # Create a new one
    dir.create(paste(pathToGHSL, sep = ''));
    print('        Folder created');
  }
  
  # Load world GHSL raster
  worldGHSLRaster = raster(paste(worldDatabasePath, worldDatabaseRaster, sep = ''));
  
  # Get raster's CRS
  worldDBCrs = crs(worldGHSLRaster)@projargs;
  
  # Change grid's CRS to get a mask layer
  print('        Getting mask layer...')
  maskShp = st_transform(gridShp, crs = worldDBCrs);
  # Convert to Spatial polygons dataframe
  maskShp = as(maskShp, 'Spatial')
  
  # Clip raster
  print('        Clipping world raster...');
  croppedLayer = raster::crop(worldGHSLRaster, maskShp);
  clippedTmp = raster::mask(croppedLayer, maskShp);
  
  # Remove world raster and mask layer
  rm(worldGHSLRaster, maskShp, croppedLayer);
  
  # Polygonize raster
  print('        Polygonizing raster...')
  # ghslShpTmp = rasterToPolygons(clippedTmp, dissolve = TRUE);
  ghslShpTmp = gdal_polygonizeR(clippedTmp);
  names(ghslShpTmp)[1] = 'population';
  ghslShpTmp = st_as_sf(ghslShpTmp);
  
  # Reprojecting layer
  print('        Reprojecting layer...')
  gridCrs = crs(gridShp);
  ghslShp = st_transform(ghslShpTmp, crs = gridCrs);
  
  # Compute area and add to attributes
  print('        Computing GHSL area attribute...'); 
  ghslShp$areaGHSL = st_area(ghslShp);
  meanPixelArea = ghslShp %>% filter(as.numeric(areaGHSL) > 60000 & as.numeric(areaGHSL) < 65000) %>%
    summarise(area = as.numeric(mean(areaGHSL))) %>%
    dplyr::select(area)
  # Update population
  ghslShp$population = ghslShp$population * (as.numeric(ghslShp$areaGHSL) / meanPixelArea$area);
  # Take off null values
  ghslShp = ghslShp %>% filter(population > 0);
  
  # Free memory
  rm(clippedTmp, ghslShpTmp);
  
  
  ###############################################################################
  # Intersect grid and ghsl Layer
  ###############################################################################
  
  print('     >> Intersecting grid and GHSL layer...')
  
  # Process intersection
  gridXghslShp = st_intersection(gridShp, st_buffer(ghslShp, 0));
  # Adds area
  gridXghslShp$areaX = st_area(gridXghslShp);
  print('        Intersection completed')
  
  # Free memory
  rm(ghslShp);
  
  
  ###############################################################################
  # Process intersection fields and file
  ###############################################################################
  
  print('     >> Processing grid and GHSL layer...')
  
  gridGHSLSf = AggregateGHSLDataToGrid_V3(gridShp, gridXghslShp, year, countryName, cityName);
  
  # Save Sf to shp and csv
  st_write(obj = gridGHSLSf,
           dsn = paste(pathToGHSL, 'grille_GHSL_density_', year, '_', toupper(cityName), '.shp', sep = ''),
           layer = paste('grille_GHSL_density_', year, '_', toupper(cityName), sep = ''),
           driver = "ESRI Shapefile",
           delete_dsn = TRUE);
  
  st_write(obj = gridGHSLSf,
           dsn = paste(pathToGHSL, 'grille_GHSL_density_', year, '_', toupper(cityName), '.csv', sep = ''),
           delete_dsn = TRUE);
}

# Message box for the end of the script
msgBox <- tkmessageBox(title = 'Aggregation script',
                       message = paste('Data successfully aggregated for ', cityName, ' - ', countryName, sep = ''),
                       icon = 'info',
                       type = 'ok')







