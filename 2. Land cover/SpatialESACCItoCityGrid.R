###############################################################################
# AGGREGATE ESACCI SPATIAL LAND COVER DATA IN CITY GRID
###############################################################################

# Clean workspace
rm(list = ls())
gc()

print('***** PREPARING GIS DATA FOR ESACCI *****\n')
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
source('./R_Script/ProcessingTools/Aggregate_ESACCIToGrid/ProcessESACCIData.R');
source('./R_Script/ProcessingTools/Aggregate_ESACCIToGrid/gdal_polygonizeR.R');

# Import city database
cityDatabaseDf = read.csv('./CityDatabases/cityDatabase.csv',
                          stringsAsFactors = FALSE)

cityDatabaseDf = cityDatabaseDf %>% filter(PopDensityYear == '2015' & TransportSource == 'Google') %>%
  distinct(City, Country, Continent, GridEPSG)

esacciYear = list('2015');


cityCount = 0;

# From MELBOURNE i = 239
cityDatabaseDf = cityDatabaseDf[239:281,]

# Initialize GUI progress bar
progressBar = tkProgressBar(title = paste('ESACCI Land Cover aggregation...' , sep = ''),
                            min = 0,
                            max = length(cityDatabaseDf$City),
                            width = 600);


###############################################################################
# LOOPS OVER CITIES
###############################################################################
for(i in (1:length(cityDatabaseDf$City)))
{
  # Get city
  cityDataDf = cityDatabaseDf[i, ];
  
  # Update progress bar
  cityCount = cityCount + 1;
  prevPercentage = (cityCount - 1) / length(cityDatabaseDf$City) * 100;
  maxStep = 12 * length(esacciYear);
  stepCount = 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Initialization - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  pathToGrid = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, sep = '');
  pathToLandCover = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Land_Cover/', sep = '');
  
  # Check if folder already exists
  if(dir.exists(pathToLandCover))
  {
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': Land Cover folder already exists - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  } else
  {
    # Create a new one
    dir.create(pathToLandCover);
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': Land Cover folder created - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  }
  
  # Load city grid shp
  gridShp = st_read(paste(pathToGrid, '.shp', sep = ''));
  
  ###############################################################################
  # LOOPS OVER ESACCI YEARS
  ###############################################################################
  for(year in esacciYear)
  {
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    # Update filename to current year
    fileName1 = paste('grid_ESACCI_LandCover_', year, '_', toupper(cityDataDf$City), '_',  cityDataDf$GridEPSG, sep = '');
    fileName2 = paste('gridUrb_ESACCI_LandCover_', year, '_', toupper(cityDataDf$City), '_',  cityDataDf$GridEPSG, sep = '');
    
    # Check if grid already exists
    if(file.exists(paste(pathToLandCover, '/', fileName1, '.csv', sep = '')))
    {
      setTkProgressBar(progressBar, cityCount, 
                       label = paste(cityDataDf$City, ': ', year, ' - Land Cover grid already exists. Erase ? - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
      # User input
      bErase = readline(prompt = '        Do you want to erase it ? (Y/N) ');
      
      if(tolower(bErase) == 'y')
      {
        # Erase file
        unlink(paste(pathToLandCover, '/', fileName1, '.csv', sep = ''));
        setTkProgressBar(progressBar, cityCount, 
                         label = paste(cityDataDf$City, ': ', year, ' - Grid erased - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
      } else if(tolower(bErase) == 'n')
      {
        setTkProgressBar(progressBar, cityCount, 
                         label = paste(cityDataDf$City, ': ', year, ' - Grid not erased - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
        next;
      } else
      {
        # Error
        stop('        /!\ Answer must be Y or N');
      }
    }
    

    ###############################################################################
    # Clip, reproject and polygonize raster
    ###############################################################################
    
    # World ESACCI raster paths
    worldDatabasePath = paste('/home/quentin/Documents/NEDUM/data_World/ESACCI_LandCover/', year, '_LandCover/', sep = '');
    worldDatabaseRaster = paste('ESACCI-LC-L4-LCCS-Map-300m-P1Y-', year, '-v2.0.7.tif', sep = '');
    
    # Load world ESACCI raster
    worldESACCIRaster = raster(paste(worldDatabasePath, worldDatabaseRaster, sep = ''));
    
    # Get raster's CRS
    worldDBCrs = crs(worldESACCIRaster)@projargs;
    
    # Change grid's CRS to get a mask layer
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Getting mask layer - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    maskShp = st_transform(gridShp, crs = worldDBCrs);
    # Convert to Spatial polygons dataframe
    maskShp = as(maskShp, 'Spatial')
    
    # Clip raster
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Clipping world raster - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    croppedLayer = raster::crop(worldESACCIRaster, maskShp);
    clippedTmp = raster::mask(croppedLayer, maskShp);
    
    # Remove world raster and mask layer
    rm(worldESACCIRaster, maskShp, croppedLayer);
    
    # Polygonize raster
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Polygonizing raster - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    esacciShpTmp = gdal_polygonizeR(clippedTmp);
    names(esacciShpTmp)[1] = 'cat';
    esacciShpTmp = st_as_sf(esacciShpTmp);
    
    # Reprojecting layer
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Reprojecting layer - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    gridCrs = crs(gridShp);
    esacciShp = st_transform(esacciShpTmp, crs = gridCrs);
    
    # Compute area and add to attributes
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Computing area - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    esacciShp$areaESACCI = st_area(esacciShp);
    
    # Free memory
    rm(clippedTmp, esacciShpTmp);
    
    
    ###############################################################################
    # Intersect grid and esacci Layer
    ###############################################################################
    
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Intersecting layer - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    # Process intersection
    gridXesacciShp = st_intersection(gridShp, st_buffer(esacciShp, 0));
    # Adds area
    gridXesacciShp$areaX = st_area(gridXesacciShp);
    
    # Free memory
    rm(esacciShp);
    
    
    ###############################################################################
    # Process intersection fields and file
    ###############################################################################
    
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Processing ESACCI data... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    gridESACCISf = AggregateESACCIDataToGrid(cityDataDf, gridShp, gridXesacciShp, year);
    
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Computing urbanization layer - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    # Possible to urbanize or not
    gridUrbSf = gridShp;
    # Possible to urbanize area
    gridUrbSf$OpenedToUrb = gridESACCISf$ESACCI10 + gridESACCISf$ESACCI11 + gridESACCISf$ESACCI12 + gridESACCISf$ESACCI14 + gridESACCISf$ESACCI20 + gridESACCISf$ESACCI30 +
      gridESACCISf$ESACCI40 + gridESACCISf$ESACCI110 + gridESACCISf$ESACCI120 + gridESACCISf$ESACCI121 + gridESACCISf$ESACCI122 + gridESACCISf$ESACCI130 +
      gridESACCISf$ESACCI140 + gridESACCISf$ESACCI150 + gridESACCISf$ESACCI151 + gridESACCISf$ESACCI152 + gridESACCISf$ESACCI153 + gridESACCISf$ESACCI190 +
      gridESACCISf$ESACCI200 + gridESACCISf$ESACCI201 + gridESACCISf$ESACCI202;
    
    gridUrbSf$ClosedToUrb = gridESACCISf$ESACCI50 + gridESACCISf$ESACCI60 + gridESACCISf$ESACCI61 + gridESACCISf$ESACCI62 + gridESACCISf$ESACCI70 + gridESACCISf$ESACCI71 +
      gridESACCISf$ESACCI72 + gridESACCISf$ESACCI80 + gridESACCISf$ESACCI81 + gridESACCISf$ESACCI82 + gridESACCISf$ESACCI90 + gridESACCISf$ESACCI100 +
      gridESACCISf$ESACCI160 + gridESACCISf$ESACCI170 + gridESACCISf$ESACCI180 + gridESACCISf$ESACCI210 + gridESACCISf$ESACCI220 + gridESACCISf$ESACCI230;
    
    gridUrbSf$TotalArea = gridUrbSf$OpenedToUrb + gridUrbSf$ClosedToUrb;
    
    stepCount = stepCount + 1;
    setTkProgressBar(progressBar, cityCount, 
                     label = paste(cityDataDf$City, ': ', year, ' - Saving files - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
    
    # Save Sf to shp and csv
    st_write(obj = gridUrbSf,
             dsn = paste(pathToLandCover, fileName2, '.shp', sep = ''),
             layer = paste(pathToLandCover, fileName2, sep = ''),
             driver = "ESRI Shapefile",
             delete_dsn = TRUE);
    
    st_write(obj = gridESACCISf,
             dsn = paste(pathToLandCover, fileName1, '.csv', sep = ''),
             delete_dsn = TRUE);
    
    st_write(obj = gridUrbSf,
             dsn = paste(pathToLandCover, fileName2, '.csv', sep = ''),
             delete_dsn = TRUE);
  }
}

close(progressBar)

# Message box for the end of the script
msgBox <- tkmessageBox(title = 'ESACCI Land Cover aggregation script',
                       message = 'Script successfully completed',
                       icon = 'info',
                       type = 'ok')







