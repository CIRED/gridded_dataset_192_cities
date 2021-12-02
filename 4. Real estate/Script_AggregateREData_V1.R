# ################################################################################################################## #
# AGGREGATE REAL ESTATE DATA
# ################################################################################################################## #


# -------------- #
# Initialization
# -------------- #

# Clean workspace
rm(list=ls())
gc()

# Import libraries
library(tidyverse)
library(rgdal)
library(sp)
library(sf)
library(tcltk)

setwd('C:/Users/charl/OneDrive/Bureau/City_dataStudy');

# Import city database
cityDatabaseDf = read.csv('./CityDatabases/cityDatabase.csv',
                          stringsAsFactors = FALSE);

cityDatabaseDf = cityDatabaseDf %>% filter(TransactionSource != '') %>%
  distinct(City, Country, Continent, GridEPSG, TransactionType, TransactionSource, TransactionMonth, TransactionYear)


######################################################################################################################################################################
# Function to aggregate in the grid
###################################################################################################################################################################### 

AggregateInGrid = function(transactionType, transactionData, grid, selectedCity)
{
  # Spatial join
  dataOverlay = over(transactionData, grid)
  dataOverlay = mutate(dataOverlay, transactionData$dataID)
  names(dataOverlay)[5] = 'dataID'
  dataOverlay = left_join(transactionData@data, dataOverlay, by = c('dataID' = 'dataID'))
  
  # Aggregation
  if(transactionType == 'Sale')
  {
    dataAggregate = dataOverlay %>% group_by(ID) %>%
      summarise(avgPrice = mean(Price),
                avgSize = mean(Size),
                avgSale = mean(Sale),
                medPrice = median(Price),
                medSize = median(Size),
                medSale = median(Sale),
                regSale = lm(Sale * Size ~ 0 + Size)$coeff,
                dataCount = n()) %>%
      arrange(ID)
  }
  else
  {
    dataAggregate = dataOverlay %>% group_by(ID) %>%
      summarise(avgPrice = mean(Price),
                avgSize = mean(Size),
                avgRent = mean(Rent),
                medPrice = median(Price),
                medSize = median(Size),
                medRent = median(Rent),
                regRent = lm(Rent * Size ~ 0 + Size)$coeff,
                dataCount = n()) %>%
      arrange(ID)
  }
  
  griddedData = grid
  griddedData@data = left_join(griddedData@data, dataAggregate, by = ('ID' = 'ID'))
  
  # Save shp
  if(!dir.exists(paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Real_Estate/GridData/', sep = '')))
  {
    dir.create(paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Real_Estate/GridData/', sep = ''))
  }
  
  return(griddedData)
}




# ----------- #
# MAIN SCRIPT
# ----------- #

# Select cities
# cityDatabaseDf = cityDatabaseDf %>% filter(City == 'Istanbul')

# Data outliers method : '', '_boxplotOutliers', '_hampelOutliers', '_percentilesOutliers'
outliersMethod = '_hampelOutliers';

# Counting of cities
cityCount = 0;
# Maximum number of steps during the process
maxStep = 5;

# Initialize GUI progress bar
progressBar = tkProgressBar(title = paste('Aggregate real estate data' , sep = ''),
                            min = 0,
                            max = length(cityDatabaseDf$City),
                            width = 600);

# cityDatabaseDf = cityDatabaseDf[362:length(cityDatabaseDf$City),];


# Loop over the selected cities
for(i in (1:length(cityDatabaseDf$City)))
{
  # Select current city
  cityDataDf = cityDatabaseDf[i,];
  
  # Update progress bar
  cityCount = cityCount + 1;
  prevPercentage = (cityCount - 1) / length(cityDatabaseDf$City) * 100;
  
  # ------------------------------------
  # Update progress bar : Initialization
  stepCount = 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Initialization - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Path to data folder
  pathToData = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Real_Estate/', sep = '');

  # Data file name
  fileName = paste(cityDataDf$TransactionType, '_', cityDataDf$TransactionSource, '_', cityDataDf$TransactionMonth, cityDataDf$TransactionYear, '_',
                   toupper(cityDataDf$City), outliersMethod, '.csv', sep = '');
  
  pathToGrid = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, sep = '');
  
  
  # -------------------------------------
  # Update progress bar : Import data
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Importing data... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Import grid shp
  gridShp = readOGR(dsn = paste(pathToGrid, '.shp', sep = ''),
                    layer = paste('grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, sep = ''));
  
  transactionDf = read.csv(paste(pathToData, fileName, sep = ''),
                           sep = ',',
                           stringsAsFactors = FALSE)
  
  # If transactionDf is empty
  if(nrow(transactionDf) == 0)
  {
    transactionDf[1,] = c(NA, NA, NA, NA ,NA);
  }
  
  dataID = seq(1, length(transactionDf[,5]))
  transactionDf = cbind(dataID, transactionDf)
  rm(dataID)
  
  
  # --------------------------------------------------
  # Update progress bar : Convert to shp and reproject
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Converting data to .shp and reprojecting... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Convert .csv to .shp in WGS84 projection
  transactionShp = SpatialPointsDataFrame(cbind(transactionDf[, 3], transactionDf[, 2]),
                                         transactionDf,
                                         proj4string = CRS('+init=epsg:4326'))

  # Reproject .shp in city's CRS
  transactionShp = spTransform(transactionShp, proj4string(gridShp))
  
  
  # -----------------------------------------
  # Update progress bar : Aggregating in grid
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Aggregating data in grid... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Aggregate points in grid
  griddedTransactionShp = AggregateInGrid(cityDataDf$TransactionType, transactionShp, gridShp, cityDataDf$City)
  griddedTransactionSf = st_as_sf(griddedTransactionShp);
  
  
  # ----------------------------------
  # Update progress bar : Saving files
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Saving files... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Save Sf to shp and csv
  st_write(obj = griddedTransactionSf,
           dsn = paste(pathToData, 'GridData/', 'gridded', cityDataDf$TransactionType, '_', cityDataDf$TransactionSource, '_', cityDataDf$TransactionMonth, 
                       cityDataDf$TransactionYear, '_', toupper(cityDataDf$City), outliersMethod, '.shp', sep = ''),
           layer = paste('gridded', cityDataDf$TransactionType, '_', cityDataDf$TransactionSource, '_', cityDataDf$TransactionMonth, 
                         cityDataDf$TransactionYear, '_', toupper(cityDataDf$City), outliersMethod, sep = ''),
           driver = "ESRI Shapefile",
           delete_dsn = TRUE);
  
  st_write(obj = griddedTransactionSf,
           dsn = paste(pathToData, 'GridData/', 'gridded', cityDataDf$TransactionType, '_', cityDataDf$TransactionSource, '_', cityDataDf$TransactionMonth, 
           cityDataDf$TransactionYear, '_', toupper(cityDataDf$City), outliersMethod, '.csv', sep = ''),
           delete_dsn = TRUE);
}

close(progressBar)

# Message box for the end of the script
msgBox <- tkmessageBox(title = 'Real estate data aggregation script',
                       message = 'Script successfully completed',
                       icon = 'info',
                       type = 'ok')
































