# ################################################################################################################## #
# REMOVE OUTLIERS FROM REAL ESTATE RAW DATA
# ################################################################################################################## #


# -------------- #
# Initialization
# -------------- #

# Clean workspace
rm(list=ls())
gc()

# Import libraries
library(tidyverse)
library(sf)
library(tcltk)

setwd('/home/quentin/Documents/City_dataStudy');

# Import city database
cityDatabaseDf = read.csv('./CityDatabases/cityDatabase.csv',
                          stringsAsFactors = FALSE);

cityDatabaseDf = cityDatabaseDf %>% filter(TransactionSource != '') %>%
  distinct(City, Country, Continent, GridEPSG, TransactionType, TransactionSource, TransactionMonth, TransactionYear)


#############################################################################################################################################################################
# Remove outliers from data
#############################################################################################################################################################################

# Documentation :
# -> dataDf is a dataframe
# -> variable is a string with the variable's name to be considered for the outliers
# -> method is a string containing the method to be used : 'Boxplot' | 'Percentiles' | 'Hampel'

RemoveOutliers = function(dataDf, variable, method)
{
  # Select method
  switch(tolower(method),
         'boxplot' = {newDataDf = UseOutliersBoxplotMethod(dataDf, variable)},
         'percentiles' = {newDataDf = UseOutliersPercentilesMethod(dataDf, variable)},
         'hampel' = {newDataDf = UseOutliersHampelMethod(dataDf, variable)},
         'keep' = {return(dataDf)});
  
  # Remove outliers from dataframe
  variableCol = grep(variable, colnames(newDataDf));
  newDataDf[,variableCol][newDataDf$IsOutlier == TRUE] = NA;
  newDataDf = subset(newDataDf, select = -IsOutlier);
  
  return(newDataDf)
}


#############################################################################################################################################################################
# Find outliers using Boxplot method
#############################################################################################################################################################################

# Documentation :
# -> dataDf is a dataframe for one city, one transaction type, source and year (Basically one gridded data of rent / sale)
# -> variable is a string with the variable's name to be considered for the outliers

UseOutliersBoxplotMethod = function(dataDf, variable)
{
  # Initialize dataframe to return
  newDataDf = data.frame(matrix(ncol = length(dataDf) + 1, nrow = 0)); 
  columnNames = c(colnames(dataDf), 'IsOutlier');
  colnames(newDataDf) = columnNames
  rm(columnNames)
  
  dataDfTmp = dataDf;
  
  # If no row exit function
  if(nrow(dataDfTmp) == 0)
  {
    dataDfTmp = newDataDf;
    return(newDataDf);
  } else
  {
    dataDfTmp$IsOutlier = FALSE;
  }
  
  variableCol = grep(variable, colnames(dataDfTmp));
  outliersValues = boxplot.stats(dataDfTmp[,variableCol])$out;
  outliersIndex = which(dataDfTmp[,variableCol] %in% c(outliersValues));
  
  dataDfTmp$IsOutlier[outliersIndex] = TRUE;
  
  newDataDf = dataDfTmp;
  
  return(newDataDf)
}


#############################################################################################################################################################################
# Find outliers using Percentiles method
#############################################################################################################################################################################

# Documentation :
# -> dataDf is a dataframe for one city, one transaction type, source and year (Basically one gridded data of rent / sale)
# -> variable is a string with the variable's name to be considered for the outliers

UseOutliersPercentilesMethod = function(dataDf, variable)
{
  # Initialize dataframe to return
  newDataDf = data.frame(matrix(ncol = length(dataDf) + 1, nrow = 0)); 
  columnNames = c(colnames(dataDf), 'IsOutlier');
  colnames(newDataDf) = columnNames
  rm(columnNames)
  
  dataDfTmp = dataDf;
  
  # If no row exit function
  if(nrow(dataDfTmp) == 0)
  {
    dataDfTmp = newDataDf;
    return(newDataDf);
  } else
  {
    dataDfTmp$IsOutlier = FALSE;
  }
  
  variableCol = grep(variable, colnames(dataDfTmp));
  
  pInf = 0.025;
  pSup = 0.975;
  
  bInf = quantile(dataDfTmp[,variableCol], pInf, na.rm = TRUE);
  bSup = quantile(dataDfTmp[,variableCol], pSup, na.rm = TRUE);
  
  outliersIndex = which(dataDfTmp[,variableCol] < bInf | dataDfTmp[,variableCol] > bSup);
  
  dataDfTmp$IsOutlier[outliersIndex] = TRUE;
  
  newDataDf = dataDfTmp;
  
  return(newDataDf)
}


#############################################################################################################################################################################
# Find outliers using Hampel method
#############################################################################################################################################################################

# Documentation :
# -> dataDf is a dataframe for one city, one transaction type, source and year (Basically one gridded data of rent / sale)
# -> variable is a string with the variable's name to be considered for the outliers

UseOutliersHampelMethod = function(dataDf, variable)
{
  # Initialize dataframe to return
  newDataDf = data.frame(matrix(ncol = length(dataDf) + 1, nrow = 0)); 
  columnNames = c(colnames(dataDf), 'IsOutlier');
  colnames(newDataDf) = columnNames
  rm(columnNames)
  
  dataDfTmp = dataDf;
  
  # If no row exit function
  if(nrow(dataDfTmp) == 0)
  {
    dataDfTmp = newDataDf;
    return(newDataDf);
  } else
  {
    dataDfTmp$IsOutlier = FALSE;
  }
  
  variableCol = grep(variable, colnames(dataDfTmp));
  
  k = 3;
  
  bInf = median(dataDfTmp[,variableCol], na.rm = TRUE) - k * mad(dataDfTmp[,variableCol], na.rm = TRUE);
  bSup = median(dataDfTmp[,variableCol], na.rm = TRUE) + k * mad(dataDfTmp[,variableCol], na.rm = TRUE);
  
  outliersIndex = which(dataDfTmp[,variableCol] < bInf | dataDfTmp[,variableCol] > bSup);
  
  dataDfTmp$IsOutlier[outliersIndex] = TRUE;
  
  newDataDf = dataDfTmp;
  
  return(newDataDf)
}



# ----------- #
# MAIN SCRIPT #
# ----------- #


# Narrow city selection
# cityDatabaseDf = cityDatabaseDf %>% filter(City == '' & TransactionType == '' & TransactionYear == '')

# Parameters
# boxplot - percentiles - hampel - keep
outMethod = 'boxplot';

# Counting of cities
cityCount = 0;
# Maximum number of steps during the process
maxStep = 9;

# Initialize GUI progress bar
progressBar = tkProgressBar(title = paste('Remove outliers in Real Estate Dataset' , sep = ''),
                            min = 0,
                            max = length(cityDatabaseDf$City),
                            width = 600);

cityDatabaseDf = cityDatabaseDf %>% filter(Country == 'Colombia')


for(i in (1:length(cityDatabaseDf$City)))
{
  # Select current city
  cityDataDf = cityDatabaseDf[i,];
  
  # Update progress bar
  cityCount = cityCount + 1;
  prevPercentage = (cityCount - 1) / length(cityDatabaseDf$City) * 100;
  
  # Update progress bar
  stepCount = 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Initialization - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Path to data folder
  pathToData = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Real_Estate/RawData/', sep = '');
  
  # Data file name
  fileName = paste(cityDataDf$TransactionType, '_', cityDataDf$TransactionSource, '_', cityDataDf$TransactionMonth, cityDataDf$TransactionYear, '_',
                   toupper(cityDataDf$City), '_BRUT.csv', sep = '');
  
  
  pathToGrid = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, sep = '');
  
  
  # -------------------------------------
  # Update progress bar : Import raw data
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Importing raw data... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  rawDataDf = read.csv(paste(pathToData, fileName, sep = ''));
  
  
  # ---------------------------------
  # Update progress bar : Import grid
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Importing grid data... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  pathToGrid = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, sep = '');
  gridSf = st_read(paste(pathToGrid, '.shp', sep = ''));
  
  # Convert to 4326
  gridSf = st_transform(gridSf, 4326);
  
  # Get grid extension
  gridBbox = st_bbox(gridSf)
  minLng = as.numeric(gridBbox$xmin - 0.5)
  maxLng = as.numeric(gridBbox$xmax + 0.5)
  minLat = as.numeric(gridBbox$ymin - 0.5)
  maxLat = as.numeric(gridBbox$ymax + 0.5)
  
  
  # --------------------------------------------------------------
  # Update progress bar : Removing raw data outside of grid extent
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Removing raw data outside of grid extent... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  rawDataDf = rawDataDf %>% filter(wgsLong >= minLng & wgsLong <= maxLng) %>%
    filter(wgsLat >= minLat & wgsLat <= maxLat)
  
  
  # -------------------------------------------
  # Update progress bar : Remove Price outliers
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Removing price outliers... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  cleanedDataDf = RemoveOutliers(rawDataDf, 'Price', outMethod);
  
  
  # ------------------------------------------
  # Update progress bar : Remove Size outliers
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Removing size outliers... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  cleanedDataDf = RemoveOutliers(cleanedDataDf, 'Size', outMethod);
  
  
  # -------------------------------------------
  # Update progress bar : Remove Price outliers
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Removing price per sqm outliers... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  cleanedDataDf = RemoveOutliers(cleanedDataDf, cityDataDf$TransactionType, outMethod);
  
  
  # --------------------------------------
  # Update progress bar : Remove NA values
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Removing NA values... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  cleanedDataDf = cleanedDataDf %>% filter(!is.na(wgsLat) & !is.na(wgsLong)) %>%
    filter(!is.na(Price) & !is.na(Size) & !is.na(.[[5]])) %>%
    filter(Size >= 5) %>%
    filter(.[[5]] >= 0.5) %>%
    arrange(desc(.[[5]]));
  
  
  # ---------------------------------
  # Update progress bar : Saving file
  stepCount = stepCount + 1;
  setTkProgressBar(progressBar, cityCount,
                   label = paste(cityDataDf$City, ': Saving file... - ', round(prevPercentage + (1/length(cityDatabaseDf$City)) * (stepCount/maxStep) * 100, 1), '% completed', sep = ''));
  
  # Path to saving folder
  pathToSavingFolder = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Real_Estate/', sep = '');
  
  # Data result file name
  resFileName = paste(cityDataDf$TransactionType, '_', cityDataDf$TransactionSource, '_', cityDataDf$TransactionMonth, cityDataDf$TransactionYear, '_',
                      toupper(cityDataDf$City), '_', outMethod, 'Outliers.csv', sep = '');
  
  # Saving .csv
  write.csv(cleanedDataDf,
            paste(pathToSavingFolder, resFileName, sep = ''),
            row.names = FALSE);
}


close(progressBar)

# Message box for the end of the script
msgBox <- tkmessageBox(title = 'Outliers removing script',
                       message = 'Script successfully completed',
                       icon = 'info',
                       type = 'ok')














