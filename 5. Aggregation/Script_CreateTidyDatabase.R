# ################################################################################################################## #
# CREATE TIDY DATABASE       
# ################################################################################################################## #
#6 STEPS TO ADD RENTS VAR

# -------------- #
# Initialization
# -------------- #

# Clean workspace
rm(list=ls())

# Import libraries
library(tidyverse)
library(reader)
library(tcltk)

setwd('C:/Users/charl/OneDrive/Bureau/City_dataStudy');


# --------------------------------- #
# Import the cityDatabase dataframe
# --------------------------------- #

cityDatabaseDf = read.csv('./CityDatabases/cityDatabase.csv',
                          stringsAsFactors = FALSE);


list_cities = unique(cityDatabaseDf$City[cityDatabaseDf$TransactionType == "Rent"])

cityDatabaseDf = cityDatabaseDf[cityDatabaseDf$City %in% list_cities, ]

# ---------------------------------------------------------------------------------------------------------- #
# Import average data and create a tidy database for a given city for one year and one transport source
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> cityDatabaseDf is a dataframe containing the caracteristics of the city

ImportAvgTidyData = function(cityDatabaseDf)
{
  # Check if there is only a single city in cityDatabaseDf
  if(nrow(cityDatabaseDf) != 1)
  {
    head(cityDatabaseDf);
    stop('More than one row in cityDatabaseDf')
  }
  
  # Format strings in case it would not be already
  cityDatabaseDf$Continent = gsub(' ', '_', cityDatabaseDf$Continent);
  cityDatabaseDf$Country = gsub(' ', '_', cityDatabaseDf$Country);
  cityDatabaseDf$City = gsub(' ', '_', cityDatabaseDf$City);
  
  # Grid data
  gridDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Grid/grille_', toupper(cityDatabaseDf$City), '_finale.csv', sep = ''),
                    stringsAsFactors = FALSE)
  # Sort ascending
  gridDf = gridDf %>% arrange(ID)
  
  centerDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Grid/Centre_', toupper(cityDatabaseDf$City), '_final.csv', sep = ''),
                      stringsAsFactors = FALSE)
  
  # Transport data
  drivingTimesDf = ImportTransportData(cityDatabaseDf = cityDatabaseDf,
                                       gridDf = gridDf,
                                       transportType = 'Driving');
  transitTimesDf = ImportTransportData(cityDatabaseDf = cityDatabaseDf,
                                       gridDf = gridDf,
                                       transportType = 'Transit');

  # Land cover data
  landCoverDf = ImportLandCoverData(cityDatabaseDf = cityDatabaseDf,
                                      gridDf = gridDf);
  
  landCoverDf$ClosedToUrb = (landCoverDf$ESACCI160 + landCoverDf$ESACCI170 + landCoverDf$ESACCI180 + landCoverDf$ESACCI210 + landCoverDf$ESACCI220) / landCoverDf$AREA
  landCoverDf$OpenedToUrb = 1 - landCoverDf$ClosedToUrb
  
  
  # Population density data
  popDensityDf = ImportPopDensityData(cityDatabaseDf = cityDatabaseDf,
                                      gridDf = gridDf);
  
  # Real estate data
  transactionDf = ImportTransactionData(cityDatabaseDf = cityDatabaseDf,
                                        gridDf = gridDf);

  # Fill dataframe    
  tidyDataDf = data.frame(ID = gridDf$ID,
                          X = gridDf$XCOORD,
                          Y = gridDf$YCOORD,
                          Area = gridDf$AREA,
                          City = cityDatabaseDf$City,
                          Country = cityDatabaseDf$Country,
                          Continent = cityDatabaseDf$Continent,
                          Currency = cityDatabaseDf$Currency,
                          GridEPSG = cityDatabaseDf$GridEPSG,
                          dCenter = sqrt((gridDf$XCOORD - centerDf$X)^2 + (gridDf$Y - centerDf$Y)^2),
                          TransactionType = cityDatabaseDf$TransactionType,
                          TransactionSource = cityDatabaseDf$TransactionSource,
                          TransactionMonth = cityDatabaseDf$TransactionMonth,
                          TransactionYear = cityDatabaseDf$TransactionYear,
                          avgSize = transactionDf$avgSize,
                          avgPriceSqm = transactionDf$avgPriceSqm,
                          medSize = transactionDf$medSize,
                          medPriceSqm = transactionDf$medPriceSqm,
                          regPriceSqm = transactionDf$regPriceSqm,
                          nRealEstateData = transactionDf$dataCount,
                          avgSize_boxplotOutliers = transactionDf$avgSize_boxplotOutliers,
                          avgPriceSqm_boxplotOutliers = transactionDf$avgPriceSqm_boxplotOutliers,
                          medSize_boxplotOutliers = transactionDf$medSize_boxplotOutliers,
                          medPriceSqm_boxplotOutliers = transactionDf$medPriceSqm_boxplotOutliers,
                          regPriceSqm_boxplotOutliers = transactionDf$regPriceSqm_boxplotOutliers,
                          nRealEstateData_boxplotOutliers = transactionDf$dataCount_boxplotOutliers,
                          avgSize_hampelOutliers = transactionDf$avgSize_hampelOutliers,
                          avgPriceSqm_hampelOutliers = transactionDf$avgPriceSqm_hampelOutliers,
                          medSize_hampelOutliers = transactionDf$medSize_hampelOutliers,
                          medPriceSqm_hampelOutliers = transactionDf$medPriceSqm_hampelOutliers,
                          regPriceSqm_hampelOutliers = transactionDf$regPriceSqm_hampelOutliers,
                          nRealEstateData_hampelOutliers = transactionDf$dataCount_hampelOutliers,
                          avgSize_percentilesOutliers = transactionDf$avgSize_percentilesOutliers,
                          avgPriceSqm_percentilesOutliers = transactionDf$avgPriceSqm_percentilesOutliers,
                          medSize_percentilesOutliers = transactionDf$medSize_percentilesOutliers,
                          medPriceSqm_percentilesOutliers = transactionDf$medPriceSqm_percentilesOutliers,
                          regPriceSqm_percentilesOutliers = transactionDf$regPriceSqm_percentilesOutliers,
                          nRealEstateData_percentilesOutliers = transactionDf$dataCount_percentilesOutliers,
                          TransportSource = cityDatabaseDf$TransportSource,
                          RushHour = cityDatabaseDf$RushHour,
                          TransportYear = cityDatabaseDf$TransportYear,
                          DistanceDriving = drivingTimesDf$Distance,
                          DurationDriving = drivingTimesDf$Duration,
                          DistanceTransit = transitTimesDf$Distance,
                          DurationTransit = transitTimesDf$Duration,
                          PopDensitySource = cityDatabaseDf$PopDensitySource,
                          PopDensityYear = cityDatabaseDf$PopDensityYear,
                          PopDensity = popDensityDf$PopDensity,
                          OpenedToUrb = landCoverDf$OpenedToUrb,
                          ClosedToUrb = landCoverDf$ClosedToUrb,
                          ESACCI10 = landCoverDf$ESACCI10,
                          ESACCI11 = landCoverDf$ESACCI11,
                          ESACCI12 = landCoverDf$ESACCI12,
                          ESACCI14 = landCoverDf$ESACCI14,
                          ESACCI20 = landCoverDf$ESACCI20,
                          ESACCI30 = landCoverDf$ESACCI30,
                          ESACCI40 = landCoverDf$ESACCI40,
                          ESACCI50 = landCoverDf$ESACCI50,
                          ESACCI60 = landCoverDf$ESACCI60,
                          ESACCI61 = landCoverDf$ESACCI61,
                          ESACCI62 = landCoverDf$ESACCI62,
                          ESACCI70 = landCoverDf$ESACCI70,
                          ESACCI71 = landCoverDf$ESACCI71,
                          ESACCI72 = landCoverDf$ESACCI72,
                          ESACCI80 = landCoverDf$ESACCI80,
                          ESACCI81 = landCoverDf$ESACCI81,
                          ESACCI82 = landCoverDf$ESACCI82,
                          ESACCI90 = landCoverDf$ESACCI90,
                          ESACCI100 = landCoverDf$ESACCI100,
                          ESACCI110 = landCoverDf$ESACCI110,
                          ESACCI120 = landCoverDf$ESACCI120,
                          ESACCI121 = landCoverDf$ESACCI121,
                          ESACCI122 = landCoverDf$ESACCI122,
                          ESACCI130 = landCoverDf$ESACCI130,
                          ESACCI140 = landCoverDf$ESACCI140,
                          ESACCI150 = landCoverDf$ESACCI150,
                          ESACCI151 = landCoverDf$ESACCI151,
                          ESACCI152 = landCoverDf$ESACCI152,
                          ESACCI153 = landCoverDf$ESACCI153,
                          ESACCI160 = landCoverDf$ESACCI160,
                          ESACCI170 = landCoverDf$ESACCI170,
                          ESACCI180 = landCoverDf$ESACCI180,
                          ESACCI190 = landCoverDf$ESACCI190,
                          ESACCI200 = landCoverDf$ESACCI200,
                          ESACCI201 = landCoverDf$ESACCI201,
                          ESACCI202 = landCoverDf$ESACCI202,
                          ESACCI210 = landCoverDf$ESACCI210,
                          ESACCI220 = landCoverDf$ESACCI220,
                          ESACCI230 = landCoverDf$ESACCI230);

  return(tidyDataDf)
}


# ---------------------------------------------------- #
# Import transport data of 'Driving' or 'Transit' type
# ---------------------------------------------------- #

# Documentation :
# -> cityDatabaseDf is a dataframe containing the caracteristics of the city (A single one)
# -> gridDf is a dataframe containing the grid
# -> transportType is a string with either 'Driving' or 'Transit'

ImportTransportData = function(cityDatabaseDf, gridDf, transportType)
{
  # Check if there is only a single city in cityDf
  if(nrow(cityDatabaseDf) != 1)
  {
    head(cityDatabaseDf);
    stop('More than one row in cityDatabaseDf')
  }
  
  # Check if there is a transport source
  if(cityDatabaseDf$TransportSource == '' | is.na(cityDatabaseDf$TransportSource))
  {
    transportTimesDf = gridDf;
    transportTimesDf$Duration = NA;
    transportTimesDf$Distance = NA;
    
    return(transportTimesDf)
  } else
  {
    # Import transport csv
    transportTimesDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Transport/interp', transportType, 'Times', 
                                      cityDatabaseDf$TransportSource, '_', cityDatabaseDf$City, '_', cityDatabaseDf$RushHour, '_', cityDatabaseDf$GridEPSG, '.csv', sep = ''),
                                sep = '\t',
                                stringsAsFactors = FALSE);
    # If separator is ','
    if(length(transportTimesDf) == 1)
    {
      transportTimesDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Transport/interp', transportType, 'Times', 
                                        cityDatabaseDf$TransportSource, '_', cityDatabaseDf$City, '_', cityDatabaseDf$RushHour, '_', cityDatabaseDf$GridEPSG, '.csv', sep = ''),
                                  sep = ',',
                                  stringsAsFactors = FALSE);
    }
    
    # Convert ID as integer and sort ascending
    transportTimesDf$ID = as.integer(transportTimesDf$ID);
    transportTimesDf = transportTimesDf %>% arrange(ID);
    
    return(transportTimesDf)
  }
}


# ------------------------------ #
# Import population density data
# ------------------------------ #

# Documentation :
# -> cityDatabaseDf is a dataframe containing the caracteristics of the city (A single one)
# -> gridDf is a dataframe containing the grid

ImportPopDensityData = function(cityDatabaseDf, gridDf)
{
  # Check if there is only a single city in cityDf
  if(nrow(cityDatabaseDf) != 1)
  {
    head(cityDatabaseDf);
    stop('More than one row in cityDatabaseDf')
  }
  
  # Check if there is a transport source
  if(cityDatabaseDf$PopDensitySource == '' | is.na(cityDatabaseDf$PopDensitySource))
  {
    popDensityDf = gridDf;
    popDensityDf$PopDensity = NA;
    
    return(transportTimesDf)
  } else
  {
    if(file.exists(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Population_Density/grille_', cityDatabaseDf$PopDensitySource,
                         '_density_', cityDatabaseDf$PopDensityYear, '_', toupper(cityDatabaseDf$City), '.txt', sep = '')))
    {
      fileExt = 'txt';
    } else
    {
      fileExt = '.csv';
    }
    
    delim = get.delim(fn = paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Population_Density/grille_', cityDatabaseDf$PopDensitySource,
                                 '_density_', cityDatabaseDf$PopDensityYear, '_', toupper(cityDatabaseDf$City), '.', fileExt, sep = ''));
    
    popDensityDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Population_Density/grille_', cityDatabaseDf$PopDensitySource,
                                  '_density_', cityDatabaseDf$PopDensityYear, '_', toupper(cityDatabaseDf$City), '.', fileExt, sep = ''),
                            stringsAsFactors = FALSE,
                            sep = delim);
    popDensityDf = popDensityDf %>% arrange(ID_pixel);
    
    # Change names
    colnames(popDensityDf) = c(colnames(gridDf), 'PopDensity');
    
    return(popDensityDf)
  }
}

# ------------------------------ #
# Import land cover data
# ------------------------------ #

# Documentation :
# -> cityDatabaseDf is a dataframe containing the caracteristics of the city (A single one)
# -> gridDf is a dataframe containing the grid

ImportLandCoverData = function(cityDatabaseDf, gridDf)
{
  # Check if there is only a single city in cityDf
  if(nrow(cityDatabaseDf) != 1)
  {
    head(cityDatabaseDf);
    stop('More than one row in cityDatabaseDf')
  }
  if(cityDatabaseDf$PopDensityYear != 2015)
  {
    landCoverDf = gridDf;
    landCoverDf$OpenedToUrb = NA;
    landCoverDf$ClosedToUrb = NA;
    landCoverDf$ESACCI10 = NA;
    landCoverDf$ESACCI11 = NA;
    landCoverDf$ESACCI12 = NA;
    landCoverDf$ESACCI14 = NA;
    landCoverDf$ESACCI20 = NA;
    landCoverDf$ESACCI30 = NA;
    landCoverDf$ESACCI40 = NA;
    landCoverDf$ESACCI50 = NA;
    landCoverDf$ESACCI60 = NA;
    landCoverDf$ESACCI61 = NA;
    landCoverDf$ESACCI62 = NA;
    landCoverDf$ESACCI70 = NA;
    landCoverDf$ESACCI71 = NA;
    landCoverDf$ESACCI72 = NA;
    landCoverDf$ESACCI80 = NA;
    landCoverDf$ESACCI81 = NA;
    landCoverDf$ESACCI82 = NA;
    landCoverDf$ESACCI90 = NA;
    landCoverDf$ESACCI100 = NA;
    landCoverDf$ESACCI110 = NA;
    landCoverDf$ESACCI120 = NA;
    landCoverDf$ESACCI121 = NA;
    landCoverDf$ESACCI122 = NA;
    landCoverDf$ESACCI130 = NA;
    landCoverDf$ESACCI140 = NA;
    landCoverDf$ESACCI150 = NA;
    landCoverDf$ESACCI151 = NA;
    landCoverDf$ESACCI152 = NA;
    landCoverDf$ESACCI153 = NA;
    landCoverDf$ESACCI160 = NA;
    landCoverDf$ESACCI170 = NA;
    landCoverDf$ESACCI180 = NA;
    landCoverDf$ESACCI190 = NA;
    landCoverDf$ESACCI200 = NA;
    landCoverDf$ESACCI201 = NA;
    landCoverDf$ESACCI202 = NA;
    landCoverDf$ESACCI210 = NA;
    landCoverDf$ESACCI220 = NA;
    landCoverDf$ESACCI230 = NA;
    
    return(landCoverDf)
  } else
  {
    landCoverDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Land_Cover/gridUrb_ESACCI_LandCover_2015_', toupper(cityDatabaseDf$City), '_', cityDatabaseDf$GridEPSG, '.csv', sep = ''),
                            stringsAsFactors = FALSE);
     landCoverDf = landCoverDf %>% arrange(ID);
 
    landCoverDf$OpenedToUrb = landCoverDf$OpenedToUrb / (landCoverDf$OpenedToUrb + landCoverDf$ClosedToUrb)
  landCoverDf$ClosedToUrb = 1 - landCoverDf$OpenedToUrb
 
   landCoverDf = landCoverDf[ , !(colnames(landCoverDf) %in% c("TotalArea"))]
 
  landCoverDf2 = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Land_Cover/grid_ESACCI_LandCover_2015_', toupper(cityDatabaseDf$City), '_', cityDatabaseDf$GridEPSG, '.csv', sep = ''),
                        stringsAsFactors = FALSE);
 
   landCoverDf2 = landCoverDf2[ , !(colnames(landCoverDf2) %in% c("XCOORD", "YCOORD", "AREA", "ESACCI_tot"))]
   landCoverDf = merge(landCoverDf, landCoverDf2, on = "ID")
   return(landCoverDf)
  }
}

# ----------------------------------- #
# Import real estate transaction data
# ----------------------------------- #

# Documentation :
# -> cityDatabaseDf is a dataframe containing the caracteristics of the city (A single one)
# -> gridDf is a dataframe containing the grid

ImportTransactionData = function(cityDatabaseDf, gridDf)
{
  # Check if there is only a single city in cityDf
  if(nrow(cityDatabaseDf) != 1)
  {
    head(cityDatabaseDf);
    stop('More than one row in cityDatabaseDf')
  }
  
  # Check if there is a transaction source
  if(cityDatabaseDf$TransactionSource == '' | is.na(cityDatabaseDf$TransactionSource))
  {
    transactionDf = gridDf;
    transactionDf$avgSize = NA;
    transactionDf$avgPriceSqm = NA;
    transactionDf$medSize = NA;
    transactionDf$medPriceSqm = NA;
    transactionDf$regPriceSqm = NA;
    transactionDf$dataCount = NA;
    transactionDf$avgSize_boxplotOutliers = NA;
    transactionDf$avgPriceSqm_boxplotOutliers = NA;
    transactionDf$medSize_boxplotOutliers = NA;
    transactionDf$medPriceSqm_boxplotOutliers = NA;
    transactionDf$regPriceSqm_boxplotOutliers = NA;
    transactionDf$dataCount_boxplotOutliers = NA;
    transactionDf$avgSize_hampelOutliers = NA;
    transactionDf$avgPriceSqm_hampelOutliers = NA;
    transactionDf$medSize_hampelOutliers = NA;
    transactionDf$medPriceSqm_hampelOutliers = NA;
    transactionDf$regPriceSqm_hampelOutliers = NA;
    transactionDf$dataCount_hampelOutliers = NA;
    transactionDf$avgSize_percentilesOutliers = NA;
    transactionDf$avgPriceSqm_percentilesOutliers = NA;
    transactionDf$medSize_percentilesOutliers = NA;
    transactionDf$medPriceSqm_percentilesOutliers = NA;
    transactionDf$regPriceSqm_percentilesOutliers = NA;
    transactionDf$dataCount_percentilesOutliers = NA;
    
    return(transactionDf)
  } else
  {
    transactionDf = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Real_Estate/GridData/gridded', cityDatabaseDf$TransactionType, '_',
                                   cityDatabaseDf$TransactionSource, '_', cityDatabaseDf$TransactionMonth, cityDatabaseDf$TransactionYear, '_',
                                   toupper(cityDatabaseDf$City), '.csv', sep = ''),
                                   stringsAsFactors = FALSE);
    
    transactionDf_boxplotOutliers = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Real_Estate/GridData/gridded', cityDatabaseDf$TransactionType, '_',
                                             cityDatabaseDf$TransactionSource, '_', cityDatabaseDf$TransactionMonth, cityDatabaseDf$TransactionYear, '_',
                                             toupper(cityDatabaseDf$City), '_boxplotOutliers', '.csv', sep = ''),
                                             stringsAsFactors = FALSE);
    
    transactionDf_hampelOutliers = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Real_Estate/GridData/gridded', cityDatabaseDf$TransactionType, '_',
                                            cityDatabaseDf$TransactionSource, '_', cityDatabaseDf$TransactionMonth, cityDatabaseDf$TransactionYear, '_',
                                            toupper(cityDatabaseDf$City), '_hampelOutliers', '.csv', sep = ''),
                                            stringsAsFactors = FALSE);
    
    transactionDf_percentilesOutliers = read.csv(paste('./Data/', cityDatabaseDf$Country, '/', cityDatabaseDf$City, '/Real_Estate/GridData/gridded', cityDatabaseDf$TransactionType, '_',
                                                 cityDatabaseDf$TransactionSource, '_', cityDatabaseDf$TransactionMonth, cityDatabaseDf$TransactionYear, '_',
                                                 toupper(cityDatabaseDf$City), '_percentilesOutliers', '.csv', sep = ''),
                                                 stringsAsFactors = FALSE);
    
    colnames(transactionDf) = c("ID", "XCOORD", "YCOORD", "AREA", "avgPrice", "avgSize", "avgPriceSqm", "medPrice", "medSize", "medPriceSqm", "regPriceSqm", "dataCount")
    colnames(transactionDf_boxplotOutliers) = c("ID", "XCOORD", "YCOORD", "AREA", "avgPrice", "avgSize_boxplotOutliers", "avgPriceSqm_boxplotOutliers", "medPrice", "medSize_boxplotOutliers", "medPriceSqm_boxplotOutliers", "regPriceSqm_boxplotOutliers", "dataCount_boxplotOutliers")
    colnames(transactionDf_hampelOutliers) = c("ID", "XCOORD", "YCOORD", "AREA", "avgPrice", "avgSize_hampelOutliers", "avgPriceSqm_hampelOutliers", "medPrice", "medSize_hampelOutliers", "medPriceSqm_hampelOutliers", "regPriceSqm_hampelOutliers", "dataCount_hampelOutliers")
    colnames(transactionDf_percentilesOutliers) = c("ID", "XCOORD", "YCOORD", "AREA", "avgPrice", "avgSize_percentilesOutliers", "avgPriceSqm_percentilesOutliers", "medPrice", "medSize_percentilesOutliers", "medPriceSqm_percentilesOutliers", "regPriceSqm_percentilesOutliers", "dataCount_percentilesOutliers")
    
    transactionDf_boxplotOutliers = transactionDf_boxplotOutliers[ , !(colnames(transactionDf_boxplotOutliers) %in% c("XCOORD", "YCOORD", "AREA", "avgPrice", "medPrice"))]
    transactionDf_hampelOutliers = transactionDf_hampelOutliers[ , !(colnames(transactionDf_hampelOutliers) %in% c("XCOORD", "YCOORD", "AREA", "avgPrice", "medPrice"))]
    transactionDf_percentilesOutliers = transactionDf_percentilesOutliers[ , !(colnames(transactionDf_percentilesOutliers) %in% c("XCOORD", "YCOORD", "AREA", "avgPrice", "medPrice"))]
    transactionDf = transactionDf[ , !(colnames(transactionDf) %in% c("avgPrice", "medPrice"))]
    
    transactionDf = merge(transactionDf, transactionDf_boxplotOutliers, on = "ID")
    transactionDf = merge(transactionDf, transactionDf_hampelOutliers, on = "ID")
    transactionDf = merge(transactionDf, transactionDf_percentilesOutliers, on = "ID")
    
    transactionDf = transactionDf %>% arrange(ID);
    
    return(transactionDf);
  }
}


#############################################################################################################################################################################
# MAIN SCRIPT TO BUILD TIDY DATABASE
#############################################################################################################################################################################

# Import city database
cityDatabaseDf = read.csv('./CityDatabases/cityDatabase.csv',
                          stringsAsFactors = FALSE);

list_cities = unique(cityDatabaseDf$City[cityDatabaseDf$TransactionType == "Rent"])

cityDatabaseDf = cityDatabaseDf[cityDatabaseDf$City %in% list_cities, ]

# To initialize the dataframe
avgCityDataDf = data.frame(matrix(ncol = 92, nrow = 0)) 
columnNames = c('ID', 'X', 'Y', 'Area', 'City', 'Country', 'Continent', 'Currency', 'GridEPSG', 'dCenter', 'TransactionType', 'TransactionSource', 'TransactionMonth', 
                'TransactionYear', "avgSize", "avgPriceSqm", "medSize", "medPriceSqm", "regPriceSqm", "dataCount", "medSize_boxplotOutliers", "dataCount_boxplotOutliers", "medSize_hampelOutliers", "medPriceSqm_hampelOutliers", "regPriceSqm_hampelOutliers",    
                "dataCount_hampelOutliers", "avgSize_percentilesOutliers", "avgPriceSqm_percentilesOutliers",
                "medSize_percentilesOutliers", "medPriceSqm_percentilesOutliers", "regPriceSqm_percentilesOutliers",
                "dataCount_percentilesOutliers", 'TransportSource', 'RushHour', 'TransportYear', 'DistanceDriving', 'DurationDriving', 'DistanceTransit', 
                'DurationTransit', 'PopDensitySource', 'PopDensityYear', 'PopDensity', 'OpenedToUrb', 'ClosedToUrb', "ESACCI10", "ESACCI11",  "ESACCI12",  "ESACCI14",  "ESACCI20",  "ESACCI30",  "ESACCI40",  "ESACCI50", 
                "ESACCI60",  "ESACCI61",  "ESACCI62",  "ESACCI70",  "ESACCI71",  "ESACCI72",  "ESACCI80",  "ESACCI81",  "ESACCI82", 
                "ESACCI90",  "ESACCI100", "ESACCI110","ESACCI120", "ESACCI121", "ESACCI122", "ESACCI130", "ESACCI140", "ESACCI150",
                "ESACCI151", "ESACCI152", "ESACCI153","ESACCI160", "ESACCI170", "ESACCI180", "ESACCI190", "ESACCI200", "ESACCI201",
                "ESACCI202", "ESACCI210", "ESACCI220", "ESACCI230")
colnames(avgCityDataDf) = columnNames
rm(columnNames)

# Initialize GUI progress bar
progressBar = tkProgressBar(title = 'Building tidy database...',
                            min = 0,
                            max = nrow(cityDatabaseDf),
                            width = 300);

barCount = 0;

# Loop over cityDatabaseDf rows
for(iRow in (1:nrow(cityDatabaseDf)))
{
  # Update bar count
  barCount = barCount + 1;
  
  # Tidy city data
  tidyDataTmp = ImportAvgTidyData(cityDatabaseDf[iRow,]);
  
  avgCityDataDf = rbind(avgCityDataDf, tidyDataTmp);
  rm(tidyDataTmp);
  
  # Update progress bar
  setTkProgressBar(progressBar, barCount, label= paste(round((barCount/nrow(cityDatabaseDf)) * 100, 1), '% completed [', cityDatabaseDf$City[iRow], ']', sep = ''));
}

close(progressBar);

# Save data to R format
saveRDS(avgCityDataDf, file = paste('./R_Databases/avgCityData_CL_20220712.rds', sep = ''))















