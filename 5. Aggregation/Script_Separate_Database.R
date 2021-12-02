# ------------------------------------------------------------------------------------------------------------------ #
# SCRIPT SEPARATE DATABASE
# ------------------------------------------------------------------------------------------------------------------ #

# Clean workspace
rm(list = ls())
gc()

# Import libraries
library(tidyverse)

setwd('C:/Users/charl/OneDrive/Bureau/City_dataStudy');

# Data outliers method : '', '_boxplotOutliers', '_hampelOutliers', '_percentilesOutliers'
#outliersMethod = '_boxplotOutliers_PerPixel';

# Import data
avgData = readRDS(file = paste('./R_Databases/avgCityData_CL.rds', sep = ''))

# --- Transport data --- #
transportDataDf = avgData %>% filter(PopDensityYear == 2015) %>%
  distinct(ID, X, Y, Area, City, Country, Continent, GridEPSG, dCenter,
           TransportSource, RushHour, TransportYear, DistanceDriving, DurationDriving, DistanceTransit, DurationTransit)


saveRDS(transportDataDf, './R_Databases/TransportData_CL.rds')
write.csv(transportDataDf, './R_Databases/TransportData_CL.csv', row.names = FALSE)


# --- Population density data --- #
populationDensityDataDf = avgData %>% distinct(ID, X, Y, Area, City, Country, Continent, GridEPSG, dCenter,
                                               PopDensitySource, PopDensityYear, PopDensity)

saveRDS(populationDensityDataDf, './R_Databases/PopulationDensityData_CL.rds')
write.csv(populationDensityDataDf, './R_Databases/PopulationDensityData_CL.csv', row.names = FALSE)


# --- real estate data --- #
realEstateDataDf = avgData %>% filter(TransactionSource != '') %>%
  dplyr::select(ID, X, Y, Area, City, Country, Continent, GridEPSG, dCenter,
                Currency, TransactionType, TransactionSource, TransactionMonth, TransactionYear,
                avgSize, avgPriceSqm, medSize, medPriceSqm, regPriceSqm, nRealEstateData,
                avgSize_boxplotOutliers, avgPriceSqm_boxplotOutliers, medSize_boxplotOutliers, medPriceSqm_boxplotOutliers, regPriceSqm_boxplotOutliers, nRealEstateData_boxplotOutliers, 
                avgSize_hampelOutliers, avgPriceSqm_hampelOutliers, medSize_hampelOutliers, medPriceSqm_hampelOutliers, regPriceSqm_hampelOutliers, nRealEstateData_hampelOutliers,
                avgSize_percentilesOutliers, avgPriceSqm_percentilesOutliers, medSize_percentilesOutliers, medPriceSqm_percentilesOutliers, regPriceSqm_percentilesOutliers, nRealEstateData_percentilesOutliers)

saveRDS(realEstateDataDf, paste('./R_Databases/RealEstateData_CL.rds', sep = ''))
write.csv(realEstateDataDf, paste('./R_Databases/RealEstateData_CL.csv', sep = ''), row.names = FALSE)

# --- land cover data --- #

landCoverDataDf = avgData %>% filter(PopDensityYear == 2015) %>%
  dplyr::select(ID, X, Y, Area, City, Country, Continent, GridEPSG, dCenter,
                OpenedToUrb, ClosedToUrb, ESACCI10, ESACCI11, ESACCI12, ESACCI14, ESACCI20, ESACCI30,
                ESACCI40, ESACCI50, ESACCI60, ESACCI61, ESACCI62, ESACCI70, ESACCI71, ESACCI72, 
                ESACCI80, ESACCI81, ESACCI82, ESACCI90, ESACCI100, ESACCI110, ESACCI120, ESACCI121, ESACCI122, 
                ESACCI130, ESACCI140, ESACCI150, ESACCI151, ESACCI152, ESACCI153, ESACCI160, ESACCI170, ESACCI180,
                ESACCI190, ESACCI200, ESACCI201, ESACCI202, ESACCI210, ESACCI220, ESACCI230)

saveRDS(landCoverDataDf, paste('./R_Databases/LandCoverData_CL.rds', sep = ''))
write.csv(landCoverDataDf, paste('./R_Databases/LandCoverData_CL.csv', sep = ''), row.names = FALSE)
