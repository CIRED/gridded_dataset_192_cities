# ------------------------------------------------------------------------------------------------------------------ #
# FUNCTIONS TO PROCESS CITY DATA
# ------------------------------------------------------------------------------------------------------------------ #

# Import libraries
library(tidyverse)
library(matlib)


# ---------------------------------------------------------------------------------------------------------- #
# Interpolation function giving NA values to x out of range
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> x and y are two vectors
# -> xi is the vector of x to be interpolated
# -> method is a string with the method to be used

interp2 = function(x, y, xi = x, method) 
{
  yi = rep(NA, length(xi));
  sel = which(xi >= range(x)[1] & xi <= range(x)[2]);
  yi[sel] = interp1(x = x, y = y, xi = xi[sel], method);
  
  return(yi);
}


# ---------------------------------------------------------------------------------------------------------- #
# Compute spatial mean for one city
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing the data
# -> xName is the field name of X in dataDf
# -> yName is the field name of Y in dataDf
# -> precision is a numeric value of the precision in X units

ComputeSpatialMean = function(dataDf, xName, yName, precision)
{
  # Select columns to keep
  dataDf = dataDf %>% select(City, Country, Continent, Currency, GridEPSG, TransactionType, TransactionSource, TransactionMonth, TransactionYear, TransportSource, RushHour,
                             TransportYear, PopDensitySource, PopDensityYear, xName, yName);
  
  # Number of rows in spatial mean dataframe depending on the chosen precision
  nMeanRow = max(dataDf[xName]) / precision;
  
  # Dataframe of spatial mean
  spatialMeanDataDf = data.frame(matrix(nrow = nMeanRow, ncol = ncol(dataDf) + 2));
  colnames(spatialMeanDataDf) = c(colnames(dataDf), 'LowerBand', 'UpperBand');
  
  # Fill with other values
  spatialMeanDataDf[, 1:(ncol(dataDf)-2)] = dataDf[1, 1:(ncol(dataDf)-2)];
  
  precisionStep = 0;
  
  # Loop over the number of rows
  for(iRow in (1:nrow(spatialMeanDataDf)))
  {
    spatialMeanDataDf[[xName]][iRow] = precisionStep
    spatialMeanDataDf[[yName]][iRow] = mean(dataDf[yName][dataDf[xName] >= precisionStep & dataDf[xName] < (precisionStep + precision)], na.rm = TRUE)
    spatialMeanDataDf[['LowerBand']][iRow] = min(dataDf[yName][dataDf[xName] >= precisionStep & dataDf[xName] < (precisionStep + precision)], na.rm = TRUE)
    spatialMeanDataDf[['UpperBand']][iRow] = max(dataDf[yName][dataDf[xName] >= precisionStep & dataDf[xName] < (precisionStep + precision)], na.rm = TRUE)
    
    # Replace inf by NA
    spatialMeanDataDf$LowerBand[is.infinite(spatialMeanDataDf$LowerBand)] = NA;
    spatialMeanDataDf$UpperBand[is.infinite(spatialMeanDataDf$UpperBand)] = NA
    
    precisionStep = precisionStep + precision;
  }
  
  return(spatialMeanDataDf)
}


# ---------------------------------------------------------------------------------------------------------- #
# Compute spatial mean for multiple cities
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing the data
# -> xName is the field name of X in dataDf
# -> yName is the field name of Y in dataDf
# -> precision is a numeric value of the precision in X units

ComputeMultipleSpatialMean = function(dataDf, xName, yName, precision)
{
  # Unique combination of data
  uniqueDataPartDf = unique(dataDf[! (colnames(dataDf) %in% c('ID', 'X', 'Y', 'Area', 'dCenter', 'Size', 'PriceSqm', 'DistanceDriving', 'DurationDriving',
                                                              'DistanceTransit', 'DurationTransit', 'PopDensity'))])
  
  # Remove NA values if there are
  uniqueDataPartDf$TransactionYear[is.na(uniqueDataPartDf$TransactionYear)] = '';
  uniqueDataPartDf$TransportYear[is.na(uniqueDataPartDf$TransportYear)] = '';
  
  dataDf$TransactionYear[is.na(dataDf$TransactionYear)] = '';
  dataDf$TransportYear[is.na(dataDf$TransportYear)] = '';
  
  # First city
  dataDfTmp = dataDf %>% filter(City == uniqueDataPartDf$City[1] &
                                Country == uniqueDataPartDf$Country[1] &
                                Continent == uniqueDataPartDf$Continent[1] &
                                Currency == uniqueDataPartDf$Currency[1] &
                                GridEPSG == uniqueDataPartDf$GridEPSG[1] &
                                TransactionType == uniqueDataPartDf$TransactionType[1] &
                                TransactionSource == uniqueDataPartDf$TransactionSource[1] &
                                TransactionMonth == uniqueDataPartDf$TransactionMonth[1] &
                                TransactionYear == uniqueDataPartDf$TransactionYear[1] &
                                TransportSource == uniqueDataPartDf$TransportSource[1] &
                                RushHour == uniqueDataPartDf$RushHour[1] &
                                TransportYear == uniqueDataPartDf$TransportYear[1] &
                                PopDensitySource == uniqueDataPartDf$PopDensitySource[1] &
                                PopDensityYear == uniqueDataPartDf$PopDensityYear[1])
  
  spatialMeanDataDf = ComputeSpatialMean(dataDf = dataDfTmp,
                                         xName = xName,
                                         yName = yName,
                                         precision = precision);
  rm(dataDfTmp);
  
  # If only one unique data line
  if(nrow(uniqueDataPartDf) == 1)
  {
    return(spatialMeanDataDf);
  } else
  {
    for(i in (2:nrow(uniqueDataPartDf)))
    {
      dataDfTmp = dataDf %>% filter(City == uniqueDataPartDf$City[i] &
                                      Country == uniqueDataPartDf$Country[i] &
                                      Continent == uniqueDataPartDf$Continent[i] &
                                      Currency == uniqueDataPartDf$Currency[i] &
                                      GridEPSG == uniqueDataPartDf$GridEPSG[i] &
                                      TransactionType == uniqueDataPartDf$TransactionType[i] &
                                      TransactionSource == uniqueDataPartDf$TransactionSource[i] &
                                      TransactionMonth == uniqueDataPartDf$TransactionMonth[i] &
                                      TransactionYear == uniqueDataPartDf$TransactionYear[i] &
                                      TransportSource == uniqueDataPartDf$TransportSource[i] &
                                      RushHour == uniqueDataPartDf$RushHour[i] &
                                      TransportYear == uniqueDataPartDf$TransportYear[i] &
                                      PopDensitySource == uniqueDataPartDf$PopDensitySource[i] &
                                      PopDensityYear == uniqueDataPartDf$PopDensityYear[i]);
      
      spatialMeanDataDfTmp = ComputeSpatialMean(dataDf = dataDfTmp,
                                                xName = xName,
                                                yName = yName,
                                                precision = precision);
      
      spatialMeanDataDf = rbind(spatialMeanDataDf, spatialMeanDataDfTmp);
      
      # Clear memory
      rm(dataDfTmp, spatialMeanDataDfTmp);
    }
    
    return(spatialMeanDataDf)
  }
}


# ---------------------------------------------------------------------------------------------------------- #
# Compute rescaling factor for transaction in PriceSqm = f(x)
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> profileDataDf is a dataframe containing the profile data with a 'Source' column
# -> xName is the field name of X in dataDf
# -> refCitySource is a string with the Source column of the reference city. Ex : 'Paris - avendrealouer - Jul-2019 (EUR)'

ComputeTransactionRescalingFactor = function(profileDataDf, xName, refCitySource)
{
  # Number of curves
  uniqueDataPartDf = unique(profileDataDf[! (colnames(profileDataDf) %in% c('ID', 'X', 'Y', 'Area', 'dCenter', 'Size', 'PriceSqm', 'DistanceDriving', 'DurationDriving',
                                                              'DistanceTransit', 'DurationTransit', 'PopDensity', 'LowerBand', 'UpperBand'))]);
  nCurves = nrow(uniqueDataPartDf);
  
  # Create a squared matrix with the number of cities
  M = matrix(0L,
             nrow = nCurves - 1,
             ncol = nCurves - 1);
  
  # Data matrixes
  matrixCurvesDf = profileDataDf %>% filter(Source != refCitySource)
  listMatrixCurves = unique(matrixCurvesDf$Source);
  refCityDf = profileDataDf %>% filter(Source == refCitySource)
  
  # Fill M matrix
  for(iRow in 1:(nCurves-1))
  {
    for(iCol in 1:(nCurves-1))
    {
      # Diagonal
      if(iRow == iCol)
      {
        M[iRow, iCol] = (nCurves - 1) * sum(matrixCurvesDf$PriceSqm[matrixCurvesDf$Source == listMatrixCurves[iRow]] ^ 2, na.rm = TRUE)
      } else
      {
        M[iRow, iCol] = -sum(matrixCurvesDf$PriceSqm[matrixCurvesDf$Source == listMatrixCurves[iRow]] * matrixCurvesDf$PriceSqm[matrixCurvesDf$Source == listMatrixCurves[iCol]], na.rm = TRUE)
      }
    }
  }
  
  rm(iRow, iCol);
  
  # Create a column matrix B
  B = matrix(0L,
             nrow = nCurves - 1,
             ncol = 1);
  
  # Fill B
  for(iRow in 1:(nCurves-1))
  {
    B[iRow, 1] = sum(refCityDf$PriceSqm * matrixCurvesDf$PriceSqm[matrixCurvesDf$Source == listMatrixCurves[iRow]], na.rm = TRUE)
  }
  
  # Compute M determinant
  detM = det(M);
  
  if(detM != 0)
  {
    print(paste('det(M) is not null. Unique solution', sep = ''))
  } else
  {
    print(paste('det(M) is null'));
  }
  
  # Solution vector of lambdas
  lambdasVector = inv(M) %*% B;
  
  profileDataDf$lambda = NA;
  profileDataDf$Rescaling = 'Non rescaled';
  
  # Dataframe of rescaled data
  rescaledDf = profileDataDf;
  rescaledDf$Rescaling = 'Rescaled';
  
  # Multiply by new lambda coefficient
  rescaledDf$lambda[rescaledDf$Source == refCitySource] = 1;
  
  for(source in listMatrixCurves)
  {
    rescaledDf$lambda[rescaledDf$Source == source] = lambdasVector[which(listMatrixCurves == source)];
  }
  
  rescaledDf$PriceSqm = rescaledDf$PriceSqm * rescaledDf$lambda;
  rescaledDf$LowerBand = rescaledDf$LowerBand * rescaledDf$lambda;
  rescaledDf$UpperBand = rescaledDf$UpperBand * rescaledDf$lambda;
  
  # Combine the two dataframes
  rescaledDf = rbind(profileDataDf, rescaledDf);
  
  return(rescaledDf)
}











