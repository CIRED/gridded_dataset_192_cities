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