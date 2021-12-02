###############################################################################
# PROCESS GHSL DENSITY DATA WITH CITY GRID
###############################################################################


# Third faster version
AggregateGHSLDataToGrid_V3 = function(gridSf, gridXghslSf, ghslYear, countryName, cityName)
{
  # Order dataframes
  gridSf = gridSf[order(gridSf$ID, decreasing = FALSE),];
  gridXghslSf = gridXghslSf[order(gridXghslSf$ID, decreasing = FALSE),];
  
  # Convert areas to numeric
  gridXghslSf$areaGHSL = as.numeric(gridXghslSf$areaGHSL);
  gridXghslSf$areaX = as.numeric(gridXghslSf$areaX);
  
  # Copy gridSf to initialize gridGHSLSf
  gridGHSLSf = gridSf;
  # Add new density column filled with 0
  gridGHSLSf$density_GHSL = 0;
  
  # Dataframe of unique grid pixels intersected
  uGridIDDf = data.frame(ID = unique(gridXghslSf$ID));
  
  # Initialize GUI progress bar
  progressBar = tkProgressBar(title = paste('Aggregation progress - ', cityName, ' - ', countryName, sep = ''),
                              min = 0,
                              max = length(uGridIDDf$ID),
                              width = 300);
  
  barCount = 0;
  
  # Double loop over IDs
  for(i in uGridIDDf$ID)
  {
    # Bar count update
    barCount = barCount + 1;
    
    # Get index of unique value i
    uGridIndex = which(gridXghslSf$ID == i);
    
    for(j in uGridIndex)
    {
      if(gridSf$ID[i] == gridXghslSf$ID[j])
      {
        # Compute density
        gridGHSLSf$density_GHSL[i] = gridGHSLSf$density_GHSL[i] + (gridXghslSf$population[j] / (gridXghslSf$areaGHSL[j] / 1000000)) * (gridXghslSf$areaX[j] / 1000000);
      }
    }
    
    # Update progress bar
    setTkProgressBar(progressBar, barCount, label= paste(year, ': ', round((barCount/length(uGridIDDf$ID)) * 100, 1), '% completed', sep = ''));
  }
  
  # Clear progress bar
  close(progressBar);
  
  # Change names
  colnames(gridGHSLSf) = c('ID_pixel', 'X_density_GHSL', 'Y_density_GHSL', 'AREA_pixel', 'geometry', 'density_GHSL');
  
  # Return gridGHSL
  return(gridGHSLSf)
}


# Second faster version
AggregateGHSLDataToGrid_V2 = function(gridSf, gridXghslSf, ghslYear, countryName, cityName)
{
  # Order dataframes
  gridSf = gridSf[order(gridSf$ID, decreasing = FALSE),];
  gridXghslSf = gridXghslSf[order(gridXghslSf$ID, decreasing = FALSE),];
  
  # Convert areas to numeric
  gridXghslSf$areaGHSL = as.numeric(gridXghslSf$areaGHSL);
  gridXghslSf$areaX = as.numeric(gridXghslSf$areaX);
  
  # Copy gridSf to initialize gridGHSLSf
  gridGHSLSf = gridSf;
  # Add new density column filled with 0
  gridGHSLSf$density_GHSL = 0;
  
  # Dataframe of unique grid pixels intersected
  uGridIDDf = data.frame(ID = unique(gridXghslSf$ID));
  
  # Initialize GUI progress bar
  progressBar = tkProgressBar(title = 'Aggregation progress',
                              min = 0,
                              max = length(uGridIDDf$ID),
                              width = 300);
  
  barCount = 0;
  
  # Double loop over IDs
  for(i in uGridIDDf$ID)
  {
    # Bar count update
    barCount = barCount + 1;
    # Indexes to remove because they matched
    rmIndex = c();
    
    # Get index of unique value i
    uGridIndex = which(gridXghslSf$ID == i);
    
    for(j in uGridIndex)
    {
      if(gridSf$ID[i] == gridXghslSf$ID[j])
      {
        # Compute density
        gridGHSLSf$density_GHSL[i] = gridGHSLSf$density_GHSL[i] + (gridXghslSf$population[j] / (gridXghslSf$areaGHSL[j] / 1000000)) * (gridXghslSf$areaX[j] / 1000000);
        # Fill rmIndex
        rmIndex = c(rmIndex, j);
      }
    }
    
    # Drop matching rows
    if(!is.null(rmIndex))
    {
      gridXghslSf = gridXghslSf[-rmIndex,];
    }
    
    # Update progress bar
    setTkProgressBar(progressBar, barCount, label= paste(round((barCount/length(uGridIDDf$ID)) * 100, 1), "% done"));
  }
  
  # Clear progress bar
  close(progressBar);
  
  # Return gridGHSL
  return(gridGHSLSf)
}


# First slow version
AggregateGHSLDataToGrid_V1 = function(gridSf, gridXghslSf, ghslYear, countryName, cityName)
{
  # Order dataframes
  gridSf = gridSf[order(gridSf$ID, decreasing = FALSE),];
  gridXghslSf = gridXghslSf[order(gridXghslSf$ID, decreasing = FALSE),];
  
  # Convert areas to numeric
  gridXghslSf$areaGHSL = as.numeric(gridXghslSf$areaGHSL);
  gridXghslSf$areaX = as.numeric(gridXghslSf$areaX);
  
  # Copy gridSf to initialize gridGHSLSf
  gridGHSLSf = gridSf;
  # Add new density column filled with 0
  gridGHSLSf$density_GHSL = 0;
  
  # Dataframe of unique grid pixels intersected
  uGridIDDf = data.frame(ID = unique(gridXghslSf$ID));
  
  # Initialize GUI progress bar
  progressBar = tkProgressBar(title = 'Aggregation progress',
                              min = 0,
                              max = length(uGridIDDf$ID),
                              width = 300);
  
  barCount = 0;
  
  # Double loop over IDs
  for(i in uGridIDDf$ID)
  {
    # Bar count update
    barCount = barCount + 1;
    # Indexes to remove because they matched
    rmIndex = c();
    
    # Get index of unique value i
    uGridIndex = which(gridShp$ID == i);
    
    for(j in (1:length(gridXghslSf$ID)))
    {
      if(gridSf$ID[i] == gridXghslSf$ID[j])
      {
        # Compute density
        gridGHSLSf$density_GHSL[i] = gridGHSLSf$density_GHSL[i] + (gridXghslSf$population[j] / (gridXghslSf$areaGHSL[j] / 1000000)) * (gridXghslSf$areaX[j] / 1000000);
        # Fill rmIndex
        rmIndex = c(rmIndex, j);
      }
    }
    
    # Drop matching rows
    if(!is.null(rmIndex))
    {
      gridXghslSf = gridXghslSf[-rmIndex,];
    }
    
    # Update progress bar
    setTkProgressBar(progressBar, barCount, label= paste(round((barCount/length(uGridIDDf$ID)) * 100, 1), "% done"));
  }
  
  # Clear progress bar
  close(progressBar);
  
  # Return gridGHSL
  return(gridGHSLSf)
}
