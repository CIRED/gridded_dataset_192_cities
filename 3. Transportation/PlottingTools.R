# ------------------------------------------------------------------------------------------------------------------ #
# FUNCTIONS TO PLOT CITY DATA
# ------------------------------------------------------------------------------------------------------------------ #

# Import libraries
library(tidyverse)

# Import files
source('./R_Script/ProcessingTools/DataTools.R')


# ---------------------------------------------------------------------------------------------------------- #
# Plot dCenter X population density of cities in different year
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing data for a single city or various ones 
# -> precision is the precision to compute the profile
# -> facetScale is a string which gives the scales to use for x, y or both axis : 'fixed', 'free_x', 'free_y' or 'free'
# -> printPlot is a boolean : TRUE prints the plot and FALSE does not

Draw_dCenter_X_PopDensity = function(dataDf, precision, facetScale, printPlot)
{
  # Compute cities profiles
  spatialMeanDf = ComputeMultipleSpatialMean(dataDf = dataDf,
                                             xName = 'dCenter',
                                             yName = 'PopDensity',
                                             precision = 1000);
  
  # Build plot
  plot = ggplot(spatialMeanDf, aes(x = dCenter/1000, y = PopDensity, color = City)) +
    geom_ribbon(aes(ymin = LowerBand, ymax = UpperBand), fill = 'green', size = 0.2, alpha = 0.10) + 
    geom_line(size = 1.5) +
    theme_grey() +
    labs(title = paste('Population density profile from ', spatialMeanDf$PopDensitySource, sep = ''),
         x = 'Distance from CBD (Km)',
         y = 'Population density (People/km²)') +
    theme(plot.title = element_text(size = 30, face = 'bold', hjust = 0),
          axis.title = element_text(size = 20, face = 'bold', hjust = 0.5),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = 'top',
          legend.justification = 'left',
          legend.title = element_text(size = 20, face = 'bold'),
          legend.text = element_text(size = 20),
          strip.text.x = element_text(size = 20, face = 'bold.italic')) +
    facet_wrap(~ PopDensityYear, ncol = 2, scales = facetScale)
  
  # Show plot depending printPlot
  if(printPlot)
  {
    print(plot);
    return(plot);
  }else
  {
    return(plot);
  }
}


# ---------------------------------------------------------------------------------------------------------- #
# Plot dCenter X log(population density) of cities in different year
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing data for a single city or various ones 
# -> precision is the precision to compute the profile
# -> facetScale is a string which gives the scales to use for x, y or both axis : 'fixed', 'free_x', 'free_y' or 'free'
# -> printPlot is a boolean : TRUE prints the plot and FALSE does not

Draw_dCenter_X_logPopDensity = function(dataDf, precision, facetScale, printPlot)
{
  # Compute cities profiles
  spatialMeanDf = ComputeMultipleSpatialMean(dataDf = dataDf,
                                             xName = 'dCenter',
                                             yName = 'PopDensity',
                                             precision = 1000);
  
  # Build plot
  plot = ggplot(spatialMeanDf, aes(x = dCenter/1000, y = log(PopDensity), color = City)) +
    geom_ribbon(aes(ymin = log(LowerBand), ymax = log(UpperBand)), fill = 'green', size = 0.2, alpha = 0.10) + 
    geom_line(size = 1.5) +
    theme_grey() +
    labs(title = paste('Population density profile from ', spatialMeanDf$PopDensitySource, sep = ''),
         x = 'Distance from CBD (Km)',
         y = 'Population density (People/km²)') +
    theme(plot.title = element_text(size = 30, face = 'bold', hjust = 0),
          axis.title = element_text(size = 20, face = 'bold', hjust = 0.5),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = 'top',
          legend.justification = 'left',
          legend.title = element_text(size = 20, face = 'bold'),
          legend.text = element_text(size = 20),
          strip.text.x = element_text(size = 20, face = 'bold.italic')) +
    facet_wrap(~ PopDensityYear, ncol = 2, scales = facetScale)
  
  # Show plot depending printPlot
  if(printPlot)
  {
    print(plot);
    return(plot);
  }else
  {
    return(plot);
  }
}


# ---------------------------------------------------------------------------------------------------------- #
# Plot dCenter X PriceSqm for 'Rent' or 'Sale'
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing data for a single city or various ones
# -> transactionType is the transaction to be ploted : 'Rent' or 'Sale'
# -> precision is the precision to compute the profile
# -> facetScale is a string which gives the scales to use for x, y or both axis : 'fixed', 'free_x', 'free_y' or 'free'
# -> printPlot is a boolean : TRUE prints the plot and FALSE does not

Draw_dCenter_X_PriceSqm = function(dataDf, transactionType, precision, facetScale, printPlot)
{
  # Filter the right transaction
  dataDf = dataDf %>% filter(TransactionType == transactionType);
  
  # Compute cities profiles
  spatialMeanDf = ComputeMultipleSpatialMean(dataDf = dataDf,
                                             xName = 'dCenter',
                                             yName = 'PriceSqm',
                                             precision = 1000);
  
  # Create a new column for the plot
  spatialMeanDf$Source = paste(spatialMeanDf$City, ' - ', spatialMeanDf$TransactionSource, ' - ', spatialMeanDf$TransactionMonth, '-',
                               spatialMeanDf$TransactionYear, ' (', spatialMeanDf$Currency, ')', 
                                 sep = '');
  
  # Adapt title legend
  if(transactionType == 'Rent')
  {
    titleLegend = 'Monthly rent in different cities at different date';  
  } else
  {
    titleLegend = 'Sale price in different cities at different date';  
  }
  
  # Build plot
  plot = ggplot(spatialMeanDf, aes(x = dCenter/1000, y = PriceSqm, color = Source)) +
    geom_ribbon(aes(ymin = LowerBand, ymax = UpperBand), fill = 'green', size = 0.2, alpha = 0.10) + 
    geom_line(size = 1.5) +
    theme_grey() +
    labs(title = titleLegend,
         x = 'Distance from CBD (Km)',
         y = 'Price per m²') +
    theme(plot.title = element_text(size = 30, face = 'bold', hjust = 0),
          axis.title = element_text(size = 20, face = 'bold', hjust = 0.5),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = 'top',
          legend.justification = 'left',
          legend.title = element_text(size = 15, face = 'bold'),
          legend.text = element_text(size = 10),
          strip.text.x = element_text(size = 20, face = 'bold.italic')) +
    facet_wrap(~ Currency, ncol = 3, scales = facetScale)
  
  # Show plot depending printPlot
  if(printPlot)
  {
    print(plot);
    return(plot);
  }else
  {
    return(plot);
  }
}


# ---------------------------------------------------------------------------------------------------------- #
# Plot dCenter X Size for 'Rent' or 'Sale'
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing data for a single city or various ones
# -> transactionType is the transaction to be ploted : 'Rent' or 'Sale'
# -> precision is the precision to compute the profile
# -> facetScale is a string which gives the scales to use for x, y or both axis : 'fixed', 'free_x', 'free_y' or 'free'
# -> printPlot is a boolean : TRUE prints the plot and FALSE does not

Draw_dCenter_X_Size = function(dataDf, transactionType, precision, facetScale, printPlot)
{
  # Filter the right transaction
  dataDf = dataDf %>% filter(TransactionType == transactionType);
  
  # Compute cities profiles
  spatialMeanDf = ComputeMultipleSpatialMean(dataDf = dataDf,
                                             xName = 'dCenter',
                                             yName = 'Size',
                                             precision = 1000);
  
  # Create a new column for the plot
  spatialMeanDf$Source = paste(spatialMeanDf$City, ' - ', spatialMeanDf$TransactionSource, ' - ', spatialMeanDf$TransactionMonth, '-',
                               spatialMeanDf$TransactionYear, 
                               sep = '');
  
  # Adapt title legend
  if(transactionType == 'Rent')
  {
    titleLegend = 'Dwelling size for rent in different cities at different date';  
  } else
  {
    titleLegend = 'Dwelling size for sale in different cities at different date';  
  }
  
  # Build plot
  plot = ggplot(spatialMeanDf, aes(x = dCenter/1000, y = Size, color = Source)) +
    geom_ribbon(aes(ymin = LowerBand, ymax = UpperBand), fill = 'green', size = 0.2, alpha = 0.10) + 
    geom_line(size = 1.5) +
    theme_grey() +
    labs(title = titleLegend,
         x = 'Distance from CBD (Km)',
         y = 'Dwelling size in m²') +
    theme(plot.title = element_text(size = 30, face = 'bold', hjust = 0),
          axis.title = element_text(size = 20, face = 'bold', hjust = 0.5),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = 'top',
          legend.justification = 'left',
          legend.title = element_text(size = 15, face = 'bold'),
          legend.text = element_text(size = 10),
          strip.text.x = element_text(size = 20, face = 'bold.italic')) +
    facet_wrap(~ Continent, ncol = 3, scales = facetScale)
  
  # Show plot depending printPlot
  if(printPlot)
  {
    print(plot);
    return(plot);
  }else
  {
    return(plot);
  }
}


# ---------------------------------------------------------------------------------------------------------- #
# Plot dCenter X Duration for Driving and Transit
# ---------------------------------------------------------------------------------------------------------- #

# Documentation :
# -> dataDf is a dataframe containing data for a single city or various ones 
# -> precision is the precision to compute the profile
# -> facetScale is a string which gives the scales to use for x, y or both axis : 'fixed', 'free_x', 'free_y' or 'free'
# -> printPlot is a boolean : TRUE prints the plot and FALSE does not

Draw_dCenter_X_Duration = function(dataDf, precision, facetScale, printPlot)
{
  # Compute cities profiles
  drivingSpatialMeanDf = ComputeMultipleSpatialMean(dataDf = dataDf,
                                             xName = 'dCenter',
                                             yName = 'DurationDriving',
                                             precision = 1000);
  
  transitSpatialMeanDf = ComputeMultipleSpatialMean(dataDf = dataDf,
                                                    xName = 'dCenter',
                                                    yName = 'DurationTransit',
                                                    precision = 1000);
  
  drivingSpatialMeanDf = drivingSpatialMeanDf %>% dplyr::select(City, Country, Continent, dCenter, DurationDriving, LowerBand, UpperBand) %>%
    mutate(TransportMode = 'Driving')
  
  transitSpatialMeanDf = transitSpatialMeanDf %>% dplyr::select(City, Country, Continent, dCenter, DurationTransit, LowerBand, UpperBand) %>%
    mutate(TransportMode = 'Transit')
  
  names(drivingSpatialMeanDf)[5] = 'Duration';
  names(transitSpatialMeanDf)[5] = 'Duration'; 
  
  spatialMeanDf = rbind(drivingSpatialMeanDf, transitSpatialMeanDf)
  
  # Build plot
  plot = ggplot(spatialMeanDf, aes(x = dCenter/1000, y = Duration/60, color = TransportMode)) +
    geom_ribbon(aes(ymin = LowerBand/60, ymax = UpperBand/60), fill = 'green', size = 0.2, alpha = 0.10) + 
    geom_line(size = 1.5) +
    theme_grey() +
    labs(title = paste('Duration from CBD for cities in ', spatialMeanDf$Country, sep = ''),
         x = 'Distance from CBD (Km)',
         y = 'Duration (minutes)') +
    theme(plot.title = element_text(size = 30, face = 'bold', hjust = 0),
          axis.title = element_text(size = 20, face = 'bold', hjust = 0.5),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          legend.position = 'top',
          legend.justification = 'left',
          legend.title = element_text(size = 20, face = 'bold'),
          legend.text = element_text(size = 20),
          strip.text.x = element_text(size = 20, face = 'bold.italic')) +
    facet_wrap(~ City, ncol = 2, scales = facetScale)
  
  # Show plot depending printPlot
  if(printPlot)
  {
    print(plot);
    return(plot);
  }else
  {
    return(plot);
  }
}