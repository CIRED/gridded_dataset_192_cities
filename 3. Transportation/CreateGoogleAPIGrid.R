# ------------------------------------------------------------------------------------------------------------------ #
# SCRIPT CREATE GOOGLE API GRID
# ------------------------------------------------------------------------------------------------------------------ #

# Clean workspace
rm(list = ls())
gc()

# Import libraries
library(tidyverse)
library(sf)
library(sp)
library(concaveman)
library(reader)
library(tcltk)

setwd('/home/quentin/Documents/City_dataStudy')

# Import city database
cityDatabaseDf = read.csv('./CityDatabases/cityDatabase.csv',
                          stringsAsFactors = FALSE)
# Keep cities which does not already have transport data
cityDatabaseDf = cityDatabaseDf %>% filter(PopDensityYear == '2015') %>%
  dplyr::select(City, Country, Continent, Currency, GridEPSG) %>%
  distinct


# ----------------------------------------------- #
# Function to save center data converted in WGS84
# ----------------------------------------------- #

# Documentation :
# -> cityDataDf is a single line dataframe with fields : City, Country, Continent, GridEPSG

SaveWGS84Center = function(cityDataDf, path, fileName)
{
  # Import center
  centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_', cityDataDf$GridEPSG, '.shp',
                         sep = '');
  
  if(!file.exists(centerFileName))
  {
    centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_final_', cityDataDf$GridEPSG, '.shp',
                           sep = '');
  }
  
  centerSf = st_read(dsn = centerFileName);
  
  # Convert to WGS84
  centerSf = st_transform(centerSf, crs = 4326)
  centerSf = cbind(centerSf, st_coordinates(centerSf));
  
  names(centerSf)[names(centerSf) == 'X.1'] <- 'wgsLong';
  names(centerSf)[names(centerSf) == 'Y.1'] <- 'wgsLat';
  
  st_write(centerSf, paste(path, '/', fileName, sep = ''),
           delete_dsn = TRUE)
}
  
  
# --------------------------------- #
# Function to create point shapefile
# --------------------------------- #

# Documentation :
# -> cityDataDf is a single line dataframe with fields : City, Country, Continent, GridEPSG

CreateGoogleAPIGrid = function(cityDataDf, nbPoints, nbLines, dZero, thetaZero)
{
  # Import grid
  gridSf = st_read(dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, '.shp',
                               sep = ''))
  
  # Import center
  centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_', cityDataDf$GridEPSG, '.shp',
                         sep = '');
  
  if(!file.exists(centerFileName))
  {
    centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_final_', cityDataDf$GridEPSG, '.shp',
                           sep = '');
  }
  
  centerSf = st_read(dsn = centerFileName);
  
  # Center coordinates
  x0 = centerSf$X;
  y0 = centerSf$Y;
  
  # Get grid bbox
  bbox = st_bbox(gridSf);
  bboxDf = data.frame(x = c(bbox[1], bbox[1], bbox[3], bbox[3]),
                      y = c(bbox[2], bbox[4], bbox[2], bbox[4]));
  
  bboxPoints = bboxDf %>% st_as_sf(coords = c('x', 'y')) %>%
    st_set_crs(st_crs(gridSf))
  bboxSf = concaveman(bboxPoints) %>%
    st_cast("MULTILINESTRING")
  
  # Clear memory
  rm(bboxPoints, bbox)
  
  # Get maximum distance from center
  dMax = gridSf %>% mutate(dCenter = sqrt((XCOORD - centerSf$X)^2 + (YCOORD - centerSf$Y)^2) + 1000) %>%
    summarise(d = max(dCenter, na.rm = TRUE))
  dMax = dMax$d;
  
  # Angle between lines
  theta = (2 * pi) / nbLines;
  
  # Create a dataframe containing lines data
  linesDf = data.frame(matrix(ncol = 3, nrow = nbLines));
  colnames(linesDf) = c('ID', 'theta', 'd');
  
  # Fill dataframe
  for(i in (1:nbLines))
  {
    # Line ID
    linesDf$ID[i] = i;
    
    # Line angle with origin
    linesDf$theta[i] = thetaZero + (i-1) * theta;
    
    x1 = x0 + dMax * round(cos(linesDf$theta[i]), 5);
    y1 = y0 + dMax * round(sin(linesDf$theta[i]), 5);
    
    vecLineDf = data.frame(x = c(x0, x1),
                           y = c(y0, y1));
    
    vecLineSf = st_as_sf(SpatialLines(list(Lines(Line(vecLineDf), ID = "a")))) %>%
      st_set_crs(st_crs(gridSf));
    
    interPoint = st_intersection(vecLineSf, bboxSf);
    
    # Compute line length adding an offset to slighly be out of the grid
    linesDf$d[i] = sqrt((st_coordinates(interPoint)[1] - centerSf$X)^2 + (st_coordinates(interPoint)[2] - centerSf$Y)^2) + 2000;
    
    # Clear memory
    rm(x1, y1, vecLineDf, vecLineSf, interPoint);
  }
  
  # Computing spacing between points on lines
  pointSpacing = (sum(linesDf$d, na.rm = TRUE) - nbLines * dZero) / (nbPoints - nbLines);
  
  # Creating grid
  googleGridDf = data.frame(x = double(),
                            y = double(),
                            lineID = integer(),
                            theta = double(),
                            dMax = double())
  
  for(i in (1:nbLines))
  {
    x = x0 + seq(from = dZero, to = linesDf$d[i], by = pointSpacing) * round(cos(linesDf$theta[i]), 5);
    y = y0 + seq(from = dZero, to = linesDf$d[i], by = pointSpacing) * round(sin(linesDf$theta[i]), 5);
    
    googleGridDfTmp = data.frame(matrix(ncol = 5, nrow = length(x)));
    colnames(googleGridDfTmp) = c('x', 'y', 'lineID', 'theta', 'dMax');
    
    googleGridDfTmp$x = x;
    googleGridDfTmp$y = y;
    googleGridDfTmp$lineID = i;
    googleGridDfTmp$theta = linesDf$theta[i];
    googleGridDfTmp$dMax = linesDf$d[i];
    
    googleGridDf = rbind(googleGridDf, googleGridDfTmp);
    
    rm(googleGridDfTmp);
  }
  
  googleGridSf = googleGridDf %>% st_as_sf(coords = c('x', 'y'),
                                           crs = st_crs(gridSf))
  
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  # Convert to WGS84
  googleGridSf = st_transform(googleGridSf, crs = 4326)
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  names(googleGridSf)[names(googleGridSf) == 'X.1'] <- 'wgsLong';
  names(googleGridSf)[names(googleGridSf) == 'Y.1'] <- 'wgsLat';
  
  return(googleGridSf);
}


# --------------------------------------------------------- #
# Function to create point shapefile including ocean limits
# --------------------------------------------------------- #

# Documentation :
# -> cityDataDf is a single line dataframe with fields : City, Country, Continent, GridEPSG

CreateGoogleAPIGrid_OceanIncluded = function(cityDataDf, nbPoints, nbLines, dZero, thetaZero)
{
  # Import grid
  gridSf = st_read(dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, '.shp',
                               sep = ''))
  
  # Import center
  centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_', cityDataDf$GridEPSG, '.shp',
                         sep = '');
  
  if(!file.exists(centerFileName))
  {
    centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_final_', cityDataDf$GridEPSG, '.shp',
                           sep = '');
  }
  
  centerSf = st_read(dsn = centerFileName);
  
  # Center coordinates
  x0 = centerSf$X;
  y0 = centerSf$Y;
  
  # Ocean shapefile
  oceanSf = st_read(dsn = '/home/quentin/Documents/NEDUM/data_World/NaturalEarthData/ne_10m_ocean/ne_10m_ocean.shp');
  
  # Get wgs bbox
  wgsBbox = st_bbox(st_transform(gridSf, crs = 4326));
  
  oceanSf = st_crop(x = oceanSf,
                    y = c(xmin = as.numeric(wgsBbox[1]),
                          ymin = as.numeric(wgsBbox[2]),
                          xmax = as.numeric(wgsBbox[3]),
                          ymax = as.numeric(wgsBbox[4])));
  
  oceanSf = st_transform(oceanSf, crs = cityDataDf$GridEPSG);
  
  # Get grid without ocean if oceanSf is empty
  if(nrow(oceanSf) > 0)
  {
    gridSf = st_difference(gridSf, oceanSf);  
  }
  
  # Get grid bbox
  bboxSf = st_union(gridSf);
  bboxSf = st_cast(bboxSf, "MULTILINESTRING")
  
  # Get maximum distance from center
  dMax = gridSf %>% mutate(dCenter = sqrt((XCOORD - centerSf$X)^2 + (YCOORD - centerSf$Y)^2) + 4000) %>%
    summarise(d = max(dCenter, na.rm = TRUE))
  dMax = dMax$d;
  
  # Angle between lines
  theta = (2 * pi) / nbLines;
  
  # Create a dataframe containing lines data
  linesDf = data.frame(matrix(ncol = 3, nrow = nbLines));
  colnames(linesDf) = c('ID', 'theta', 'd');
  
  # Fill dataframe
  for(i in (1:nbLines))
  {
    # Line ID
    linesDf$ID[i] = i;
    
    # Line angle with origin
    linesDf$theta[i] = thetaZero + (i-1) * theta;
    
    x1 = x0 + dMax * round(cos(linesDf$theta[i]), 5);
    y1 = y0 + dMax * round(sin(linesDf$theta[i]), 5);
    
    vecLineDf = data.frame(x = c(x0, x1),
                           y = c(y0, y1));
    
    vecLineSf = st_as_sf(SpatialLines(list(Lines(Line(vecLineDf), ID = "a")))) %>%
      st_set_crs(st_crs(gridSf));
    
    # Get intersection
    interPoint = st_intersection(vecLineSf, bboxSf);
    # Keep only the furthest point
    interPointDf = as.data.frame(st_coordinates(interPoint));
    # Compute distance from center to get the furthest intersection point
    interPointDf$d = sqrt((interPointDf$X - x0)^2 + (interPointDf$Y - y0)^2);
    index = which(interPointDf$d == max(interPointDf$d));
    
    interPointX = interPointDf$X[index];
    interPointY = interPointDf$Y[index];
    
    # Compute line length adding an offset to slighly be out of the grid
    linesDf$d[i] = sqrt((interPointX - centerSf$X)^2 + (interPointY - centerSf$Y)^2) + 3000;
    
    # Clear memory
    rm(x1, y1, vecLineDf, vecLineSf, interPoint);
  }
  
  # Check if dZero is not higher than linesDf$d
  if(dZero >= min(linesDf$d, na.rm = TRUE))
  {
    dZero = min(linesDf$d, na.rm = TRUE);
  }
  
  # Computing spacing between points on lines
  pointSpacing = (sum(linesDf$d, na.rm = TRUE) - nbLines * dZero) / (nbPoints - nbLines);
  
  # Creating grid
  googleGridDf = data.frame(x = double(),
                            y = double(),
                            lineID = integer(),
                            theta = double(),
                            dMax = double())
  
  for(i in (1:nbLines))
  {
    x = x0 + seq(from = dZero, to = linesDf$d[i], by = pointSpacing) * round(cos(linesDf$theta[i]), 5);
    y = y0 + seq(from = dZero, to = linesDf$d[i], by = pointSpacing) * round(sin(linesDf$theta[i]), 5);
    
    googleGridDfTmp = data.frame(matrix(ncol = 5, nrow = length(x)));
    colnames(googleGridDfTmp) = c('x', 'y', 'lineID', 'theta', 'dMax');
    
    googleGridDfTmp$x = x;
    googleGridDfTmp$y = y;
    googleGridDfTmp$lineID = i;
    googleGridDfTmp$theta = linesDf$theta[i];
    googleGridDfTmp$dMax = linesDf$d[i];
    
    googleGridDf = rbind(googleGridDf, googleGridDfTmp);
    
    rm(googleGridDfTmp);
  }
  
  googleGridSf = googleGridDf %>% st_as_sf(coords = c('x', 'y'),
                                           crs = st_crs(gridSf))
  
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  # Convert to WGS84
  googleGridSf = st_transform(googleGridSf, crs = 4326)
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  names(googleGridSf)[names(googleGridSf) == 'X.1'] <- 'wgsLong';
  names(googleGridSf)[names(googleGridSf) == 'Y.1'] <- 'wgsLat';
  
  return(googleGridSf);
}


# --------------------------------- #
# Function to create point shapefile
# --------------------------------- #

# Documentation :
# -> cityDataDf is a single line dataframe with fields : City, Country, Continent, GridEPSG

CreateRandomGoogleAPIGrid = function(cityDataDf, nbPoints)
{
  # Import grid
  gridSf = st_read(dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, '.shp',
                               sep = ''))
  
  # Import center
  centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_', cityDataDf$GridEPSG, '.shp',
                         sep = '');
  
  if(!file.exists(centerFileName))
  {
    centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_final_', cityDataDf$GridEPSG, '.shp',
                           sep = '');
  }
  
  centerSf = st_read(dsn = centerFileName);
  
  # Center coordinates
  x0 = centerSf$X;
  y0 = centerSf$Y;
  
  # Get grid bbox
  bbox = st_bbox(gridSf);
  
  # Creating grid
  googleGridDf = data.frame(x = double(nbPoints),
                            y = double(nbPoints));
  
  # Compute a smaller bbox
  # Size of the new bbox
  s = 0.6;
  
  xmin = x0 - s * (x0 - bbox[1]);
  xmax = x0 + s * (bbox[3] - x0);
  ymin = y0 - s * (y0 - bbox[2]);
  ymax = y0 + s * (bbox[4] - y0);
  
  # Get random x inside bbox
  googleGridDf$x = runif(nbPoints, xmin, xmax);
  # Get random y inside bbox
  googleGridDf$y = runif(nbPoints, ymin, ymax);
  
  googleGridSf = googleGridDf %>% st_as_sf(coords = c('x', 'y'),
                                           crs = st_crs(gridSf))
  
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  # Convert to WGS84
  googleGridSf = st_transform(googleGridSf, crs = 4326)
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  names(googleGridSf)[names(googleGridSf) == 'X.1'] <- 'wgsLong';
  names(googleGridSf)[names(googleGridSf) == 'Y.1'] <- 'wgsLat';
  
  return(googleGridSf);
}


# ----------------------------------------------------------------- #
# Function to create random points shapefile including ocean limits
# ----------------------------------------------------------------- #

# Documentation :
# -> cityDataDf is a single line dataframe with fields : City, Country, Continent, GridEPSG

CreateRandomGoogleAPIGrid_OceanIncluded = function(cityDataDf, nbPoints)
{
  # Import grid
  gridSf = st_read(dsn = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/grille_', toupper(cityDataDf$City), '_finale_', cityDataDf$GridEPSG, '.shp',
                               sep = ''))
  
  # Import center
  centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_', cityDataDf$GridEPSG, '.shp',
                         sep = '');
  
  if(!file.exists(centerFileName))
  {
    centerFileName = paste('./Data/', cityDataDf$Country, '/', cityDataDf$City, '/Grid/Centre_', toupper(cityDataDf$City), '_final_', cityDataDf$GridEPSG, '.shp',
                           sep = '');
  }
  
  centerSf = st_read(dsn = centerFileName);
  
  # Center coordinates
  x0 = centerSf$X;
  y0 = centerSf$Y;
  
  # Ocean shapefile
  oceanSf = st_read(dsn = '/home/quentin/Documents/NEDUM/data_World/NaturalEarthData/ne_10m_ocean/ne_10m_ocean.shp');
  
  # Get wgs bbox
  wgsBbox = st_bbox(st_transform(gridSf, crs = 4326));
  
  oceanSf = st_crop(x = oceanSf,
                    y = c(xmin = as.numeric(wgsBbox[1]),
                          ymin = as.numeric(wgsBbox[2]),
                          xmax = as.numeric(wgsBbox[3]),
                          ymax = as.numeric(wgsBbox[4])));
  
  oceanSf = st_transform(oceanSf, crs = cityDataDf$GridEPSG);
  
  # Get grid without ocean if oceanSf is empty
  if(nrow(oceanSf) > 0)
  {
    gridSf = st_difference(gridSf, oceanSf);  
  }
  
  # Get grid bbox
  bboxSf = st_union(gridSf);
  bboxSf = st_cast(bboxSf, "POLYGON")
  
  googleGridSf = st_sample(bboxSf, size = nbPoints, type = "random");
  
  googleGridDf = as.data.frame(st_coordinates(googleGridSf))
  googleGridSf = googleGridDf %>% st_as_sf(coords = c('X', 'Y'),
                                           crs = st_crs(gridSf))
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  # Convert to WGS84
  googleGridSf = st_transform(googleGridSf, crs = 4326)
  googleGridSf = cbind(googleGridSf, st_coordinates(googleGridSf));
  
  names(googleGridSf)[names(googleGridSf) == 'X.1'] <- 'wgsLong';
  names(googleGridSf)[names(googleGridSf) == 'Y.1'] <- 'wgsLat';
  
  return(googleGridSf);
}


# ----------- #
# MAIN SCRIPT
# ----------- #

nbPoints = 90;
nbLines = 8;
dZero = 800;
thetaZero = pi/16;
gridNameSuffix = '90_V4'


# Initialize GUI progress bar
progressBar = tkProgressBar(title = 'Creating grid for Google API...',
                            min = 0,
                            max = nrow(cityDatabaseDf),
                            width = 300);

barCount = 0;


# # Loop over cities circular grid
# for(i in (1:length(cityDatabaseDf$City)))
# {
#   # Get city
#   cityDataDf = cityDatabaseDf[i, ];
# 
#   # Update bar count
#   barCount = barCount + 1;
# 
#   # Update progress bar
#   setTkProgressBar(progressBar, barCount, label = paste(round((barCount/nrow(cityDatabaseDf)) * 100, 1), '% completed [', cityDataDf$City, ']', sep = ''));
# 
#   googleGridSf = CreateGoogleAPIGrid(cityDataDf, nbPoints, nbLines, dZero, thetaZero);
# 
#   # Save result
#   ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)),
#          dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)), FALSE)
#   ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))),
#          dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))), FALSE)
# 
#   path = file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''));
#   fileName = paste('googleGrid_', cityDataDf$City, '_', cityDataDf$GridEPSG, '_', gridNameSuffix, '.csv', sep = '');
# 
#   # Save Google grid
#   st_write(googleGridSf, paste(path, '/', fileName, sep = ''),
#            delete_dsn = TRUE)
# 
#   # Save WGS84 Center
#   SaveWGS84Center(cityDataDf = cityDataDf,
#                   path = path,
#                   fileName = paste('googleCenter_', cityDataDf$City, '_', cityDataDf$GridEPSG, '.csv', sep = ''));
# }
# 
# close(progressBar)


# Loop over cities circular grid including ocean boundaries
for(i in (138:length(cityDatabaseDf$City)))
{
  # Get city
  cityDataDf = cityDatabaseDf[i, ];

  # Update bar count
  barCount = barCount + 1;

  # Update progress bar
  setTkProgressBar(progressBar, barCount, label = paste(round((barCount/nrow(cityDatabaseDf)) * 100, 1), '% completed [', cityDataDf$City, ']', sep = ''));

  googleGridSf = CreateGoogleAPIGrid_OceanIncluded(cityDataDf, nbPoints, nbLines, dZero, thetaZero);

  # Save result
  ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)),
         dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)), FALSE)
  ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))),
         dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))), FALSE)

  path = file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''));
  fileName = paste('googleGrid_', cityDataDf$City, '_', cityDataDf$GridEPSG, '_', gridNameSuffix,  '.csv', sep = '');

  # Save Google grid
  st_write(googleGridSf, paste(path, '/', fileName, sep = ''),
           delete_dsn = TRUE)

  # Save WGS84 Center
  SaveWGS84Center(cityDataDf = cityDataDf,
                  path = path,
                  fileName = paste('googleCenter_', cityDataDf$City, '_', cityDataDf$GridEPSG, '.csv', sep = ''));
}

close(progressBar)


# # Loop over cities random grid
# for(i in (1:length(cityDatabaseDf$City)))
# {
#   # Get city
#   cityDataDf = cityDatabaseDf[i, ];
# 
#   # Update bar count
#   barCount = barCount + 1;
# 
#   # Update progress bar
#   setTkProgressBar(progressBar, barCount, label = paste(round((barCount/nrow(cityDatabaseDf)) * 100, 1), '% completed [', cityDataDf$City, ']', sep = ''));
# 
#   googleGridSf = CreateRandomGoogleAPIGrid(cityDataDf, nbPoints);
# 
#   # Save result
#   ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)),
#          dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)), FALSE)
#   ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))),
#          dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))), FALSE)
# 
#   path = file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''));
#   fileName = paste('googleGrid_', cityDataDf$City, '_', cityDataDf$GridEPSG, '_', gridNameSuffix, '.csv', sep = '');
# 
#   # Save Google grid
#   st_write(googleGridSf, paste(path, '/', fileName, sep = ''),
#            delete_dsn = TRUE)
# 
#   # Save WGS84 Center
#   SaveWGS84Center(cityDataDf = cityDataDf,
#                   path = path,
#                   fileName = paste('googleCenter_', cityDataDf$City, '_', cityDataDf$GridEPSG, '.csv', sep = ''));
# }
# 
# close(progressBar)


# # Loop over cities random grid ocean included
# for(i in (1:length(cityDatabaseDf$City)))
# {
#   # Get city
#   cityDataDf = cityDatabaseDf[i, ];
# 
#   # Update bar count
#   barCount = barCount + 1;
# 
#   # Update progress bar
#   setTkProgressBar(progressBar, barCount, label = paste(round((barCount/nrow(cityDatabaseDf)) * 100, 1), '% completed [', cityDataDf$City, ']', sep = ''));
# 
#   googleGridSf = CreateRandomGoogleAPIGrid_OceanIncluded(cityDataDf, nbPoints);
# 
#   # Save result
#   ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)),
#          dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', cityDataDf$Country)), FALSE)
#   ifelse(!dir.exists(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))),
#          dir.create(file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''))), FALSE)
# 
#   path = file.path('/home/quentin/Documents/Google_API_Program/Project/', paste(cityDataDf$Country, '/', cityDataDf$City, sep = ''));
#   fileName = paste('googleGrid_', cityDataDf$City, '_', cityDataDf$GridEPSG, '_', gridNameSuffix, '.csv', sep = '');
# 
#   # Save Google grid
#   st_write(googleGridSf, paste(path, '/', fileName, sep = ''),
#            delete_dsn = TRUE)
# 
#   # Save WGS84 Center
#   SaveWGS84Center(cityDataDf = cityDataDf,
#                   path = path,
#                   fileName = paste('googleCenter_', cityDataDf$City, '_', cityDataDf$GridEPSG, '.csv', sep = ''));
# }
# 
# close(progressBar)









