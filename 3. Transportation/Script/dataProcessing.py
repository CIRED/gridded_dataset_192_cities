# -*- coding: utf-8 -*-
"""
Created on Mon Feb 26 14:21:43 2018

@author: Quentin LEPETIT
"""

# Import libraries
import sys
import numpy as np
import polyline


# Function to import a csv file as an array (Only with float fields)
def importCsv(filePath):
    
    data = np.loadtxt(filePath, delimiter = ',', skiprows = 1);
    return data;


# Encode polylines for Google API
def googleencodepolyline(lat, lng):
    
    # lat is np array
    # lng is np array
    
    if (type(lat) is not np.ndarray or type(lng) is not np.ndarray):
        
        sys.stderr.write('    -> ERROR : Could not encode centers polyline. Latitude or Longitude is not a proper ndarray type');
        sys.exit(-1);
    
    if (len(lat) != len(lng)):
        
        sys.stderr.write('    -> ERROR : Could not encode centers polyline. Latitude and Longitude do not have the same length');
        sys.exit(-1);
    
    # Coord list
    coord = list();
    
    # Format the vectors to have (lat, lng) tuples
    for i in range(0, len(lat)):
        coord.append((lat[i], lng[i]));
    
    enccoord = polyline.encode(coord);
    enccoord = enccoord.encode('ascii');
    
    return enccoord;
