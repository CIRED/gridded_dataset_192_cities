# -*- coding: utf-8 -*-
"""
Created on Fri Sep 28 17:34:31 2018

@author: quentin
"""

###############################################################################
# FUNCTIONS TO SAVE DATA
###############################################################################

import os
import sys
import pandas


# SAVE DATAFRAME TO CSV
def SaveDfToCsv(path, csvName, dataDf):
    
    # Check if dataBaseDf is a pandas' dataframe
    if(not isinstance(dataDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in SaveDfToCsv is not a Dataframe. ");
        sys.exit(-1);     
    
    # Check if csv name has the right form
    if(csvName.find('.csv') > -1):
        # Erase .csv
        csvName.replace('.csv', '');
    
    csvToSave = path + csvName + '.csv';    
    dataDf.to_csv(csvToSave);
    
    print("Data saved : " + csvToSave);
    

# CHECK IF DATA CSV NAME EXISTS AND CHANGES IT IN CASE
def CheckDataCsv(dataPath, dataCsvName):
    
    # Check if date folder exists
    if(not(os.path.isdir(dataPath))):
        
        # Create folder
        os.makedirs(dataPath);
        # Returns dataCsvName
        return dataCsvName;
        
    else:
        
        # Index to increment dataCsvName
        csvNameIndex = 1;
        dataCsvNameTmp = dataCsvName;        
        
        while(os.path.exists(dataPath + dataCsvNameTmp + '.csv')):
            
            # Loop to increment dataCsvName
            dataCsvNameTmp = dataCsvName + '_' + str(csvNameIndex);
            csvNameIndex = csvNameIndex + 1;
        
        dataCsvName = dataCsvNameTmp;
        return dataCsvName;
    
