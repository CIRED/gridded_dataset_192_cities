# -*- coding: utf-8 -*-
"""
Created on Mon Sep 24 15:57:07 2018

@author: quentin
"""

###############################################################################
# DATABSE TOOLS
###############################################################################

import os
import sys
import pandas
import numpy as np
import stringTools as strTools
    

# CHECK IF DATABASE EXISTS
def CheckDataBase(dataBasePath):
    
    if(os.path.isfile(dataBasePath)):
        return;
        
    else:
        sys.stderr.write("DataBase : " + dataBasePath + " not found. ");
        sys.exit(-1);


# IMPORT ROW DATABASE AS DATAFRAME
def ImportRowDataBase(dataBasePath, separator):
    
    dataBaseDf = pandas.read_csv(dataBasePath, sep = separator);
    dataBaseDf = strTools.FormatNaNToString(dataBaseDf);

    return dataBaseDf;
    

# IMPORT COLUMN DATABASE AS DATAFRAME
def ImportColumnDataBase(dataBasePath, separator):
    
    dataBaseDf = pandas.io.parsers.read_csv(dataBasePath, sep = separator, index_col = 0);
    dataBaseDf = dataBaseDf.T;
    
    # Convert needed values to int / float
    if(not(pandas.isnull(dataBaseDf.maxResultsPerPage[0]))):
        dataBaseDf.maxResultsPerPage[0] = int(dataBaseDf.maxResultsPerPage[0]);
    
    if(not(pandas.isnull(dataBaseDf.resOffset[0]))):
        dataBaseDf.resOffset[0] = int(dataBaseDf.resOffset[0]);
        
    if(not(pandas.isnull(dataBaseDf.adsOffset[0]))):
        dataBaseDf.adsOffset[0] = int(dataBaseDf.adsOffset[0]);
        
    if(not(pandas.isnull(dataBaseDf.latOffset[0]))):
        dataBaseDf.latOffset[0] = int(dataBaseDf.latOffset[0]);
        
    if(not(pandas.isnull(dataBaseDf.longOffset[0]))):
        dataBaseDf.longOffset[0] = int(dataBaseDf.longOffset[0]);
        
    if(not(pandas.isnull(dataBaseDf.priceOffset[0]))):
        dataBaseDf.priceOffset[0] = int(dataBaseDf.priceOffset[0]);
        
    if(not(pandas.isnull(dataBaseDf.sizeOffset[0]))):
        dataBaseDf.sizeOffset[0] = int(dataBaseDf.sizeOffset[0]);

    dataBaseDf = strTools.FormatNaNToString(dataBaseDf);        
    
    return dataBaseDf;


# SEPARATE ZONE NAMES AND URL IN FUNCTION OF A SEPARATOR
def GetZoneURL(zoneName, dataBaseDf, separator):
    
    # Check if dataBaseDf is a pandas' dataframe
    if(not isinstance(dataBaseDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in GetZoneURL is not a Dataframe. ");
        sys.exit(-1);
    
    # Zone names indexes
    sepNameIndex = strTools.GetExprPos(dataBaseDf.zoneName[0], separator);
    numberZones = len(sepNameIndex) + 1;
    
    if(not(sepNameIndex)):
        
        # If the list is empty, there is no zone in the database and we stop
        if(not(dataBaseDf.zoneName[0])):
            
            sys.stderr.write("No zone name found in the database. ");
            sys.exit(-1);
        
        # Case there is only one zone in the database
        else:
            
            zoneURL = dataBaseDf.zoneURL[0];
            return zoneURL;
    
    # Zone URL indexes
    sepURLIndex = strTools.GetExprPos(dataBaseDf.zoneURL[0], separator);
    
    # If the list is empty, there is no zone in the database and we stop
    if(not(sepURLIndex)):
        
        sys.stderr.write("No zone URL found in the database. ");
        sys.exit(-1);
    
    zoneNameList = list();
    zoneURLList = list();
    
    for i in range(0, numberZones):
        
        # First element
        if(i == 0):
            
            zoneNameList.append(dataBaseDf.zoneName[0][0:sepNameIndex[0][0]]);
            zoneURLList.append(dataBaseDf.zoneURL[0][0:sepURLIndex[0][0]]);
            
        else:
            
            # Last element
            if(i == numberZones - 1):
                
                zoneNameList.append(dataBaseDf.zoneName[0][sepNameIndex[numberZones-2][1]:len(dataBaseDf.zoneName[0])]);
                zoneURLList.append(dataBaseDf.zoneURL[0][sepURLIndex[numberZones-2][1]:len(dataBaseDf.zoneURL[0])]);
                
            else:
                
                zoneNameList.append(dataBaseDf.zoneName[0][sepNameIndex[i-1][1]:sepNameIndex[i][0]]);
                zoneURLList.append(dataBaseDf.zoneURL[0][sepURLIndex[i-1][1]:sepURLIndex[i][0]]);
    
    # Get zone index in the list
    try:
        zoneIndexInList = zoneNameList.index(zoneName);
    except:
        sys.stderr.write("The zone name given - " + zoneName + " - was not found in the database. ");
        sys.exit(-1);
    
    # Return zone URL
    zoneURL = zoneURLList[zoneIndexInList];
    return zoneURL;
    
    
# GET DWELLING TYPE URL
def GetDwellingTypeURL(dwellingType, dataBaseDf):
    
    # Check if dataBaseDf is a pandas' dataframe
    if(not isinstance(dataBaseDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in GetDwellingTypeURL is not a Dataframe. ");
        sys.exit(-1);    
    
    # Case without specific dwelling type given
    if(dwellingType == "NoSpecificDwelling"):
        
        dwellingTypeURL = "";
        return dwellingTypeURL;
    
    # Returning the right dwelling type URL
    if(dwellingType == "Flat"):
        
        dwellingTypeURL = dataBaseDf.flatURL[0];
        return dwellingTypeURL;
        
    else:
        
        if(dwellingType == "House"):
            
            dwellingTypeURL = dataBaseDf.houseURL[0];
            return dwellingTypeURL;
            
        else:
            
            sys.stderr.write("The dwelling type given is not right. It must be \"Flat\" or \"House\". ");
            sys.exit(-1);
            
            
# GET TRANSACTION TYPE URL
def GetTransactionTypeURL(transactionType, dataBaseDf):
    
    # Check if dataBaseDf is a pandas' dataframe
    if(not isinstance(dataBaseDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in GetTransactionTypeURL is not a Dataframe. ");
        sys.exit(-1);    
    
    # Returning the right dwelling type URL
    if(transactionType == "Rent"):
        
        transactionTypeURL = dataBaseDf.rentURL[0];
        return transactionTypeURL;
        
    else:
        
        if(transactionType == "Sale"):
            
            transactionTypeURL = dataBaseDf.saleURL[0];
            return transactionTypeURL;
            
        else:
            
            sys.stderr.write("The transaction type given is not right. It must be \"Rent\" or \"Sale\". ");
            sys.exit(-1);


# CREATE DATAFRAME CONTAINING THE URL ELEMENTS
def CreateURLDf(dataBaseDf, dwellingTypeURL, transactionTypeURL, zoneURL):
    
    # Check if dataBaseDf is a pandas' dataframe
    if(not isinstance(dataBaseDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in CreateURLDf is not a Dataframe. ");
        sys.exit(-1); 
        
    # Values and labels
    stringValues = [(dataBaseDf.homepageURL[0], dataBaseDf.pageNumberURL[0], dwellingTypeURL, transactionTypeURL, zoneURL, dataBaseDf.patternURL[0], dataBaseDf.patternAdURL[0], dataBaseDf.exceptionAdURL[0])];
    labels = ['homepageURL', 'pageNumberURL', 'dwellingTypeURL', 'transactionTypeURL', 'zoneURL', 'patternURL', 'patternAdURL', 'exceptionAdURL'];
    
    # Create dataframe
    urlDf = pandas.DataFrame.from_records(stringValues, columns = labels);
    return urlDf;


# CREATE DATAFRAME CONTAINING DELIMITERS
def CreateDelimDf(dataBaseDf):
    
    # Check if dataBaseDf is a pandas' dataframe
    if(not isinstance(dataBaseDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in CreateDelimDf is not a Dataframe. ");
        sys.exit(-1);     
    
    # First index for the delimiters in the csv
    indexInit = 11;    
        
    # Values and labels
    stringValues = (dataBaseDf.iat[0,indexInit],);
    labels = list();
        
    for index in range(indexInit + 1, dataBaseDf.size):        
        stringValues = stringValues + (dataBaseDf.iat[0,index],);
        
    for index in range(indexInit, dataBaseDf.size):        
        labels.append(dataBaseDf.columns[index]);
            
    # Shape stringValues data
    stringValues = [stringValues];
    
    # Create dataframe
    delimDf = pandas.DataFrame.from_records(stringValues, columns = labels);
    return delimDf;
    
    
# CREATE DATAFRAME CONTAINING THE DATA FOR EACH AD
def CreateDataDf(transactionType, adsURLList):
    
    # Labels
    labels = ['lat', 'long', 'price', 'dwSize', 'sqm' + transactionType];
    
    # Create dataframe
    dataDf = pandas.DataFrame(index = range(len(adsURLList)), columns = labels);
    return dataDf;


    



    







