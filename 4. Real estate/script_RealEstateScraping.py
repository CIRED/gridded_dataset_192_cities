# -*- coding: utf-8 -*-
"""
Created on Mon Sep 24 15:42:58 2018

@author: Quentin Lepetit
"""

###############################################################################
# SCRAPPING REAL ESTATE DATA FROM WEBSITES
###############################################################################

# Clear workspace
def clearall():
    all = [var for var in globals() if var[0] != "_"]
    for var in all:
        del globals()[var]
        
clearall()

# Path to program folder
mainPath = "C:/Users/charl/City_Database/API_RealEstateScraping";


###############################################################################
# IMPORT LIBRARIES
###############################################################################

import os
import sys
sys.path.append(mainPath);
import datetime

import dataBaseTools as tools
import scrapingDataTools as sdTools
import saveDataTools as svTools

# Changing working directory
os.chdir(mainPath);


###############################################################################
# CARACTERISTICS
###############################################################################

country = "China";
sourceWebsite = "Beijing_anjukeRent";
# Select a zone from the website. Can be a city, a state... Check the source website
zoneName = "Haidian";
# Select dwelling type : "Flat" / "House" / "NoSpecificDwelling"
dwellingType = "Flat";
# Select transaction : "Rent" / "Sale"
transactionType = "Rent";

# Database options
dataBaseFileType = ".csv";
dataBaseSeparator = ",";

# Data otpions
dataFolderName = "Test";

# Web options
# Pause between each page loading (seconds)
pauseDuration = 1;
# Maximum number of pages to investigate
maxPages = 2;

# Path to database
dataBasePath = "./Data/" + country + "/" + sourceWebsite + "/" + "dataBase_" + country + "_" + sourceWebsite + dataBaseFileType;

# Summary screen output
print("REAL ESTATE DATA : From " + sourceWebsite + " - " + country + " - " + zoneName + " - " + dwellingType + " for " + transactionType);


###############################################################################
# IMPORT DATABASE
###############################################################################

# Check if exists
tools.CheckDataBase(dataBasePath);

# Then import
dataBaseDf = tools.ImportColumnDataBase(dataBasePath, dataBaseSeparator);

# Import user agents
userAgentDf = sdTools.ImportUserAgentDataBase('dataBase_UserAgent.csv');

# Get URLs
zoneURL = tools.GetZoneURL(zoneName, dataBaseDf, ",");
dwellingTypeURL = tools.GetDwellingTypeURL(dwellingType, dataBaseDf);
transactionTypeURL = tools.GetTransactionTypeURL(transactionType, dataBaseDf);

# Dataframe containing the elements to build the URLs
urlDf = tools.CreateURLDf(dataBaseDf, dwellingTypeURL, transactionTypeURL, zoneURL);
# Create a dataframe containing the delimiters
delimDf = tools.CreateDelimDf(dataBaseDf);


###############################################################################
# DOWNLOAD DATA
###############################################################################

# Retrieve the number of Ads and pages
numberResultsAndPages = sdTools.GetNumberResultsAndPages(urlDf, delimDf, maxPages, userAgentDf);

# From each page get the URL links to the ads
adsURLList = sdTools.GetAdsURL(numberResultsAndPages, maxPages, userAgentDf, pauseDuration, urlDf, delimDf);

# Scrap data
dataDf = sdTools.ScrapAdsData(userAgentDf, urlDf, delimDf, adsURLList, pauseDuration, transactionType);


###############################################################################
# SAVE DATA
###############################################################################

# Path to data
dataPath = "./Data/" + country + "/" + sourceWebsite + "/" + dataFolderName + "/";
# Csv name
today = datetime.datetime.today();
dataCsvName = transactionType + "_" + dwellingType + "_" + sourceWebsite + "_" + zoneName + "_" + str(today.day) + "-" + str(today.month) + "-" + str(today.year);
dataCsvName = dataCsvName.replace(' ', '_');

# Check if csv exits
dataCsvName = svTools.CheckDataCsv(dataPath, dataCsvName);

# Save csv 
svTools.SaveDfToCsv(dataPath, dataCsvName, dataDf);

















