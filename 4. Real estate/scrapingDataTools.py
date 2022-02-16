# -*- coding: utf-8 -*-
"""
Created on Wed Sep 26 11:30:03 2018

@author: quentin
"""

###############################################################################
# SCRAPING DATA FUNCTIONS
###############################################################################

import sys
import pandas
import time
import math
import stringTools as strTools
import dataBaseTools as tools
import requests
import random


# IMPORT USER AGENT DATABASE
def ImportUserAgentDataBase(userAgentDbName):
    
    userAgentDf = pandas.read_csv('./Program/UserAgent/' + userAgentDbName, sep = ',', index_col = 0);

    return userAgentDf;
    
    
# GET A RANDOM USER AGENT
def GetRandomUserAgent(userAgentDf):
    
    fHeaders = {'User-Agent': userAgentDf.index[random.randint(0, len(userAgentDf.index) - 1)]};
    return fHeaders;
    
    
# BUILD RESULT PAGE URL
def BuildPageURL(urlDf, pageNumber):
    
    # Check if urlDf is a pandas' dataframe
    if(not isinstance(urlDf, pandas.DataFrame)):
        
        sys.stderr.write("Argument given in BuildPageURL is not a Dataframe. ");
        sys.exit(-1);
        
    # Initializing URL
    pageURL = urlDf.patternURL[0];
    # Setting page number URL part
    pageNumberURL = urlDf.pageNumberURL[0].replace("1", str(pageNumber));
    
    # Replacing the elements with URL part
    pageURL = pageURL.replace("homepageURL", urlDf.homepageURL[0]);
    pageURL = pageURL.replace("pageNumberURL", pageNumberURL)
    pageURL = pageURL.replace("dwellingTypeURL", urlDf.dwellingTypeURL[0]);
    pageURL = pageURL.replace("transactionTypeURL", urlDf.transactionTypeURL[0]);
    pageURL = pageURL.replace("zoneURL", urlDf.zoneURL[0]);
    
    return pageURL;


# GET TOTAL NUMBER OF RESULTS AND PAGES FOR SELECTED ZONE
def GetNumberResultsAndPages(urlDf, delimDf, maxPages, userAgentDf):
    
    # First page
    firstPageURL = BuildPageURL(urlDf, 1);
    # Getting an user-agent
    fHeaders = GetRandomUserAgent(userAgentDf);
    
    # Loading page
    requestURL = requests.get(firstPageURL, headers = fHeaders, timeout = 5);
    content = requestURL.content;
    
    
    ##### Case delimiter1 + OffSet #####
    if((delimDf.delimResults1[0] != 'NaN_String') and (delimDf.delimResults2[0] == 'NaN_String')):
        
        # Get position of the delimiter
        indexResults1 = strTools.GetExprPos(content, delimDf.delimResults1[0]);
        
        # Get number of results
        try:
            numberResultsStr = content[indexResults1[0][1] : indexResults1[0][1] + delimDf.resOffset[0]];
            numberResultsStr = numberResultsStr.replace('.', '');
            numberResultsStr = strTools.FilterDigits(numberResultsStr);
            numberResults = int(numberResultsStr);
            # Number of pages found
            numberResultsPages = int(math.ceil(numberResults / float(delimDf.maxResultsPerPage[0])));
            
        except:
            numberResults = 0;
            numberResultsPages = 0;
        
        
    ##### Case delimiter1 + delimiter2 #####
    if((delimDf.delimResults1[0] != 'NaN_String') and (delimDf.delimResults2[0] != 'NaN_String')):
        
        # Get position of the delimiters
        indexResults1 = strTools.GetExprPos(content, delimDf.delimResults1[0]);
        indexResults2 = strTools.GetExprPos(content, delimDf.delimResults2[0]);
        
        # Get number of results
        try:
            numberResultsStr = content[indexResults1[0][1]:indexResults2[0][0]];
            numberResultsStr = numberResultsStr.replace('.', '');
            numberResultsStr = strTools.FilterDigits(numberResultsStr);
            numberResults = int(numberResultsStr);
            # Number of pages found
            numberResultsPages = int(math.ceil(numberResults / float(delimDf.maxResultsPerPage[0])));
            
        except:
            numberResults = 0;
            numberResultsPages = 0;
        
        
    # Screen output
    print(str(numberResults) + " ads results found");
    print(str(numberResultsPages) + " pages found");

    if(numberResults == 0):
        sys.exit(-1);
        
    else:        
        if(numberResults > (maxPages * delimDf.maxResultsPerPage[0])):
            
            numberResults = int(maxPages * delimDf.maxResultsPerPage[0]);
            numberResultsPages = int(maxPages);
            print(str(numberResultsPages) + " pages to be investigated (" + str(numberResults) + " ads) because of forced maximum of " + str(maxPages) + " pages");
            
        else:
            
            print(str(numberResultsPages) + " pages to be investigated (" + str(numberResults) + " ads) - No forced maximum");
    
    # List containing the number of results and pages
    numberResultsAndPages = [numberResults, numberResultsPages];
    
    return numberResultsAndPages;
    
    
# GET URL FOR EACH AD OF EACH PAGE
def GetAdsURL(numberResultsAndPages, maxPages, userAgentDf, pauseDuration, urlDf, delimDf):
    
    # Initialize ads URL list
    adsURLList = [""] * numberResultsAndPages[0];
    
    # Number of URL already extracted : Null for now
    urlExtracted = 0;
    
    # Loop over the number of pages
    for indexPage in range(1, numberResultsAndPages[1] + 1):
        
        # Build URL for selected page
        pageURL = BuildPageURL(urlDf, indexPage);        
        
        #######################################################################
        # LAST PAGE CASE
        if(indexPage == numberResultsAndPages[1]):
            
            # Load page
            try:
                # Getting an user-agent
                fHeaders = GetRandomUserAgent(userAgentDf);
                # Loading page
                requestURL = requests.get(pageURL, headers = fHeaders, timeout = 5);
                content = requestURL.content;
                
            except:
                print("Impossible to read URL for page {page}".format(page = str(indexPage)));
                
                # nan for ads URL of this page
                for indexAd in range(urlExtracted, numberResultsAndPages[0]):
                    
                    adsURLList[indexAd] = 'nan';
                    
                urlExtracted = numberResultsAndPages[0];
                
                # Skip to next iteration
                continue;
                
            # Pause in the script
            time.sleep(pauseDuration);
            
            #### Case only delimiter1 + Offset #####
            if((delimDf.delimAds1[0] != 'NaN_String') and (delimDf.delimAds2[0] == 'NaN_String')):
                
                # Get position of delimiterAds1
                indexDelimAds1 = strTools.GetExprPos(content, delimDf.delimAds1[0]);
                
                # Getting URLs
                for indexAd in range(urlExtracted, numberResultsAndPages[0]):
                    
                    # Ad URL
                    adURL = content[indexDelimAds1[indexAd - urlExtracted][1]:indexDelimAds1[indexAd - urlExtracted][1] + delimDf.adsOffset[0]];                    
                    
                    # Check if URL has to be corrected
                    if(urlDf.exceptionAdURL[0] != 'NaN_String'):
                        
                        adURL = strTools.FixException(urlDf.exceptionAdURL[0], adURL);
                    
                    # Completing ad URL
                    adsURLList[indexAd] = urlDf.patternAdURL[0] + adURL;
                
                # Update URL extracted
                urlExtracted = urlExtracted + len(indexDelimAds1);
                
                # Output
                print("Page {currentPage}/{totalPages} investigated".format(currentPage = str(indexPage), totalPages = str(numberResultsAndPages[1])));
                
                # Skip to next iteration
                continue;
                
                
            ##### Case delimiter1 + delimiter2 #####
            if((delimDf.delimAds1[0] != 'NaN_String') and (delimDf.delimAds2[0] != 'NaN_String')):
                
                # Get position of delimiter
                indexDelimAds1 = strTools.GetExprPos(content, delimDf.delimAds1[0]);
                indexDelimAds2 = strTools.GetExprPos(content, delimDf.delimAds2[0]);
                
                # Getting URLs
                for indexAd in range(urlExtracted, numberResultsAndPages[0]):
                    
                    # Ad URL
                    try:
                        adURL = content[indexDelimAds1[indexAd - urlExtracted][1] : indexDelimAds2[indexAd - urlExtracted][0]];
                    except:
                        adURL = 'NaN';
                    
                    # Check if URL has to be corrected
                    if(urlDf.exceptionAdURL[0] != 'NaN_String'):
                        
                        adURL = strTools.FixException(urlDf.exceptionAdURL[0], adURL);
                        
                    # Completing ad URL
                    adsURLList[indexAd] = urlDf.patternAdURL[0] + adURL;
                        
                # Update URL extracted
                urlExtracted = urlExtracted + len(indexDelimAds1);
                
                # Output
                print("Page {currentPage}/{totalPages} investigated".format(currentPage = str(indexPage), totalPages = str(numberResultsAndPages[1])));
                
                # Skip to next iteration
                continue;
                
                
        #######################################################################    
        # OTHER PAGES
        else:            
                
            # Load page
            try:
                # Getting an user-agent
                fHeaders = GetRandomUserAgent(userAgentDf);
                # Loading page
                requestURL = requests.get(pageURL, headers = fHeaders, timeout = 5);
                content = requestURL.content;
                
            except:
                print("Impossible to read URL for page {page}".format(page = str(indexPage)));
                
                # nan for ads URL of this page
                for indexAd in range(urlExtracted, urlExtracted + delimDf.maxResultsPerPage[0]):
                    
                    adsURLList[indexAd] = 'nan';
                    
                urlExtracted = urlExtracted + delimDf.maxResultsPerPage[0];
                
                # Skip to next iteration
                continue;
                
            # Pause in the script
            time.sleep(pauseDuration);
            
            ##### Case only delimiter1 + Offset #####
            if((delimDf.delimAds1[0] != 'NaN_String') and (delimDf.delimAds2[0] == 'NaN_String')):
                
                # Get position of delimiterAds1
                indexDelimAds1 = strTools.GetExprPos(content, delimDf.delimAds1[0]);
                
                # Getting URLs
                for indexAd in range(urlExtracted, urlExtracted + len(indexDelimAds1)):
                    
                    # Ad URL
                    adURL = content[indexDelimAds1[indexAd - urlExtracted][1]:indexDelimAds1[indexAd - urlExtracted][1] + delimDf.adsOffset[0]];                    
                    
                    # Check if URL has to be corrected
                    if(urlDf.exceptionAdURL[0] != 'NaN_String'):
                        
                        adURL = strTools.FixException(urlDf.exceptionAdURL[0], adURL);
                    
                    # Completing ad URL
                    adsURLList[indexAd] = urlDf.patternAdURL[0] + adURL;
                
                # Update URL extracted
                urlExtracted = urlExtracted + len(indexDelimAds1);
                
                # Output
                print("Page {currentPage}/{totalPages} investigated".format(currentPage = str(indexPage), totalPages = str(numberResultsAndPages[1])));
                
                # Skip to next iteration
                continue;
                
                
            ##### Case delimiter1 + delimiter2 #####
            if((delimDf.delimAds1[0] != 'NaN_String') and (delimDf.delimAds2[0] != 'NaN_String')):
                
                # Get position of delimiter
                indexDelimAds1 = strTools.GetExprPos(content, delimDf.delimAds1[0]);
                indexDelimAds2 = strTools.GetExprPos(content, delimDf.delimAds2[0]);
                
                # Getting URLs
                for indexAd in range(urlExtracted, urlExtracted + len(indexDelimAds1)):
                    
                    # Ad URL
                    adURL = content[indexDelimAds1[indexAd - urlExtracted][1] : indexDelimAds2[indexAd - urlExtracted][0]];
                    
                    # Check if URL has to be corrected
                    if(urlDf.exceptionAdURL[0] != 'NaN_String'):
                        
                        adURL = strTools.FixException(urlDf.exceptionAdURL[0], adURL);
                        
                    # Completing ad URL
                    adsURLList[indexAd] = urlDf.patternAdURL[0] + adURL;
                        
                # Update URL extracted
                urlExtracted = urlExtracted + len(indexDelimAds1);
                
                # Output
                print("Page {currentPage}/{totalPages} investigated".format(currentPage = str(indexPage), totalPages = str(numberResultsAndPages[1])));
                
                # Skip to next iteration
                continue;
            
            
    # Return URL list
    return adsURLList;
    
    
# SCRAP DATA FOR EACH AD
def ScrapAdsData(userAgentDf, urlDf, delimDf, adsURLList, pauseDuration, transactionType):
    
    # Number of URL extracted
    numberURL = len(adsURLList);
    
    # Create a dataframe containing data for each ad
    dataDf = tools.CreateDataDf(transactionType, adsURLList);
    
    # Loop over the ads
    for indexAd in range(0, numberURL):
        
        # Try to load page for ad indexAd
        try:
            # Getting an user-agent
            fHeaders = GetRandomUserAgent(userAgentDf);
            # Loading page
            requestURL = requests.get(adsURLList[indexAd], headers = fHeaders, timeout = 5);
            content = requestURL.content;
            
        except:
            print("Unable to load URL for ad {numberAd}".format(numberAd = str(indexAd)));
            # Skip to next iteration
            continue;
        
        # Pause in the script
        time.sleep(pauseDuration);
        
        ##### SCRAP DATA ####
        
        # Coordinates
        coord = GetCoordData(content, delimDf);
        dataDf.lat[indexAd] = coord[0];
        dataDf.long[indexAd] = coord[1];

        # Price
        dataDf.price[indexAd] = GetPriceData(transactionType, content, delimDf);

        # Size
        dataDf.dwSize[indexAd] = GetSizeData(content, delimDf);
        
        # Price per sqm
        if(transactionType == 'Rent'):
            dataDf.sqmRent[indexAd] = ComputeSqmPrice(dataDf.price[indexAd], dataDf.dwSize[indexAd]);
        
        if(transactionType == 'Sale'):
            dataDf.sqmSale[indexAd] = ComputeSqmPrice(dataDf.price[indexAd], dataDf.dwSize[indexAd]);
        
        print("- Data for ad {currentAd}/{totalAd} loaded".format(currentAd = str(indexAd+1), totalAd = str(numberURL)));
    
    # Return dataframe
    return dataDf;


# GET COORDINATES : LATITUDE AND LONGITUDE DATA
def GetCoordData(content, delimDf):
    
    ##### Case only delimiter1 + Offset #####
    if((delimDf.delimLat1[0] and delimDf.delimLong1[0]) != 'NaN_String' and (delimDf.delimLat2[0] and delimDf.delimLong2[0]) == 'NaN_String'):
        
        # Get position of delimiLat1 in content
        indexDelimLat1 = strTools.GetExprPos(content, delimDf.delimLat1[0]);
        
        # Check if index is empty
        if(not(indexDelimLat1)):         
            # Lat becomes nan value
            lat = float('NaN');
            
        else:
            # String with lat value
            strLat = content[indexDelimLat1[0][1] : indexDelimLat1[0][1] + delimDf.latOffset[0]];
            # Filter digits and decimal
            strLat = strTools.FilterDecimalDigits(strLat);
            # Convert to float
            lat = float(strLat);
            
        # Get position of delimiLong1 in content
        indexDelimLong1 = strTools.GetExprPos(content, delimDf.delimLong1[0]);
        
        # Check if index is empty
        if(not(indexDelimLong1)):      
            # Long becomes nan value
            lng = float('NaN');
            
        else:
            # String with long value
            strLng = content[indexDelimLong1[0][1] : indexDelimLong1[0][1] + delimDf.longOffset[0]];
            # Filter digits and decimal
            strLng = strTools.FilterDecimalDigits(strLng);
            # Convert to float
            lng = float(strLng);
        
        # Returns a list with the coordinates
        coord = [lat, lng];
        return coord
        
        
    ##### Case delimiter1 + delimiter2 #####
    if((delimDf.delimLat1[0] and delimDf.delimLat2[0] and delimDf.delimLong1[0] and delimDf.delimLong2[0]) != 'NaN_String'):
        
        # Get position of lat delimiters
        indexDelimLat1 = strTools.GetExprPos(content, delimDf.delimLat1[0]);
        indexDelimLat2 = strTools.GetExprPos(content, delimDf.delimLat2[0]);
        
        # Check if index is empty
        if(not(indexDelimLat1) or not(indexDelimLat2)):
            # Lat becomes nan value
            lat = float('NaN');
            
        else:
            # String with lat value
            strLat = content[indexDelimLat1[0][1] : indexDelimLat2[0][0]];
            # Filter digits and decimal
            strLat = strTools.FilterDecimalDigits(strLat);
            # Convert to float
            lat = float(strLat);
            
        # Get position of long delimiters
        indexDelimLong1 = strTools.GetExprPos(content, delimDf.delimLong1[0]);
        indexDelimLong2 = strTools.GetExprPos(content, delimDf.delimLong2[0]);
        
        # Check if index is empty
        if(not(indexDelimLong1) or not(indexDelimLong2)):
            # Long becomes nan value
            lng = float('NaN');
            
        else:
            # String with lng value
            strLng = content[indexDelimLong1[0][1] : indexDelimLong2[0][0]];
            # Filter digits and decimal
            strLng = strTools.FilterDecimalDigits(strLng);
            # Convert to float
            lng = float(strLng);
            
        # Returns a list with the coordinates
        coord = [lat, lng];
        return coord;
        
        
    ##### Case delimiter1 Lat + delimiter2 Long with ',' separator #####
    if((delimDf.delimLat1[0] and delimDf.delimLong2[0]) != 'NaN_String' and (delimDf.delimLat2[0] and delimDf.delimLong1[0]) == 'NaN_String'):
        
        # Get position of delimiters
        indexDelimCoord1 = strTools.GetExprPos(content, delimDf.delimLat1[0]);
        indexDelimCoord2 = strTools.GetExprPos(content, delimDf.delimLong2[0]);
        
        # Check if index is empty
        if(not(indexDelimCoord1) or not(indexDelimCoord2)):
            # Returns nan value
            coord = [float('NaN'), float('NaN')];
            return coord;
            
        else:
            # String with coords
            strCoord = content[indexDelimCoord1[0][1] : indexDelimCoord2[0][0]];
            # Get position of the coma
            indexComa = strTools.GetExprPos(strCoord, ',');
            
            # Check if not empty
            if(not(indexComa)):
                # Returns nan value
                coord = [float('NaN'), float('NaN')];
                return coord;
                
            strLat = strCoord[0 : indexComa[0][0]];
            strLng = strCoord[indexComa[0][1] : len(strCoord)];
            
            try:
                # Returns a list with the coordinates
                coord = [float(strLat), float(strLng)];
                return coord;
                
            except:
                # If cannot convert to float returns nan value
                coord = [float('NaN'), float('NaN')];
                return coord;
        
        
# GET PRICE DATA
def GetPriceData(transactionType, content, delimDf):
    
    ###################################
    ##### /!\ EXCEPTION CASES /!\ #####
    ###################################
    if(delimDf.delimPriceException[0] != 'NaN_String'):
        
        price = GetExceptionPriceData(transactionType, content, delimDf);
        return price;
    
    
    ##### Case only delimiter1 + Offset #####
    if((delimDf.delimPrice1[0] != 'NaN_String') and (delimDf.delimPrice2[0] == 'NaN_String')):
    
        # Get position of delimiSize1 in content
        indexDelimPrice1 = strTools.GetExprPos(content, delimDf.delimPrice1[0]);
        
        # Check if index is empty
        if(not(indexDelimPrice1)):         
            # Price becomes nan value
            price = float('NaN');
            
        else:
            # String with price value
            strPrice = content[indexDelimPrice1[0][1] : indexDelimPrice1[0][1] + delimDf.priceOffset[0]];
            # Filter digits
            strPrice = strTools.FilterDigits(strPrice);
            # Convert to float
            price = float(strPrice);
        
        # Returns price
        return price;
        
    
    ##### Case delimiter1 + delimiter2 #####
    if((delimDf.delimPrice1[0] and delimDf.delimPrice2[0]) != 'NaN_String'):
        
        # Get positions of delimiters in content
        indexDelimPrice1 = strTools.GetExprPos(content, delimDf.delimPrice1[0]);
        indexDelimPrice2 = strTools.GetExprPos(content, delimDf.delimPrice2[0]);
        
        # Check if index is empty
        if(not(indexDelimPrice1) or not(indexDelimPrice2)):           
            # Price becomes nan value
            price = float('NaN');
            
        else:
            # String with price value
            strPrice = content[indexDelimPrice1[0][1] : indexDelimPrice2[0][0]];
            # Filter digits
            strPrice = strTools.FilterDigits(strPrice);
            # Convert to float
            price = float(strPrice);
        
        # Returns price
        return price;
        

# Exception price data
def GetExceptionPriceData(transactionType, content, delimDf):
    
    # ZONAPROP EXCEPTION    
    if(delimDf.delimPriceException[0] == "zonapropException"):
        
        # Adapt delimiter1
        if(transactionType == "Rent"):
            delimPrice1 = delimDf.delimPrice1[0].replace("TransactionType", "Alquiler");
        if(transactionType == "Sale"):
            delimPrice1 = delimDf.delimPrice1[0].replace("TransactionType", "Venta");
            
         # Get positions of delimiters in content
        indexDelimPrice1 = strTools.GetExprPos(content, delimPrice1);
        indexDelimPrice2 = strTools.GetExprPos(content, delimDf.delimPrice2[0]);
        
        # Check if index is empty
        if(not(indexDelimPrice1) or not(indexDelimPrice2)):
            # Price becomes nan value
            price = (float('NaN'), 'NaN');
            
        else:
            try:
                # String with price value
                strPriceComplete = content[indexDelimPrice1[0][1] : indexDelimPrice2[0][0]];
            except:
                price = (float('NaN'), 'NaN');
                return price;
                
            # Split string (Currency, Valor)
            strPrice = strTools.SplitString(strPriceComplete, " ");
            # Filter digits
            strPrice[1] = strTools.FilterDigits(strPrice[1]);
            # If is empty becomes NaN value            
            if(not(strPrice[1])):
                strPrice[1] = 'NaN';
                
            # Convert to float
            price = (float(strPrice[1]), strPrice[0]);
        
        return price;
        
        
    # SQUAREFOOT EXCEPTION
    if(delimDf.delimPriceException[0] == "squarefootException"):
        
        # Both prices (Sale/Rent) positions
        indexDelimPriceSR1 = strTools.GetExprPos(content, delimDf.delimPrice1[0]);
        indexDelimPriceSR2 = strTools.GetExprPos(content, delimDf.delimPrice2[0]);
        
        # Check if index is empty
        if(not(indexDelimPriceSR1) or not(indexDelimPriceSR2)):
            # Price becomes nan value
            price = float('NaN');
            return price;
            
        else:
            # String with both prices (If there are both)
            strPriceSR = content[indexDelimPriceSR1[0][1] : indexDelimPriceSR2[0][0]];
            # Erase '$' caracter to avoid finding bug
            strPriceSR = strPriceSR.replace('$', '');
            # Try to slip string by /
            indexSepPrice = strTools.GetExprPos(strPriceSR, ' / ');
            
            # Case there is no delimiter /
            if(not(indexSepPrice)):
                indexPriceSR = strTools.GetExprPos(strPriceSR, transactionType + ": HK ");
                
                # Check if empty
                if(not(indexPriceSR)):
                    price = float('NaN');
                    return price;
                
                # String with the price
                strPrice = strPriceSR[indexPriceSR[0][1] : len(strPriceSR)]
                # Check if a M = 1000000 is in the string
                indexPriceM = strTools.GetExprPos(strPrice, ' M');
                
                # Check if empty
                if(not(indexPriceM)):
                    # Take off comas and filter digits
                    strPrice = strTools.FilterDigits(strPrice);
                    price = float(strPrice);
                    return price;
                    
                else:
                    # Filter decimal digits
                    strPrice = strTools.FilterDecimalDigits(strPrice);
                    # Multiply by 1 million
                    price = float(strPrice) * 1000000;
                    return price;
                    
            # Case there is a delimiter /
            else:                
                # If transaction Type is sale, take left part
                if(transactionType == 'Sale'):
                    strPrice = strPriceSR[0 : indexSepPrice[0][0]];
                # If rent, take the right part
                else:
                    strPrice = strPriceSR[indexSepPrice[0][1] : len(strPriceSR)];
                    # Add capital letter to first letter
                    strPrice = strPrice[0].upper() + strPrice[1:len(strPrice)];
                    
                indexPriceSR = strTools.GetExprPos(strPriceSR, transactionType + ": HK ");
                
                # Check if empty
                if(not(indexPriceSR)):
                    price = float('NaN');
                    return price;
                
                # String with the price
                strPrice = strPriceSR[indexPriceSR[0][1] : len(strPriceSR)]
                # Check if a M = 1000000 is in the string
                indexPriceM = strTools.GetExprPos(strPrice, ' M');
                
                # Check if empty
                if(not(indexPriceM)):
                    # Take off comas and filter digits
                    strPrice = strTools.FilterDigits(strPrice);
                    price = float(strPrice);
                    return price;
                    
                else:
                    # Filter decimal digits
                    strPrice = strTools.FilterDecimalDigits(strPrice);
                    # Multiply by 1 million
                    price = float(strPrice) * 1000000;
                    return price;
        
        
    # IMOVELWEB BRAZIL EXCEPTION    
    if(delimDf.delimPriceException[0] == "imovelwebException"):
        
        # Adapt delimiter1
        if(transactionType == "Rent"):
            delimPrice1 = delimDf.delimPrice1[0].replace("TransactionType", "Alquiler");
        if(transactionType == "Sale"):
            delimPrice1 = delimDf.delimPrice1[0].replace("TransactionType", "Venta");
            
         # Get positions of delimiters in content
        indexDelimPrice1 = strTools.GetExprPos(content, delimPrice1);
        indexDelimPrice2 = strTools.GetExprPos(content, delimDf.delimPrice2[0]);
        
        # Check if index is empty
        if(not(indexDelimPrice1) or not(indexDelimPrice2)):
            # Price becomes nan value
            price = float('NaN');
            
        else:
            try:
                # String with price value
                strPriceComplete = content[indexDelimPrice1[0][1] : indexDelimPrice2[0][0]];
            except:
                price = float('NaN');
                return price;
                
            # Split string (Currency, Valor)
            strPrice = strTools.SplitString(strPriceComplete, " ");
            # Filter digits
            strPrice[1] = strTools.FilterDigits(strPrice[1]);
            # If is empty becomes NaN value            
            if(not(strPrice[1])):
                strPrice[1] = 'NaN';
                
            # Convert to float
            price = float(strPrice[1])
        
        return price;
        
        
    # INMUEBLES24 MEXICO EXCEPTION    
    if(delimDf.delimPriceException[0] == "inmuebles24Exception"):
        
        # Adapt delimiter1
        if(transactionType == "Rent"):
            delimPrice1 = delimDf.delimPrice1[0].replace("TransactionType", "Renta");
        if(transactionType == "Sale"):
            delimPrice1 = delimDf.delimPrice1[0].replace("TransactionType", "Venta");
            
        # Get positions of delimiters in content
        indexDelimPrice1 = strTools.GetExprPos(content, delimPrice1);
        indexDelimPrice2 = strTools.GetExprPos(content, delimDf.delimPrice2[0]);
        
        # Check if index is empty
        if(not(indexDelimPrice1) or not(indexDelimPrice2)):
            # Price becomes nan value
            price = float('NaN');
            
        else:
            try:
                # String with price value
                strPriceComplete = content[indexDelimPrice1[0][1] : indexDelimPrice2[0][0]];
            except:
                price = float('NaN');
                return price;
                
            # Split string (Currency, Valor)
            strPrice = strTools.SplitString(strPriceComplete, " ");
            # Filter digits
            strPrice[1] = strTools.FilterDigits(strPrice[1]);
            # If is empty becomes NaN value            
            if(not(strPrice[1])):
                strPrice[1] = 'NaN';
                
            # Convert to float
            price = float(strPrice[1])
        
        return price;
        
        
    # DOTHI EXCEPTION
    if(delimDf.delimPriceException[0] == "dothiException"):
        
        # Get positions of delimiters in content
        indexDelimPrice1 = strTools.GetExprPos(content, delimDf.delimPrice1[0]);
        
        # Check if index is empty
        if(not(indexDelimPrice1)):
            # Price becomes nan value
            price = float('NaN');
            
        else:
            try:
                # String with complete price value
                strPriceComplete = content[indexDelimPrice1[0][1] : indexDelimPrice1[0][1] + delimDf.priceOffset[0]];
            except:
                price = float('NaN');
                return price;
            
            # Multiplicator for the price depending on the unit
            multiplicator = float('NaN');
            
            if(strPriceComplete.find('Triệu') != -1):
                multiplicator = 1000000;
                
            if(strPriceComplete.find('Tỷ') != -1):
                multiplicator = 1000000000;
                
            # Filter digits in strPrice
            strPrice = strTools.FilterDecimalDigits(strPriceComplete);
            # If is empty becomes NaN value
            if(not(strPrice)):
                strPrice = 'NaN';
                
            # Convert to float and multiply
            price = float(strPrice) * multiplicator;
            
        return price;


# GET SIZE DATA
def GetSizeData(content, delimDf):
    
    ###################################
    ##### /!\ EXCEPTION CASES /!\ #####
    ###################################
    if(delimDf.delimSizeException[0] != 'NaN_String'):
        
        dwellingSize = GetExceptionSizeData(content, delimDf);
        return dwellingSize;
        
        
    ##### Case only delimiter1 + Offset #####
    if((delimDf.delimSize1[0] != 'NaN_String') and (delimDf.delimSize2[0] == 'NaN_String')):
        
        # Get position of delimiSize1 in content
        indexDelimSize1 = strTools.GetExprPos(content, delimDf.delimSize1[0]);
        
        # Check if index is empty
        if(not(indexDelimSize1)):         
            # Size becomes nan value
            dwellingSize = float('NaN');
            
        else:
            # String with size value
            strSize = content[indexDelimSize1[0][1] : indexDelimSize1[0][1] + delimDf.sizeOffset[0]];
            # Filter digits
            strSize = strTools.FilterDigits(strSize);
            # Convert to float
            dwellingSize = float(strSize);
            
            # Convert to sqm if needed
            if(delimDf.sizeUnit[0] == "sqft"):
                dwellingSize = 0.092903 * dwellingSize;
        
        # Returns size
        return dwellingSize;
            
    
    ##### Case delimiter1 + delimiter2 #####
    if((delimDf.delimSize1[0] and delimDf.delimSize2[0]) != 'NaN_String'):
        
        # Get positions of delimiters in content
        indexDelimSize1 = strTools.GetExprPos(content, delimDf.delimSize1[0]);
        indexDelimSize2 = strTools.GetExprPos(content, delimDf.delimSize2[0]);
        
        # Check if index is empty
        if(not(indexDelimSize1) or not(indexDelimSize2)):
            # Size becomes nan value
            dwellingSize = float('NaN');
        
        else:
            # String with size value
            strSize = content[indexDelimSize1[0][1] : indexDelimSize2[0][0]];
            # Filter digits and decimal
            strSize = strTools.FilterDecimalDigits(strSize);
            strSize = strSize.replace(',', '.');
            # Convert to float
            dwellingSize = float(strSize);
            
            # Convert to sqm if needed
            if(delimDf.sizeUnit[0] == "sqft"):
                dwellingSize = 0.092903 * dwellingSize;
            
        # Returns size
        return dwellingSize;
        
        
# Exception size data
def GetExceptionSizeData(content, delimDf):
        
    # OLX PK EXCEPTION
    if(delimDf.delimSizeException[0] == "olxPkException"):
        
        # OLX Pk website contains several units for areas
        # Check which one for current ad and affect the right multiplicator to convert it to sqm
        multiplicator = float('NaN');        
        
        if(content.find('Square Feet										</a>') != -1):
            multiplicator = 0.092903;
            
        if(content.find('Square Yards										</a>') != -1):
            multiplicator = 0.8361274;
            
        if(content.find('Marla										</a>') != -1):
            multiplicator = 25.2929;
            
        if(content.find('Kanal										</a>') != -1):
            multiplicator = 501.6362851;
            
        # Get position of delimiSize1 in content
        indexDelimSize1 = strTools.GetExprPos(content, delimDf.delimSize1[0]);
        
        # Check if index is empty
        if(not(indexDelimSize1)):         
            # Size becomes nan value
            dwellingSize = float('NaN');
            
        else:
            # String with size value
            strSize = content[indexDelimSize1[0][1] : indexDelimSize1[0][1] + delimDf.sizeOffset[0]];
            # Filter digits
            strSize = strTools.FilterDecimalDigits(strSize);
            strSize = strSize.replace(',', '');
            # Convert to float
            dwellingSize = float(strSize) * multiplicator;
            
        return dwellingSize;
        
        
# COMPUTE DATA PER SQM
def ComputeSqmPrice(price, dwSize):

    # If dwSize is NULL then return nan
    if(dwSize == 0):
        
        if(type(price) is tuple):
            return (float('NaN'), price[1]);
        else:            
            return float('NaN');
        
    else:
        if(type(price) is tuple):
            return (float(price[0]) / float(dwSize), price[1]);
        else:
            return float(price) / float(dwSize);
            
            
        
        
            
    
                
            
            
        
        
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
