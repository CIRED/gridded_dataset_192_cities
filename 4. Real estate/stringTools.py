# -*- coding: utf-8 -*-
"""
Created on Tue Sep 25 10:53:01 2018

@author: quentin
"""

###############################################################################
# FUNCTIONS TO MANIPULATE STRINGS
###############################################################################

from re import finditer
import sys


# GET INDEXES OF AN EXPRESSION INSIDE A STRING
def GetExprPos(string, expr):
    
    # List containing the indexes
    exprIndex = list();
        
    for match in finditer(expr, string):
            
        exprIndex.append(match.span());
    
    return exprIndex
    
    
# KEEP ONLY DIGITS IN A STRING
def FilterDigits(string):
    
    return filter(type(string).isdigit, string);
    
    
# KEEP ONLY DIGITS AND DECIMAL POINT IN STRING
def FilterDecimalDigits(string):
    
    return filter(lambda x: x in '-,.0123456789', string);


# FIX EXCEPTIONS IN URL STRINGS
def FixException(exception, string):
    
    # Check exception
    if(exception == "onlyDigits"):
        
        string = FilterDigits(string);
        return string;
        
    if(exception == "addHtmlExtent"):
        
        string = string + '.html';
        return string;
        
    if(exception == "dothiAdURL"):
        
        index = GetExprPos(string, '" href="/');
        string = string[index[0][1] : len(string)];
        return string
        
    # Case exception is not valid or NULL          
    sys.stderr.write("Exception given for URL - " + exception + " is not valid or NULL");
    sys.exit(-1);


# SPLIT EXPRESSION IN FUNCTION OF A SEPARATOR
def SplitString(string, separator):
    
    try:
        # Get position of the separator
        indexSep = GetExprPos(string, separator);
        # Get split string in a list
        splitString = [string[0:indexSep[0][0]], string[indexSep[0][1]:len(string)]];
        return splitString;
    except:
        splitString = ['NaN', 'NaN'];
        return splitString;
        
        
# CONVERT NAN TO EMPTY STRING IN A DATAFRAME
def FormatNaNToString(dataFrame):
    
    # Loop over the elements of the dataframe
    for i in dataFrame:
        
        # Check if nan and not int
        if(not(type(dataFrame[i][0]) is str) and not(type(dataFrame[i][0]) is int)):
            
            dataFrame[i][0] = 'NaN_String';
            
    return dataFrame;
    


