# -*- coding: utf-8 -*-
"""
Created on Mon Feb 26 11:09:32 2018

@author: Quentin LEPETIT
"""

###############################################################################
# GOOGLE API TRANSPORT SCRIPT
# Get transport data from Google API
# Special script for Hubli-Dharwad BRTS project
# CENTERS -> GRID POINTS
###############################################################################


# Clear workspace
def clearall():
    all = [var for var in globals() if var[0] != "_"]
    for var in all:
        del globals()[var]
        
clearall()


# Loading libraries
import os;
import sys
sys.path.append('/home/quentin/Documents/Google_API_Program/Script')
import datetime;
import time
import numpy as np
import math
import dataProcessing as data
import pickle
import Google_API
import urllib2
import json
import keyDatabase
import pandas
import dplython as dp


###############################################################################
# Beginning of the script
###############################################################################

print("*** GOOGLE API Script Program : Centers -> Grid Points ***\n");

# Path to Google API folder
path_GoogleAPI = '/home/quentin/Documents/Google_API_Program/';
os.chdir(path_GoogleAPI);


###############################################################################
# Project caracteristics
###############################################################################

# Create key object
API = Google_API.GoogleAPI('./', keyDatabase.key, keyDatabase.keyDataFile);


###############################################################################
# Loops over projects
###############################################################################

# Load cities database
cityDatabaseDf = pandas.read_csv('./City_dataStudy/cityDatabase.csv')
# Convert to Dplyframe object
cityDatabaseDf = dp.DplyFrame(cityDatabaseDf);
# Select useful columns and filter rows without transport source
cityDatabaseDf = (cityDatabaseDf >> dp.select(dp.X.City, dp.X.Country, dp.X.Continent, dp.X.GridEPSG, dp.X.TransportSource, dp.X.RushHour, dp.X.UTC_Jan) >> 
                 dp.sift(dp.X.TransportSource != 'Google', dp.X.TransportSource != 'Baidu'));

cityDatabaseDf = cityDatabaseDf.reset_index();
                 
# START FROM BENIN CITY NIGERIA
cityDatabaseDf = cityDatabaseDf[21:len(cityDatabaseDf)];
cityDatabaseDf = cityDatabaseDf.reset_index();


##################
# Loop over cities
for i in range(0, len(cityDatabaseDf)):

    # Change directory    
    os.chdir(path_GoogleAPI);
    
    # Country and project names
    countryName = cityDatabaseDf.Country[i];
    cityName = cityDatabaseDf.City[i];
    # Grid original projection
    gridEPSG = str(cityDatabaseDf.GridEPSG[i]);
    # Transport type (VP or TC)
    transportType = 'VP';
    # Grid and station names
    gridName = 'googleGrid_' + cityName + '_' + gridEPSG + '_135_V3' + '.csv';
    centerName = 'googleCenter_' + cityName + '_' + gridEPSG + '.csv';
    # Rush hour
    rushHourStr = cityDatabaseDf.RushHour[i];
    rushHourList = {'rushHour' : [datetime.datetime(2020, 4, 2, int(rushHourStr[0:2]), int(rushHourStr[3:5]))]};
    rushHourDf = pandas.DataFrame(data = rushHourList);

    
    ###############################################################################
    # Import data
    ###############################################################################
    
    print('    Import data...');
    
    # /!\ Coordinates are in lat/long in Google API, but in the .csv files they are givin in long/lat
    
    # Import the grid (Destination)
    gridFile = './Project/' + countryName + '/' + cityName + '/' + gridName;
    dataGrid = data.importCsv(gridFile);
    # Getting the long/lat columns
    M = np.concatenate((dataGrid[:,5:6], dataGrid[:,6:7]), axis = 1);
    # Grid coord in selected EPSG
    gridCoord = np.append(dataGrid[:,3:4], dataGrid[:,4:5], axis = 1);
    
    print('    -> Grid imported : ' + str(len(M[:,0])) + ' grid pixels');
    
    
    # Import centers (Origin)
    centersFile = './Project/' + countryName + '/' + cityName + '/' + centerName;
    centers = data.importCsv(centersFile).reshape((-1, 5));
    # Getting centers ID
    IDCenter = centers[:,0];
    # Getting the long/lat columns
    centers = np.concatenate((centers[:,3:4], centers[:,4:5]), axis = 1);
    # Convert coordinates in the good string shape for Google API to use them
    centersAPI = data.googleencodepolyline(centers[:,1], centers[:,0]);
    
    print('    -> Centers imported : ' + str(len(centers[:,0])) + ' centers\n');
        
        
    # Convert city rush hours in timestamp including UTC offset
    cityRushHourDf = rushHourDf;
    cityRushHourDf = (cityRushHourDf >> dp.mutate(UTC_RushHour = dp.X.rushHour - datetime.timedelta(hours = cityDatabaseDf.UTC_Jan[i])) >>
                                          dp.mutate(rushHourTimestamp = (dp.X.UTC_RushHour - datetime.datetime(1970, 1, 1))))
    
    # Time of evaluation
    transportTime = rushHourStr;
    
    # Format transport time to fit HHhMM format
    if(len(transportTime) == 4):
        transportTime = transportTime + '0';        
    
    # Timestamp time of evaluation
    transportTimestamp = int(cityRushHourDf.rushHourTimestamp[0].total_seconds());
    transportTimeDate = datetime.datetime.isoformat(cityRushHourDf.rushHour[0], '-');
    
    print('    Get transport data for ' + cityName + ' (Date : ' + transportTimeDate + ')\n');
    
    
    ###############################################################################
    # Setting URL parts
    ###############################################################################
    
    print('    Setting URL parts...');
    
    # Output data type (JSON), metric system and origin - destination
    URL1 = 'https://maps.googleapis.com/maps/api/distancematrix/json?' + 'units=metric&origins=enc:';
    print('    -> URL1 set');
    
    URL2 = ':&destinations=enc:';
    print('    -> URL2 set');
    
    # Choosing transport mode (VP or TC)
    # Case it is VP
    if (transportType == 'VP'):
        
        transportMode = '&traffic_model=pessimistic&key=';
        
    elif (transportType == 'TC'):
        
        transportMode = '&mode=transit&key=';
        
    else:
            
            sys.stderr.write('    -> ERROR in getting the transport Mode : Transport type given is not valid. It must be "VP" or "TC".');
            sys.exit(-1);
            
    # Set departure time with timestamp
    URL3 = ':&departure_time=' + str(transportTimestamp) + transportMode;
    print('    -> URL3 set for transport mode ' + transportType);
    
    
    ###############################################################################
    # Key and evaluations caracteristics for the project
    ###############################################################################
    
    # Specific number of evaluations per requests
    actualEvalPerRequest = len(centers[:,0]) * math.floor(API.maxEvalPerRequest / len(centers[:,0]));
    actualKeyEvalNumber = actualEvalPerRequest * math.floor(API.maxKeyEvalNumber / actualEvalPerRequest);
    numberKeyNeeded = math.ceil((len(M) * len(centers[:,0])) / actualKeyEvalNumber);
    totalEvalNeeded = len(M) * len(centers[:,0]);
    # Convert previous variables into integers
    actualEvalPerRequest = int(actualEvalPerRequest);
    actualKeyEvalNumber = int(actualKeyEvalNumber);
    numberKeyNeeded = int(numberKeyNeeded);
    totalEvalNeeded = int(totalEvalNeeded);
    
    # Check if evaluation is possible
    API.CheckEvaluationPossibility(actualEvalPerRequest, totalEvalNeeded);
        
    
    ###############################################################################
    # Getting data
    ###############################################################################
    
    print('    Getting data...');
    
    # The list that will contain the json data
    A = list();
    
    # Beginning of the evaluation loop
    initLoop = 0;
    # End of the loop is equal to the number of request needed (of actualEvalPerRequest evaluations each)
    endLoop = math.modf(float(totalEvalNeeded) / actualEvalPerRequest);
    
    if (endLoop[0] == 0):
        endLoop = endLoop[1];
        
    else:
        endLoop = endLoop[1] + 1;
    
    # Convert endLoop to int
    endLoop = int(endLoop);
        
    # Beginning of the loop over the number of request
    # Loading data from Google Distance Matrix API
    for index in range(initLoop, endLoop):
        
        if (len(M) >= (index * actualEvalPerRequest + actualEvalPerRequest) / len(centers[:,0])):
            
            try:
                
                # Regular case
                
                # Getting a valid key
                (indexKey, URL4) = API.ChooseValidKey(actualEvalPerRequest);
                
                AA = data.googleencodepolyline(M[(index * actualEvalPerRequest)/len(centers[:,0]) : (index * actualEvalPerRequest + actualEvalPerRequest)/len(centers[:,0]), 1], \
                                               M[(index * actualEvalPerRequest)/len(centers[:,0]) : (index * actualEvalPerRequest + actualEvalPerRequest)/len(centers[:,0]), 0]);
                URL = URL1 + centersAPI + URL2 + AA + URL3 + URL4;
                requestAPI = urllib2.urlopen(URL);
                content = requestAPI.read();
                A.append(json.loads(content));
                
                # Updating and saving key data
                API.UpdateKey(indexKey, actualEvalPerRequest);
                API.SaveKey(indexKey);
                
                if (A[index]['status'] == 'OVER_QUERY_LIMIT'):
                    
                    API.UpdateKey(indexKey, API.keyUseEval[indexKey]);
                    API.SaveKey(indexKey);
                    
                    sys.stderr.write('    -> ERROR : Key evaluation limit reached for key ' + str(indexKey+1) + ' : ' + API.key[indexKey] + '. ');        
                    sys.exit(-1);
                
                if (A[index]['status'] == 'REQUEST_DENIED'):
                    
                    API.UpdateKey(indexKey, API.keyUseEval[indexKey]);
                    API.SaveKey(indexKey);
                    
                    sys.stderr.write('    -> ERROR : API key ' + str(indexKey+1) + ' : ' + API.key[indexKey] + ' is invalid. It might not be activated for this API or not valid at all. ')
                    sys.exit(-1);
                    
                if (A[index]['status'] == 'UNKNOWN_ERROR'):
                    
                    API.UpdateKey(indexKey, API.keyUseEval[indexKey]);
                    API.SaveKey(indexKey);
                    
                    sys.stderr.write('    -> ERROR : API key ' + str(indexKey+1) + ' : ' + API.key[indexKey] + ' returned an unknown error. Try again later but turned to 0 evaluations for today. ')
                    sys.exit(-1);
                
                time.sleep(1);
            
            except:
            
                sys.stderr.write('    -> ERROR in downloading data from Google_API. It might be caused by an internet network problem or by ' \
                                 + 'an unexpected error on the key  ' + str(indexKey+1) + ' : ' + API.key[indexKey] + '. ');
                sys.exit(-1);
                
        else:
                
            try:
                    
                # If the end of the grid is reached
                    
                # Getting a valid key
                (indexKey, URL4) = API.ChooseValidKey((len(M) - (index * actualEvalPerRequest)/len(centers[:,0])) * len(centers[:,0]));
                
                AA = data.googleencodepolyline(M[(index * actualEvalPerRequest)/len(centers[:,0]) : len(M), 1], \
                                               M[(index * actualEvalPerRequest)/len(centers[:,0]) : len(M), 0]);
                URL = URL1 + centersAPI + URL2 + AA + URL3 + URL4;
                requestAPI = urllib2.urlopen(URL);
                content = requestAPI.read();
                A.append(json.loads(content));
                
                # Updating and saving key data
                API.UpdateKey(indexKey, (len(M) - (index * actualEvalPerRequest)/len(centers[:,0])) * len(centers[:,0]));
                API.SaveKey(indexKey);
                
                if (A[index]['status'] == 'OVER_QUERY_LIMIT'):
    
                    sys.stderr.write('    -> ERROR : Key evaluation limit reached for key ' + str(indexKey+1) + ' : ' + API.key[indexKey] + '. ');        
                    sys.exit(-1);
                
                if (A[index]['status'] == 'REQUEST_DENIED'):
                    
                    API.UpdateKey(indexKey, API.keyUseEval[indexKey]);
                    API.SaveKey(indexKey);
                    
                    sys.stderr.write('    -> ERROR : API key ' + str(indexKey+1) + ' : ' + API.key[indexKey] + ' is invalid. It might not be activated for this API or not valid at all. ')
                    sys.exit(-1);
                    
                if (A[index]['status'] == 'UNKNOWN_ERROR'):
                    
                    API.UpdateKey(indexKey, API.keyUseEval[indexKey]);
                    API.SaveKey(indexKey);
                    
                    sys.stderr.write('    -> ERROR : API key ' + str(indexKey+1) + ' : ' + API.key[indexKey] + ' returned an unknown error. Try again later but turned to 0 evaluations for today. ')
                    sys.exit(-1);
                
                time.sleep(1);
                    
            except:
                
                sys.stderr.write('    -> ERROR in downloading data from Google_API. It might be caused by an internet network problem or by ' \
                                 + 'an unexpected error on the key  ' + str(indexKey+1) + ' : ' + API.key[indexKey] + '. ');
                sys.exit(-1);
                                
        
    print('    -> Data from Google API downloaded');
                        
    
    # Formatting data
    distance = np.zeros((len(centers), len(M)));
    duration = np.zeros((len(centers), len(M)));
    status = np.zeros((len(centers), len(M)));
    
    # If personal vehicle data
    if (transportType == 'VP'):
        duration_in_traffic = np.zeros((len(centers), len(M)));
        
        
    for index in range(initLoop, endLoop):
        
        if (A[index]['status'] == 'OVER_QUERY_LIMIT'):
    
            sys.stderr.write('    -> ERROR : Key evaluation limit reached. ');        
            sys.exit(-1);
                
        if (A[index]['status'] == 'REQUEST_DENIED'):
            
            sys.stderr.write('    -> ERROR : The provided API key is invalid. It might not be activated for this API or not valid at all. ')
            sys.exit(-1);
            
        if (A[index]['status'] == 'UNKNOWN_ERROR'):
                
            sys.stderr.write('    -> ERROR : The provided API key returned a server error. ')
            sys.exit(-1);
                        
                 
        for index2 in range(0, len(centers[:,0])):
            
            for index3 in range(0, actualEvalPerRequest/len(centers[:,0])):
                
                if (index * (actualEvalPerRequest/len(centers[:,0])) + index3 <= len(M) - 1):
                        
                    if (type(A[index]['rows'][index2]['elements']) is not list):
                        
                        # Case of status is not ok
                        if (len(A[index]['rows'][index2]['elements'][index3]['status']) > 2):
                                
                            distance[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                            duration[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                            status[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = 0;
                                
                            # If personal vehicle data
                            if (transportType == 'VP'):                                
                                duration_in_traffic[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                            
                        # Regular case
                        else:
                                
                            distance[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = A[index]['rows'][index2]['elements'][index3]['distance']['value'];
                            duration[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = A[index]['rows'][index2]['elements'][index3]['duration']['value'];
                            status[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = 1;
                               
                            if (transportType == 'VP'):
                                
                                if (len(A[index]['rows'][index2]['elements'][index3]) == 3):
                                    duration_in_traffic[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                                       
                                else:
                                    duration_in_traffic[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = A[index]['rows'][index2]['elements'][index3]['duration_in_traffic']['value'];
                               
                            
                    else:
                            
                        # Case of status is not ok
                        if (len(A[index]['rows'][index2]['elements'][index3]['status']) > 2):
                                
                            distance[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                            duration[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                            status[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = 0;
                                
                            # If personal vehicle data
                            if (transportType == 'VP'):                                
                                duration_in_traffic[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                            
                        # Regular case
                        else:
                                
                            distance[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = A[index]['rows'][index2]['elements'][index3]['distance']['value'];
                            duration[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = A[index]['rows'][index2]['elements'][index3]['duration']['value'];
                            status[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = 1;
                               
                            if (transportType == 'VP'):
                                
                                if (len(A[index]['rows'][index2]['elements'][index3]) == 3):
                                    duration_in_traffic[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = np.nan;
                                       
                                else:
                                    duration_in_traffic[index2][index * (actualEvalPerRequest/len(centers[:,0])) + index3] = A[index]['rows'][index2]['elements'][index3]['duration_in_traffic']['value'];
                                
                
    print('    -> Data formatted\n');
    
    
    ###############################################################################
    # Save results
    ###############################################################################
    
    os.chdir('./Project/' + countryName + '/' + cityName + '/');
    
    # Choosing transport mode (VP or TC)
    # Case it is VP
    if (transportType == 'VP'):
        
        resultsFile = 'DrivingTimesGoogle_' + gridName.replace('.csv', '') + '_' + transportTime;
        
        distance_VP = distance;
        duration_VP = duration;
        duration_in_traffic_VP = duration_in_traffic;
        status_VP = status;
            
        print('    Saving results...\n');
        
        # Saving variables in pckl file
        f = open(resultsFile + '.pckl', 'wb');
        pickle.dump([gridCoord, distance_VP, duration_VP, duration_in_traffic_VP, status_VP, transportTimeDate], f);
        f.close();
        
        # Saving to csv
        tdistance_VP = np.transpose(distance_VP);
        tduration_VP = np.transpose(duration_VP);
        tduration_in_traffic_VP = np.transpose(duration_in_traffic_VP);
        tstatus_VP = np.transpose(status_VP);
        fileHeader = 'X, Y, distance, duration, durationInTraffic, status';
        
        for j in range(0, len(IDCenter)):
            resultsFile = resultsFile + '_Center' + str(j);
            
            # Saving in .csv
            np.savetxt(resultsFile + '.csv', \
                       np.concatenate((gridCoord, tdistance_VP, tduration_VP, tduration_in_traffic_VP, tstatus_VP), axis = 1), \
                       header = fileHeader, \
                       delimiter = ',', \
                       comments = '');
        
        
    elif (transportType == 'TC'):
        
        resultsFile = 'TransitTimesGoogle_' + gridName.replace('.csv', '') + '_' + transportTime;
        
        distance_TC = distance;
        duration_TC = duration;
        status_TC = status;
            
        print('    -> Saving results...\n');
        f = open(resultsFile + '.pckl', 'wb');
        pickle.dump([gridCoord, distance_TC, duration_TC, status_TC, transportTimeDate], f);
        f.close();
        
        # Saving to csv
        tdistance_TC = np.transpose(distance_TC);
        tduration_TC = np.transpose(duration_TC);
        tstatus_TC = np.transpose(status_TC);
        fileHeader = 'X, Y, distance, duration, status';
        
        for j in range(0, len(IDCenter)):
            resultsFile = resultsFile + '_Center' + str(j);
            
            np.savetxt(resultsFile + '.csv', \
                       np.concatenate((gridCoord, tdistance_TC, tduration_TC, tstatus_TC), axis = 1), \
                       header = fileHeader, \
                       delimiter = ',', \
                       comments = '');
    
        
    else:
            
            sys.stderr.write('    -> ERROR in getting the transport Mode : Transport type given is not valid. It must be "VP" or "TC".');
            sys.exit(-1);
        

###############################################################################
# End of the script
###############################################################################

print('*** End of the script ***');                                   
