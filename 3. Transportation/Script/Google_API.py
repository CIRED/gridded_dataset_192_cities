# -*- coding: utf-8 -*-
"""
Created on Mon Feb 26 17:16:57 2018

@author: Quentin LEPETIT
"""

# Import libraries
import os
import sys
import datetime
import pickle


# Google API class
class GoogleAPI:
    
    # Every useful Google API keys caracteristics
    # - Number of available keys : keyNumber
    # - Number of evaluations per key : maxKeyEvalNumber
    # - Number of evaluations per request : maxEvalPerRequest
    
    # Constructor
    def __init__(self, path_GoogleAPI, key, keyDataFile):
        
        print('    Initializing Google API keys...')        
        
        # Path to the folder
        self.path_GoogleAPI = path_GoogleAPI;
        
        # Global parameters for one key (Once a month)
        self.maxKeyEvalNumber = 30000;
        self.maxEvalPerRequest = 100;
        self.maxRequestPerKey = self.maxKeyEvalNumber / self.maxEvalPerRequest;
        
        # Specific keys to be used
        self.key = key;
        self.keyNumber = len(key);
        
        # Specific key data file
        self.keyDataFile = keyDataFile;
        
        # Create or load key data file     
        # If it does not exist, it's created with a warning
        if os.path.exists(self.path_GoogleAPI + 'Script/' + self.keyDataFile) == False:
            
            keyUseDate = [0] * self.keyNumber;
            keyUseEval = [0] * self.keyNumber;
            keyReinitDate = [0] * self.keyNumber;
            
            for i in range(0, self.keyNumber):
                
                keyUseDate[i] = datetime.datetime.now();
                keyUseEval[i] = self.maxKeyEvalNumber;
                # Reinit key 31 days after the last use
                keyReinitDate[i] = datetime.datetime.now() + datetime.timedelta(days = 32);
            
            self.keyUseDate = keyUseDate;
            self.keyUseEval = keyUseEval;
            self.keyReinitDate = keyReinitDate;
            
            # Save results
            f = open(self.path_GoogleAPI + 'Script/' + self.keyDataFile, 'wb');
            pickle.dump([keyUseDate, keyUseEval, keyReinitDate], f);
            f.close();
            print('    -> WARNING : Key datetime log could not be found and was created with exact today date and full evaluations available. It might cause errors if it was deleted');
            
            del i;
            del f;
            del keyUseDate;
            del keyUseEval;
            
        else:
            
            # Load key use data and evaluations left
            f = open(self.path_GoogleAPI + 'Script/' + self.keyDataFile, 'rb');
            keyUseTemp = pickle.load(f);
            
            self.keyUseDate = keyUseTemp[0];
            self.keyUseEval = keyUseTemp[1];
            self.keyReinitDate = keyUseTemp[2];
            del keyUseTemp;
            
            f.close();
            
            # Check if keys can be reinitialized
            self.CheckReinitialization();
            print('');
            
    
    # Check keys reinitialization
    def CheckReinitialization(self):
        
        # Actual time
        timeNow = datetime.datetime.now();
        
        # Check keys validity
        for indexKey in range(0, self.keyNumber):
            
            # Update reinitialization date to actual date if the key has not been used
            if(self.keyUseEval[indexKey] == self.maxKeyEvalNumber):
                self.keyReinitDate[indexKey] = timeNow + datetime.timedelta(days = 32);
                print('    -> Key ' + str(indexKey+1) + ' has not been used. Reinitialization date postponed to ' + str(self.keyReinitDate[indexKey]));
            
            # Reinitialize if more than 32 days
            elif(timeNow > self.keyReinitDate[indexKey]):
                self.keyReinitDate[indexKey] = timeNow + datetime.timedelta(days = 32);
                self.keyUseEval[indexKey] = self.maxKeyEvalNumber;
                print('    -> Key ' + str(indexKey+1) + ' has been reinitialized. Reinitialization date set to ' + str(self.keyReinitDate[indexKey]));
                
            else:
                print('    -> Key ' + str(indexKey+1) + ' : ' + str(self.keyUseEval[indexKey]) + ' evaluations left. Reinitialization date set to ' + str(self.keyReinitDate[indexKey]));
                    
                    
    # Update key eval and date
    def UpdateKey(self, indexKey, numberEvalDone):
        
        self.keyUseDate[indexKey] = datetime.datetime.now();
        self.keyUseEval[indexKey] = self.keyUseEval[indexKey] - numberEvalDone;
        
        
    # Save key eval and date
    def SaveKey(self, indexKey):
        
        # Load key use data and evaluations left
        f = open(self.path_GoogleAPI + 'Script/' + self.keyDataFile, 'rb');
        keyUseTemp = pickle.load(f);
        keyUseDate = keyUseTemp[0];
        keyUseEval = keyUseTemp[1];
        keyReinitDate = keyUseTemp[2];
        del keyUseTemp;
        f.close();
        
        # Replacing value for key number indexKey
        keyUseDate[indexKey] = self.keyUseDate[indexKey];
        keyUseEval[indexKey] = self.keyUseEval[indexKey];
        
        # Save results
        f = open(self.path_GoogleAPI + 'Script/' + self.keyDataFile, 'wb');
        pickle.dump((keyUseDate, keyUseEval, keyReinitDate), f);
        f.close();
        
        
    # Choose a valid key
    def ChooseValidKey(self, numberEvalToDo):
        
        indexKey = 0;        
        
        while(indexKey <= (self.keyNumber - 1)):
            
            if (self.keyUseEval[indexKey] >= numberEvalToDo):
                
                return (indexKey, self.key[indexKey]);
                
            else:
                
                indexKey = indexKey + 1;
    
    
    # Check if enough evaluations to evaluate the whole project
    def CheckEvaluationPossibility(self, actualEvalPerRequest, totalEvalNeeded):
        
        totalEvalAvailable = 0;
        
        for i in range(0, self.keyNumber):
            
            if (self.keyUseEval[i] >= actualEvalPerRequest):
                
                totalEvalAvailable = totalEvalAvailable + self.keyUseEval[i];
                
        # Check if enough evaluations available
        if (totalEvalAvailable >= totalEvalNeeded):
            
            print('    -> Enough evaluations available (' + str(totalEvalAvailable) + '/' + str(totalEvalNeeded) + ')\n');
            
        else:
            
            sys.stderr.write('       ERROR : Not enough evaluations available to evaluate the whole project (' + str(totalEvalAvailable) + '/' + str(totalEvalNeeded) + '). ')
            sys.exit(-1);
