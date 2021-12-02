###############################################################################
# PROCESS ESACCI LAND COVER DATA WITH CITY GRID
###############################################################################


# Third faster version
AggregateESACCIDataToGrid = function(cityDataDf, gridSf, gridXesacciSf, esacciYear)
{
  # Order dataframes
  gridSf = gridSf[order(gridSf$ID, decreasing = FALSE),];
  gridXesacciSf = gridXesacciSf[order(gridXesacciSf$ID, decreasing = FALSE),];
  
  # Convert areas to numeric
  gridXesacciSf$areaESACCI = as.numeric(gridXesacciSf$areaESACCI);
  gridXesacciSf$areaX = as.numeric(gridXesacciSf$areaX);
  
  # Convert cat to string
  gridXesacciSf$cat = as.character(gridXesacciSf$cat);
  
  # Copy gridSf to initialize gridESACCISf
  gridESACCISf = gridSf;
  
  # Add one column per land cover category
  catList = c(10, 11, 12, 14, 20, 30, 40, 50, 60, 61, 62, 70, 71, 72, 80, 81, 82, 90, 100, 110, 120, 121, 122, 130, 140, 150, 151, 152, 153, 160, 170, 180, 190, 200,
              201, 202, 210, 220, 230);
  
  for(iCat in (1:length(catList)))
  {
    gridESACCISf[, iCat + 5] = 0;
    names(gridESACCISf)[iCat + 5] = paste('ESACCI', catList[iCat], sep = '');
  }
  
  # Total
  gridESACCISf$ESACCI_tot = 0;
  
  # Dataframe of unique grid pixels intersected
  uGridIDDf = data.frame(ID = unique(gridXesacciSf$ID));
  
  # Initialize GUI progress bar
  progressBar = tkProgressBar(title = paste('Processing ESACCI - ', esacciYear, ' grid data ...' , sep = ''),
                              min = 0,
                              max = length(uGridIDDf),
                              width = 300);
  
  barCount = 0;
  
  # Double loop over IDs
  for(i in uGridIDDf$ID)
  {
    # Bar count update
    barCount = barCount + 1;
    
    # Get index of unique value i
    uGridIndex = which(gridXesacciSf$ID == i);
    
    for(j in uGridIndex)
    {
      if(gridSf$ID[i] == gridXesacciSf$ID[j])
      {
        # Switch over categories
        switch(gridXesacciSf$cat[j],
               '10' = {gridESACCISf$ESACCI10[i] = gridESACCISf$ESACCI10[i] + gridXesacciSf$areaX[j]},
               '11' = {gridESACCISf$ESACCI11[i] = gridESACCISf$ESACCI11[i] + gridXesacciSf$areaX[j]},
               '12' = {gridESACCISf$ESACCI12[i] = gridESACCISf$ESACCI12[i] + gridXesacciSf$areaX[j]},
               '14' = {gridESACCISf$ESACCI14[i] = gridESACCISf$ESACCI14[i] + gridXesacciSf$areaX[j]},
               '20' = {gridESACCISf$ESACCI20[i] = gridESACCISf$ESACCI20[i] + gridXesacciSf$areaX[j]},
               '30' = {gridESACCISf$ESACCI30[i] = gridESACCISf$ESACCI30[i] + gridXesacciSf$areaX[j]},
               '40' = {gridESACCISf$ESACCI40[i] = gridESACCISf$ESACCI40[i] + gridXesacciSf$areaX[j]},
               '50' = {gridESACCISf$ESACCI50[i] = gridESACCISf$ESACCI50[i] + gridXesacciSf$areaX[j]},
               '60' = {gridESACCISf$ESACCI60[i] = gridESACCISf$ESACCI60[i] + gridXesacciSf$areaX[j]},
               '61' = {gridESACCISf$ESACCI61[i] = gridESACCISf$ESACCI61[i] + gridXesacciSf$areaX[j]},
               '62' = {gridESACCISf$ESACCI62[i] = gridESACCISf$ESACCI62[i] + gridXesacciSf$areaX[j]},
               '70' = {gridESACCISf$ESACCI70[i] = gridESACCISf$ESACCI70[i] + gridXesacciSf$areaX[j]},
               '71' = {gridESACCISf$ESACCI71[i] = gridESACCISf$ESACCI71[i] + gridXesacciSf$areaX[j]},
               '72' = {gridESACCISf$ESACCI72[i] = gridESACCISf$ESACCI72[i] + gridXesacciSf$areaX[j]},
               '80' = {gridESACCISf$ESACCI80[i] = gridESACCISf$ESACCI80[i] + gridXesacciSf$areaX[j]},
               '81' = {gridESACCISf$ESACCI81[i] = gridESACCISf$ESACCI81[i] + gridXesacciSf$areaX[j]},
               '82' = {gridESACCISf$ESACCI82[i] = gridESACCISf$ESACCI82[i] + gridXesacciSf$areaX[j]},
               '90' = {gridESACCISf$ESACCI90[i] = gridESACCISf$ESACCI90[i] + gridXesacciSf$areaX[j]},
               '100' = {gridESACCISf$ESACCI100[i] = gridESACCISf$ESACCI100[i] + gridXesacciSf$areaX[j]},
               '110' = {gridESACCISf$ESACCI110[i] = gridESACCISf$ESACCI110[i] + gridXesacciSf$areaX[j]},
               '120' = {gridESACCISf$ESACCI120[i] = gridESACCISf$ESACCI120[i] + gridXesacciSf$areaX[j]},
               '121' = {gridESACCISf$ESACCI121[i] = gridESACCISf$ESACCI121[i] + gridXesacciSf$areaX[j]},
               '122' = {gridESACCISf$ESACCI122[i] = gridESACCISf$ESACCI122[i] + gridXesacciSf$areaX[j]},
               '130' = {gridESACCISf$ESACCI130[i] = gridESACCISf$ESACCI130[i] + gridXesacciSf$areaX[j]},
               '140' = {gridESACCISf$ESACCI140[i] = gridESACCISf$ESACCI140[i] + gridXesacciSf$areaX[j]},
               '150' = {gridESACCISf$ESACCI150[i] = gridESACCISf$ESACCI150[i] + gridXesacciSf$areaX[j]},
               '151' = {gridESACCISf$ESACCI151[i] = gridESACCISf$ESACCI151[i] + gridXesacciSf$areaX[j]},
               '152' = {gridESACCISf$ESACCI152[i] = gridESACCISf$ESACCI152[i] + gridXesacciSf$areaX[j]},
               '153' = {gridESACCISf$ESACCI153[i] = gridESACCISf$ESACCI153[i] + gridXesacciSf$areaX[j]},
               '160' = {gridESACCISf$ESACCI160[i] = gridESACCISf$ESACCI160[i] + gridXesacciSf$areaX[j]},
               '170' = {gridESACCISf$ESACCI170[i] = gridESACCISf$ESACCI170[i] + gridXesacciSf$areaX[j]},
               '180' = {gridESACCISf$ESACCI180[i] = gridESACCISf$ESACCI180[i] + gridXesacciSf$areaX[j]},
               '190' = {gridESACCISf$ESACCI190[i] = gridESACCISf$ESACCI190[i] + gridXesacciSf$areaX[j]},
               '200' = {gridESACCISf$ESACCI200[i] = gridESACCISf$ESACCI200[i] + gridXesacciSf$areaX[j]},
               '201' = {gridESACCISf$ESACCI201[i] = gridESACCISf$ESACCI201[i] + gridXesacciSf$areaX[j]},
               '202' = {gridESACCISf$ESACCI202[i] = gridESACCISf$ESACCI202[i] + gridXesacciSf$areaX[j]},
               '210' = {gridESACCISf$ESACCI210[i] = gridESACCISf$ESACCI210[i] + gridXesacciSf$areaX[j]},
               '220' = {gridESACCISf$ESACCI220[i] = gridESACCISf$ESACCI220[i] + gridXesacciSf$areaX[j]},
               '230' = {gridESACCISf$ESACCI230[i] = gridESACCISf$ESACCI230[i] + gridXesacciSf$areaX[j]}
               )
      }
    }
    
    # Update progress bar
    setTkProgressBar(progressBar, barCount, label = paste(year, ': ', round((barCount/length(uGridIDDf$ID)) * 100, 1), '% completed', sep = ''));
  }
  
  # Clear progress bar
  close(progressBar);
  
  # Return gridGHSL
  return(gridESACCISf)
}
