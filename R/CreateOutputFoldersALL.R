#' Creates some of the folders required to organize outputs in the a certain provided path and prints time spent;
#'
#' @param Year_List a list of integers representing the years to be calculated.
#' @param OUTPUT_Path a string representing a path where you'd like the folders to be created: If Windows based, MAXIMUM CHARACTER SIZE IS 118. Or go to the registry and change the maximum number of characters on a file path;
#'
#' @return has no returns. Meant to be used just to create folders;
#' @export
CreateOutputFoldersALL = function(Year_List, OUTPUT_Path){
  for (Year in Year_List){
    StartTime = Sys.time()
    #Create Agricultural Year Folders
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year))
    #Create NDVI folders
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/NDVIs"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/NDVIs", "/NDVI_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/NDVIs", "/NDVI_Binary_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/NDVIs", "/NDVI_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/NDVIs", "/NDVI_Binary_ZOI_Individual"))
    #Create Composites Folders
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/Composites"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/Composites", "/Composite_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/Composites", "/Composite_ZOI_All"))
    #Create RGBSum Folders
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/RGBSums"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/RGBSums", "/RGB_Sum_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/RGBSums", "/RGB_Sum_Binary_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/RGBSums", "/RGB_Sum_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/RGBSums", "/RGB_Sum_Binary_ZOI_Individual"))
    #Create Algorithm Folders
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/Algorithm"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/Algorithm", "/Algorithm_Binary_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/Algorithm", "/Algorithm_Binary_ZOI_Individual"))
    #Create CSVs Folders
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/CSVs"))
    dir.create(paste0(OUTPUT_Path, "/Agricultural_Year_", Year, "/CSVs", "/CSV_ZOI_Individual"))
  }
  dir.create(paste0(OUTPUT_Path, "/ZOI_Individual_Graphs"))
  dir.create(paste0(OUTPUT_Path, "/ZOI_Individual_Graphs_Overlap"))
  dir.create(paste0(OUTPUT_Path, "/ZOI_Individual_Shapefiles"))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating folders (HH:MM:SS): "), noquote(TotalTime)))
}

