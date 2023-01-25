#' Creates some of the folders required to organize outputs in the a certain provided path and prints time spent;
#'
#' @param Year_List a list of integers representing the years to be calculated. Folders will be created based; If Windows based, MAXIMUM CHARACTER SIZE IS 118. Or go to the registry and change the maximum number of characters on a file path;
#' @param OUTPUT_Path a string representing a path where you'd like the folders to be created;
#'
#' @return has no returns. Meant to be used just to create folders;
#' @export
CreateOutputFoldersALL = function(Year_List, OUTPUT_Path){
  for (Year in Year_List){
    StartTime = Sys.time()
    dir.create(paste0(OUTPUT_Path, "/AA_", Year))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/1_NDVI_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/2_Composite_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/3_RGB_Sum_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/4_NDVI_Binary_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/5_RGB_Sum_Binary_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/6_Algorithm_Binary_ZOI_All"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/7_NDVI_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/8_Composite_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/9_RGB_Sum_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/10_NDVI_Binary_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/11_RGB_Sum_Binary_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/12_Algorithm_Binary_ZOI_Individual"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/13_CSV_ZOI_Individual"))
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

