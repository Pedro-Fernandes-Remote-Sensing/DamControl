#' Creates folders for each individual ZOI;
#'
#' @param FolderDestinationPath a destination path to the folders being created, as a string;
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;
#'
#' @return returns nothing, used to create folders;
#' @export
CreateIndividualFolders = function(FolderDestinationPath, ID_list){
  StartTime = Sys.time()
  for (ID in ID_list){
    dir.create(paste0(FolderDestinationPath, "/ZOI_", ID))
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating ZOI_X Folders (HH:MM:SS): "), noquote(TotalTime)))
}

#' Creates folders for each individual ZOI, with threshold imbued in the folder name;
#'
#' @param FolderDestinationPath a destination path to the folders being created, as a string;
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;
#' @param Threshold_List a list of thresholds, usually just one, representing the threshold for either the NDVI or RGBSum, in order to create properly named folders;
#'
#' @return returns nothing, used to create folders;
#' @export
CreateIndividualFolders2 = function(FolderDestinationPath, ID_list, Threshold_List){
  StartTime = Sys.time()
  for (Threshold in Threshold_List){
    for (ID in ID_list){
      dir.create(paste0(FolderDestinationPath, "/ZOI_", ID, "_Threshold_", Threshold))
    }
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating ZOI_X_Threshold_X Folders (HH:MM:SS): "), noquote(TotalTime)))
}

#' Creates folders for each individual ZOI, with both thresholds imbued in the folder name;
#'
#' @param FolderDestinationPath a destination path to the folders being created, as a string;
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;
#' @param Threshold_List a list of thresholds, usually just one, representing the threshold for either the NDVI or RGBSum, in order to create properly named folders;
#' @param Threshold_List_2 a list of thresholds, usually just one, representing the threshold for either the NDVI or RGBSum, in order to create properly named folders; Obviously use different thresholds for these two arguments;
#'
#' @return returns nothing, used to create folders;
#' @export
CreateIndividualFolders3 = function(FolderDestinationPath, ID_list, Threshold_List, Threshold_List_2){
  StartTime = Sys.time()
  for (Threshold2 in Threshold_List_2){
    for (Threshold in Threshold_List){
      for (ID in ID_list){
        dir.create(paste0(FolderDestinationPath, "/ZOI_", ID, "_Threshold_", Threshold, "_Threshold_", Threshold2))
      }
    }
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating ZOI_X_Threshold_X_Threshold_X Folders (HH:MM:SS): "), noquote(TotalTime)))
}
