CreateIndividualFolders = function(FolderDestinationPath, ID_list){
  StartTime = Sys.time()
  for (ID in ID_list){
    dir.create(paste0(FolderDestinationPath, "/Albufeira_", ID))
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating Albufeira_X Folders (HH:MM:SS): "), noquote(TotalTime)))
}

CreateIndividualFolders2 = function(FolderDestinationPath, ID_list, Threshold_List){
  StartTime = Sys.time()
  for (Threshold in Threshold_List){
    for (ID in ID_list){
      dir.create(paste0(FolderDestinationPath, "/Albufeira_", ID, "_Threshold_", Threshold))
    }
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating Albufeira_X_Threshold_X Folders (HH:MM:SS): "), noquote(TotalTime)))
}

CreateIndividualFolders3 = function(FolderDestinationPath, ID_list, Threshold_List, Threshold_List_2){
  StartTime = Sys.time()
  for (Threshold2 in Threshold_List_2){
    for (Threshold in Threshold_List){
      for (ID in ID_list){
        dir.create(paste0(FolderDestinationPath, "/Albufeira_", ID, "_Threshold_", Threshold, "_Threshold_", Threshold2))
      }
    }
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating Albufeira_X_Threshold_X_Threshold_X Folders (HH:MM:SS): "), noquote(TotalTime)))
}
