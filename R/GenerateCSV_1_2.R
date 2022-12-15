GenerateCSV = function(Albufeira_List_of_Lists, ExportPath, Year, ID_list, ProductName, Threshold1){
  StartTime = Sys.time()
  MonthNames = list("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")
  Iterator =  1
  Albufeira_DataframeList = list()
  for (Albufeira in Albufeira_List_of_Lists) {
    AlbufeiraDataframe = data.frame(Year = integer(), Month = character(), WaterPixels = integer(), NotWaterPixels = integer(), TotalPixel = integer(), TotalWaterAreaM2 = integer(), TotalAreaM2 = integer(), WaterPercentage = integer())
    MonthNamesIterator = 1
    for (BinaryNDVI in Albufeira) {
      WaterCells = cellStats(BinaryNDVI == 1, sum)
      NonWaterCells = cellStats(BinaryNDVI == 0, sum)
      TotalCells = WaterCells + NonWaterCells
      TotalWaterAreaM2 = WaterCells*100
      TotalAreaM2 = TotalCells*100
      WaterPercentage = round(((WaterCells/TotalCells)*100), digits = 2)
      AlbufeiraDataframe[nrow(AlbufeiraDataframe) + 1,] = list(Year, MonthNames[[MonthNamesIterator]],WaterCells,NonWaterCells, TotalCells, TotalWaterAreaM2, TotalAreaM2, WaterPercentage)
      MonthNamesIterator = MonthNamesIterator + 1
    }
    Albufeira_DataframeList[[Iterator]] = AlbufeiraDataframe
    write.csv2(AlbufeiraDataframe, paste0(ExportPath, "/csv_Albufeira_", ID_list[[Iterator]], "_", ProductName, "_Threshold_", Threshold1,  ".csv"), row.names = FALSE, quote = FALSE)
    print(noquote(paste0("Finished writing csv for Albufeira ", ID_list[[Iterator]])))
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Finished writing csv for all Albufeiras")))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent generating CSVs (HH:MM:SS): "), noquote(TotalTime)))
  return(Albufeira_DataframeList)
}

GenerateCSV2 = function(Albufeira_List_of_Lists, ExportPath, Year, ID_list, ProductName, Threshold1, Threshold2){
  StartTime = Sys.time()
  MonthNames = list("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")
  Iterator =  1
  Albufeira_DataframeList = list()
  for (Albufeira in Albufeira_List_of_Lists) {
    AlbufeiraDataframe = data.frame(Year = integer(), Month = character(), WaterPixels = integer(), NotWaterPixels = integer(), TotalPixel = integer(), TotalWaterAreaM2 = integer(), TotalAreaM2 = integer(), WaterPercentage = integer())
    MonthNamesIterator = 1
    for (BinaryNDVI in Albufeira) {
      WaterCells = cellStats(BinaryNDVI == 1, sum)
      NonWaterCells = cellStats(BinaryNDVI == 0, sum)
      TotalCells = WaterCells + NonWaterCells
      TotalWaterAreaM2 = WaterCells*100
      TotalAreaM2 = TotalCells*100
      WaterPercentage = round(((WaterCells/TotalCells)*100), digits = 2)
      AlbufeiraDataframe[nrow(AlbufeiraDataframe) + 1,] = list(Year, MonthNames[[MonthNamesIterator]],WaterCells,NonWaterCells, TotalCells, TotalWaterAreaM2, TotalAreaM2, WaterPercentage)
      MonthNamesIterator = MonthNamesIterator + 1
    }
    Albufeira_DataframeList[[Iterator]] = AlbufeiraDataframe
    write.csv2(AlbufeiraDataframe, paste0(ExportPath, "/csv_Albufeira_", ID_list[[Iterator]], "_", ProductName, "_ThresholdNDVI_", Threshold1, "_ThresholdRGBSum_", Threshold2,  ".csv"), row.names = FALSE, quote = FALSE)
    print(noquote(paste0("Finished writing csv for Albufeira ", ID_list[[Iterator]])))
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Finished writing csv for all Albufeiras")))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent generating CSVs (HH:MM:SS): "), noquote(TotalTime)))
  return(Albufeira_DataframeList)
}
