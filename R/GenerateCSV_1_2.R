#' Generates a csv file that contains several rows of information, namely number of Water Cells and Non Water Cells, prints time spent and names the files acordding to the Threshold provided. Accepts a list of lists where each element is a list of all 12 months for a specific year and specific ZOI;
#'
#' @param ZOI_List_of_Lists A list of lists, where the first element is a list of all 12 months of a product for ZOI 1; Usually a list of lists of Binary_NDVI cut to each ZOI;
#' @param ExportPath a path to export the .csv files;
#' @param Year a string to represent the agricultural year, ex: 2017 or Ano Agricola 2017
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created as a global variable using CropShapefile_ExportToPath;
#' @param ProductName a Product Name to be printed in the files name, ex: BinaryNDVI or Combo;
#' @param Threshold1 a number representing the threshold used in the files that originate the csv, like 0 for NDVI;
#'
#' @return returns an organized list of dataframes that include a dataframe for each ZOI, representing an entire year of imagery, with stats such as number of Water Cells, Non Water cells, etc;
#' @importFrom raster cellStats
#' @importFrom utils write.csv2
#' @export
GenerateCSV = function(ZOI_List_of_Lists, ExportPath, Year, ID_list, ProductName, Threshold1){
  StartTime = Sys.time()
  MonthNames = list("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")
  Iterator =  1
  ZOI_DataframeList = list()
  for (ZOI in ZOI_List_of_Lists) {
    ZOIDataframe = data.frame(Year = integer(), Month = character(), WaterPixels = integer(), NotWaterPixels = integer(), TotalPixel = integer(), TotalWaterAreaM2 = integer(), TotalAreaM2 = integer(), WaterPercentage = integer(), stringsAsFactors = FALSE)
    MonthNamesIterator = 1
    for (BinaryNDVI in ZOI) {
      WaterCells = raster::cellStats(BinaryNDVI == 1, sum)
      NonWaterCells = raster::cellStats(BinaryNDVI == 0, sum)
      TotalCells = WaterCells + NonWaterCells
      TotalWaterAreaM2 = WaterCells*100
      TotalAreaM2 = TotalCells*100
      WaterPercentage = round(((WaterCells/TotalCells)*100), digits = 2)
      ZOIDataframe[nrow(ZOIDataframe) + 1,] = list(Year, MonthNames[[MonthNamesIterator]],WaterCells,NonWaterCells, TotalCells, TotalWaterAreaM2, TotalAreaM2, WaterPercentage)
      MonthNamesIterator = MonthNamesIterator + 1
    }
    ZOI_DataframeList[[Iterator]] = ZOIDataframe
    utils::write.csv2(ZOIDataframe, paste0(ExportPath, "/csv_ZOI_", ID_list[[Iterator]], "_", ProductName, "_Threshold_", Threshold1,  ".csv"), row.names = FALSE, quote = FALSE)
    print(noquote(paste0("Finished writing csv for ZOI ", ID_list[[Iterator]])))
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Finished writing csv for all ZOIs")))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent generating CSVs (HH:MM:SS): "), noquote(TotalTime)))
  return(ZOI_DataframeList)
}

#' Generates a csv file that contains several rows of information, namely number of Water Cells and Non Water Cells, prints time spent and names the files acordding to the Threshold provided. Accepts a list of lists where each element is a list of all 12 months for a specific year and specific ZOI;
#'
#' @param ZOI_List_of_Lists A list of lists, where the first element is a list of all 12 months of a product for ZOI 1; Usually a list of lists of Binary_NDVI cut to each ZOI;
#' @param ExportPath a path to export the .csv files;
#' @param Year a string to represent the agricultural year, ex: 2017 or Ano Agricola 2017
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;
#' @param ProductName a Product Name to be printed in the files name, ex: BinaryNDVI or Combo;
#' @param Threshold1 a number representing the threshold used in the files that originate the csv, like 0 for NDVI;
#' @param Threshold2 a number representing the threshold used in the files that originate the csv, mostly 800 for RGBSum;
#'
#' @return returns an organized list of dataframes that include a dataframe for each ZOI, representing an entire year of imagery, with stats such as number of Water Cells, Non Water cells, etc;
#' @importFrom raster cellStats
#' @importFrom utils write.csv2
#' @export
GenerateCSV2 = function(ZOI_List_of_Lists, ExportPath, Year, ID_list, ProductName, Threshold1, Threshold2){
  StartTime = Sys.time()
  MonthNames = list("Outubro","Novembro", "Dezembro", "Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro")
  Iterator =  1
  ZOI_DataframeList = list()
  for (ZOI in ZOI_List_of_Lists) {
    ZOIDataframe = data.frame(Year = integer(), Month = character(), WaterPixels = integer(), NotWaterPixels = integer(), TotalPixel = integer(), TotalWaterAreaM2 = integer(), TotalAreaM2 = integer(), WaterPercentage = integer(), stringsAsFactors = FALSE)
    MonthNamesIterator = 1
    for (BinaryNDVI in ZOI) {
      WaterCells = raster::cellStats(BinaryNDVI == 1, sum)
      NonWaterCells = raster::cellStats(BinaryNDVI == 0, sum)
      TotalCells = WaterCells + NonWaterCells
      TotalWaterAreaM2 = WaterCells*100
      TotalAreaM2 = TotalCells*100
      WaterPercentage = round(((WaterCells/TotalCells)*100), digits = 2)
      ZOIDataframe[nrow(ZOIDataframe) + 1,] = list(Year, MonthNames[[MonthNamesIterator]],WaterCells,NonWaterCells, TotalCells, TotalWaterAreaM2, TotalAreaM2, WaterPercentage)
      MonthNamesIterator = MonthNamesIterator + 1
    }
    ZOI_DataframeList[[Iterator]] = ZOIDataframe
    utils::write.csv2(ZOIDataframe, paste0(ExportPath, "/csv_ZOI_", ID_list[[Iterator]], "_", ProductName, "_ThresholdNDVI_", Threshold1, "_ThresholdRGBSum_", Threshold2,  ".csv"), row.names = FALSE, quote = FALSE)
    print(noquote(paste0("Finished writing csv for ZOI ", ID_list[[Iterator]])))
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Finished writing csv for all ZOIs")))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent generating CSVs (HH:MM:SS): "), noquote(TotalTime)))
  return(ZOI_DataframeList)
}
