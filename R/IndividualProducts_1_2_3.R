#' Takes a list of products, for example NDVI_2016, and cuts it to each individual ZOI, exporting the results to a certain path;
#'
#' @param ProductList_AllThreshold a list of rasters representing each month of a particular product, example a list of each month of NDVI_2016;
#' @param ZOIShapefileList a list of shapefiles representing the individual ZOI of the original shapefile;
#' @param ExportPath a string representing an export path;
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created asa global variable using CropShapefile_ExportToPath;
#' @param StringName a string to name the files, usually XXX_ as the function will attach the name of the month in the end;
#' @param StringOne a string to print as each product is finished;
#' @param StringOneZOI a string to print as each complete ZOI is finished;
#' @param StringAll a string to print in the end;
#'
#' @return a list of lists, with the first list representing ZOI_1 and element 1 of the first list being the first month corresponding to the first ZOI;
#' @importFrom raster crop
#' @importFrom raster mask
#' @importFrom raster extent
#' @importFrom raster writeRaster
#' @export
IndividualZOIProduct = function(ProductList_AllThreshold, ZOIShapefileList, ExportPath, ID_list, StringName, StringOne, StringOneZOI, StringAll){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  MonthNamesIterator = 1
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  ZOIProduct = list()
  ZOIProduct_List = list()
  for (ZOIShapefile in ZOIShapefileList){
    TrueExportPath = paste0(ExportPath, "/ZOI_", ID_list[[Iterator2]])
    for (BinaryProduct in ProductList_AllThreshold){
      Product_Cropped = raster::crop(BinaryProduct, raster::extent(ZOIShapefile))
      Product_Cropped_Masked = raster::mask(Product_Cropped, ZOIShapefile)
      ZOIProduct[[Iterator]] = Product_Cropped_Masked
      raster::writeRaster(Product_Cropped_Masked, filename = file.path(TrueExportPath, paste0(StringName, ID_list[[Iterator2]],"_", MonthNames[[MonthNamesIterator]])), format = "GTiff")
      print(noquote(paste0(StringOne, Iterator, " for ZOI ", ID_list[[Iterator2]])))
      Iterator = Iterator + 1
      MonthNamesIterator = MonthNamesIterator + 1
    }
    print(noquote(paste0(StringOneZOI, ID_list[[Iterator2]])))
    ZOIProduct_List[[Iterator2]] = ZOIProduct
    MonthNamesIterator = 1
    Iterator = 1
    Iterator2 = Iterator2 + 1
  }
  Iterator2 = 1
  print(noquote(paste0(StringAll)))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent producing Individual Products (HH:MM:SS): "), noquote(TotalTime)))
  return(ZOIProduct_List)
}

#' Takes a list of products, for example Binary_NDVI_2016, and cuts it to each individual ZOI, exporting the results to a certain path;
#'
#' @param ProductList_AllThreshold a list of rasters representing each month of a particular product, example a list of each month of Binary_NDVI_2016;
#' @param ZOIShapefileList a list of shapefiles representing the individual ZOI of the original shapefile;
#' @param ExportPath a string representing an export path;
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created as a global variable using CropShapefile_ExportToPath;
#' @param Threshold_List a list with a threshold number to be included in the files names, example: 0 as the NDVI threshold to be included the the file name, usually defined earlier in the order of operations, check Vignette;
#' @param StringName a string to name the files, usually XXX_ as the function will attach the name of the month in the end;
#' @param StringOne a string to print as each product is finished;
#' @param StringOneZOI a string to print as each complete ZOI is finished;
#' @param StringAll a string to print in the end;
#'
#' @return a list of lists, with the first list representing ZOI_1 and element 1 of the first list being the first month corresponding to the first ZOI;
#' @importFrom raster crop
#' @importFrom raster mask
#' @importFrom raster extent
#' @importFrom raster writeRaster
#' @export
IndividualZOIProduct2 = function(ProductList_AllThreshold, ZOIShapefileList, ExportPath, ID_list, Threshold_List, StringName, StringOne, StringOneZOI, StringAll){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  Iterator3 = 1
  MonthNamesIterator = 1
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  ZOIProduct = list()
  ZOIProduct_List = list()
  for (Threshold in Threshold_List){
    for (ZOIShapefile in ZOIShapefileList){
      TrueExportPath = paste0(ExportPath, "/ZOI_", ID_list[[Iterator2]], "_Threshold_", Threshold)
      for (BinaryProduct in ProductList_AllThreshold){
        Product_Cropped = raster::crop(BinaryProduct, raster::extent(ZOIShapefile))
        Product_Cropped_Masked = raster::mask(Product_Cropped, ZOIShapefile)
        ZOIProduct[[Iterator]] = Product_Cropped_Masked
        raster::writeRaster(Product_Cropped_Masked, filename = file.path(TrueExportPath, paste0(StringName, ID_list[[Iterator2]],"_", MonthNames[[MonthNamesIterator]], "_", Threshold_List[[Iterator3]])), format = "GTiff")
        print(noquote(paste0(StringOne, Iterator, " for ZOI ", ID_list[[Iterator2]])))
        Iterator = Iterator + 1
        MonthNamesIterator = MonthNamesIterator + 1
      }
      print(noquote(paste0(StringOneZOI, ID_list[[Iterator2]])))
      ZOIProduct_List[[Iterator2]] = ZOIProduct
      Threshold_ID = as.integer(Threshold)
      MonthNamesIterator = 1
      Iterator = 1
      Iterator2 = Iterator2 + 1
    }
    Iterator2 = 1
    Iterator3 = Iterator3 + 1
    print(noquote(paste0(StringAll, " with Threshold = ", Threshold_ID)))
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent producing Individual Products 2 (HH:MM:SS): "), noquote(TotalTime)))
  return(ZOIProduct_List)
}

#' Takes a list of products, for example Binary_Combo_2016, and cuts it to each individual ZOI, exporting the results to a certain path;
#'
#' @param ProductList_AllThreshold a list of rasters representing each month of a particular product, example a list of each month of Binary_Combo_2016;
#' @param ZOIShapefileList a list of shapefiles representing the individual ZOI of the original shapefile;
#' @param ExportPath a string representing an export path;
#' @param ID_list a list of IDs representing the amount of unique shapes inside the original shapefile, usually created as a global variable using CropShapefile_ExportToPath;
#' @param Threshold_List a list with a threshold number to be included in the files names, example: 0 as the NDVI threshold to be included the the file name, usually defined earlier in the order of operations, check Vignette;
#' @param Threshold_List_2 a second list with a threshold number to be included in the files name, example: 800 as the RGBSum threshold, usually defined earlier;
#' @param StringName a string to name the files, usually XXX_ as the function will attach the name of the month in the end;
#' @param StringOne a string to print as each product is finished;
#' @param StringOneZOI a string to print as each complete ZOI is finished;
#' @param StringAllOne a string to print as each combination for each ZOI is finished;
#' @param StringAllAll a string to print in the end, as every combination for all ZOI are finished;
#'
#' @return a list of lists, with the first list representing ZOI_1 and element 1 of the first list being the first month corresponding to the first ZOI;
#' @importFrom raster crop
#' @importFrom raster mask
#' @importFrom raster extent
#' @importFrom raster writeRaster
#' @export
IndividualZOIProduct3 = function(ProductList_AllThreshold, ZOIShapefileList, ExportPath, ID_list, Threshold_List, Threshold_List_2, StringName, StringOne, StringOneZOI, StringAllOne, StringAllAll){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  Iterator3 = 1
  Iterator4 = 1
  MonthNamesIterator = 1
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  ZOIProduct = list()
  ZOIProduct_List = list()
  for (Threshold in Threshold_List){
    for (Threshold2 in Threshold_List_2){
      for (ZOIShapefile in ZOIShapefileList){
        TrueExportPath = paste0(ExportPath, "/ZOI_", ID_list[[Iterator2]], "_Threshold_", Threshold, "_Threshold_", Threshold2)
        for (BinaryProduct in ProductList_AllThreshold[[Iterator3]][[Iterator4]]){
          Product_Cropped = raster::crop(BinaryProduct, raster::extent(ZOIShapefile))
          Product_Cropped_Masked = raster::mask(Product_Cropped, ZOIShapefile)
          ZOIProduct[[Iterator]] = Product_Cropped_Masked
          raster::writeRaster(Product_Cropped_Masked, filename = file.path(TrueExportPath, paste0(StringName, ID_list[[Iterator2]],"_", MonthNames[[MonthNamesIterator]], "_", Threshold_List[[Iterator3]], "_", Threshold_List_2[[Iterator4]])), format = "GTiff")
          print(noquote(paste0(StringOne, Iterator, " for ZOI ", ID_list[[Iterator2]])))
          Iterator = Iterator + 1
          MonthNamesIterator = MonthNamesIterator + 1
        }
        print(noquote(paste0(StringOneZOI, ID_list[[Iterator2]])))
        ZOIProduct_List[[Iterator2]] = ZOIProduct
        Threshold_ID = as.integer(Threshold)
        Threshold_ID_2 = as.integer(Threshold2)
        MonthNamesIterator = 1
        Iterator = 1
        Iterator2 = Iterator2 + 1
      }
      Iterator2 = 1
      Iterator3 = Iterator3 + 1
      print(noquote(paste0(StringAllOne, Threshold_ID, "and RGMSum Threshold = ", Threshold_ID_2)))
    }
    Iterator4 = Iterator4 + 1
  }
  print(noquote(paste0(StringAllAll)))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent producing Individual Products 3 (HH:MM:SS): "), noquote(TotalTime)))
  return(ZOIProduct_List)
}
