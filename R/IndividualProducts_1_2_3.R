IndividualAlbufeiraProduct = function(ProductList_AllThreshold, AlbufeiraShapefileList, ExportPath, ID_list, StringName, StringOne, StringOneAlbufeira, StringAll){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  MonthNamesIterator = 1
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  AlbufeiraProduct = list()
  AlbufeiraProduct_List = list()
  for (AlbufeiraShapefile in AlbufeiraShapefileList){
    TrueExportPath = paste0(ExportPath, "/Albufeira_", ID_list[[Iterator2]])
    for (BinaryProduct in ProductList_AllThreshold){
      Product_Cropped = crop(BinaryProduct, extent(AlbufeiraShapefile))
      Product_Cropped_Masked = mask(Product_Cropped, AlbufeiraShapefile)
      AlbufeiraProduct[[Iterator]] = Product_Cropped_Masked
      writeRaster(Product_Cropped_Masked, filename = file.path(TrueExportPath, paste0(StringName, ID_list[[Iterator2]],"_", MonthNames[[MonthNamesIterator]])), format = "GTiff")
      print(noquote(paste0(StringOne, Iterator, " for Albufeira ", ID_list[[Iterator2]])))
      Iterator = Iterator + 1
      MonthNamesIterator = MonthNamesIterator + 1
    }
    print(noquote(paste0(StringOneAlbufeira, ID_list[[Iterator2]])))
    AlbufeiraProduct_List[[Iterator2]] = AlbufeiraProduct
    MonthNamesIterator = 1
    Iterator = 1
    Iterator2 = Iterator2 + 1
  }
  Iterator2 = 1
  print(noquote(paste0(StringAll)))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent producing Individual Products 1 (HH:MM:SS): "), noquote(TotalTime)))
}

IndividualAlbufeiraProduct2 = function(ProductList_AllThreshold, AlbufeiraShapefileList, ExportPath, ID_list, Threshold_List, StringName, StringOne, StringOneAlbufeira, StringAll){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  Iterator3 = 1
  MonthNamesIterator = 1
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  AlbufeiraProduct = list()
  AlbufeiraProduct_List = list()
  for (Threshold in Threshold_List){
    for (AlbufeiraShapefile in AlbufeiraShapefileList){
      TrueExportPath = paste0(ExportPath, "/Albufeira_", ID_list[[Iterator2]], "_Threshold_", Threshold)
      for (BinaryProduct in ProductList_AllThreshold){
        Product_Cropped = crop(BinaryProduct, extent(AlbufeiraShapefile))
        Product_Cropped_Masked = mask(Product_Cropped, AlbufeiraShapefile)
        AlbufeiraProduct[[Iterator]] = Product_Cropped_Masked
        writeRaster(Product_Cropped_Masked, filename = file.path(TrueExportPath, paste0(StringName, ID_list[[Iterator2]],"_", MonthNames[[MonthNamesIterator]], "_", Threshold_List[[Iterator3]])), format = "GTiff")
        print(noquote(paste0(StringOne, Iterator, " for Albufeira ", ID_list[[Iterator2]])))
        Iterator = Iterator + 1
        MonthNamesIterator = MonthNamesIterator + 1
      }
      print(noquote(paste0(StringOneAlbufeira, ID_list[[Iterator2]])))
      AlbufeiraProduct_List[[Iterator2]] = AlbufeiraProduct
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
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent producing Individual Products 2 (HH:MM:SS): "), noquote(TotalTime)))
  return(AlbufeiraProduct_List)
}

IndividualAlbufeiraProduct3 = function(ProductList_AllThreshold, AlbufeiraShapefileList, ExportPath, ID_list, Threshold_List, Threshold_List_2, StringName, StringOne, StringOneAlbufeira, StringAllOne, StringAllAll){
  StartTime = Sys.time()
  Iterator = 1
  Iterator2 = 1
  Iterator3 = 1
  Iterator4 = 1
  MonthNamesIterator = 1
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  AlbufeiraProduct = list()
  AlbufeiraProduct_List = list()
  for (Threshold in Threshold_List){
    for (Threshold2 in Threshold_List_2){
      for (AlbufeiraShapefile in AlbufeiraShapefileList){
        TrueExportPath = paste0(ExportPath, "/Albufeira_", ID_list[[Iterator2]], "_Threshold_", Threshold, "_Threshold_", Threshold2)
        for (BinaryProduct in ProductList_AllThreshold[[Iterator3]][[Iterator4]]){
          Product_Cropped = crop(BinaryProduct, extent(AlbufeiraShapefile))
          Product_Cropped_Masked = mask(Product_Cropped, AlbufeiraShapefile)
          AlbufeiraProduct[[Iterator]] = Product_Cropped_Masked
          writeRaster(Product_Cropped_Masked, filename = file.path(TrueExportPath, paste0(StringName, ID_list[[Iterator2]],"_", MonthNames[[MonthNamesIterator]], "_", Threshold_List[[Iterator3]], "_", Threshold_List_2[[Iterator4]])), format = "GTiff")
          print(noquote(paste0(StringOne, Iterator, " for Albufeira ", ID_list[[Iterator2]])))
          Iterator = Iterator + 1
          MonthNamesIterator = MonthNamesIterator + 1
        }
        print(noquote(paste0(StringOneAlbufeira, ID_list[[Iterator2]])))
        AlbufeiraProduct_List[[Iterator2]] = AlbufeiraProduct
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
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent producing Individual Products 3 (HH:MM:SS): "), noquote(TotalTime)))
  return(AlbufeiraProduct_List)
}