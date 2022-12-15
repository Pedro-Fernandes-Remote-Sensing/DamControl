CropShapefile = function(ShapefilePath, ShapefileName, ExportPath){
  StartTime = Sys.time()
  Shapefile = readOGR(ShapefilePath, ShapefileName)
  Iterator = 1
  Iterator2 = 1
  ID_list <<- list()
  AlbufeirasShapefile_List = list()
  for (ObjectID in Shapefile$OBJECTID){
    Albufeira_Individual <- Shapefile[Shapefile$OBJECTID == ObjectID,]
    Albufeira_Cropped = crop(Shapefile, extent(Albufeira_Individual))
    writeOGR(Albufeira_Cropped, ExportPath, paste0("/Albufeira_", ObjectID), driver = "ESRI Shapefile")
    ID = as.integer(ObjectID)
    ID_list[[Iterator]] <<- ID
    print(noquote(paste0("Cropped and Exported Albufeira "), ID))
    Iterator = Iterator + 1
    AlbufeirasShapefile_List[[Iterator2]] = Albufeira_Cropped
    Iterator2 = Iterator2 + 1
  }
  print(noquote(paste0("Cropped and Exported Albufeiras to ", ExportPath)))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent cropping the shapefile (HH:MM:SS): "), noquote(TotalTime)))
  return(AlbufeirasShapefile_List)
}
