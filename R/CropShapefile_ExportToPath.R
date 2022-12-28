#' Crops a shapefile into all its individual pieces, by OBJECTID, and exports all the individual shapefiles into ExportPath, and prints time spent; Also produces a global variable called ID_List representing a list of IDs taken from the original shapefile;
#'
#' @param ShapefilePath a string representing a path to the shapefile to be cut;
#' @param ShapefileName the name of the shapefile file;
#' @param ExportPath a string representing the path to which the individual shapefiles are to be exported;
#'
#' @return a list of individual shapefiles, ordered by OBJECTID;
#' @export
#'
#' @examples Data too large, check Vignette.
CropShapefile_ExportToPath = function(ShapefilePath, ShapefileName, ExportPath){
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
