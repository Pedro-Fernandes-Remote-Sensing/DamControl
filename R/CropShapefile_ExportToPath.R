if(getRversion() >= "2.15.1")  utils::globalVariables(c("ID_list"))
#' Crops a shapefile into all its individual pieces, by UniqueID, exports all the individual shapefiles into ExportPat plus a new Shapefile with all ZOI and a new column called UniqueID, and prints time spent; Also produces a global variable called ID_List representing a list of IDs taken from the original shapefile;
#'
#' @param ShapefilePath a string representing a path to the shapefile to be cut;
#' @param ShapefileName the name of the shapefile file;
#' @param ExportPath a string representing the path to which the individual shapefiles are to be exported;
#'
#' @return a list of individual shapefiles, ordered by UniqueID;
#' @importFrom rgdal readOGR
#' @importFrom rgdal writeOGR
#' @importFrom raster crop
#' @importFrom raster extent
#' @export
CropShapefile_ExportToPath = function(ShapefilePath, ShapefileName, ExportPath){
  StartTime = Sys.time()
  Shapefile = rgdal::readOGR(dsn = paste0(ShapefilePath, ShapefileName))
  Shapefile$UniqueID<-1:nrow(Shapefile)
  rgdal::writeOGR(Shapefile, ExportPath, paste0(ShapefileName, "_UniqueID"), driver = "ESRI Shapefile")
  Iterator = 1
  pos = 1
  envir = as.environment(pos)
  var.name = "ID_list"
  assign(var.name, 1:nrow(Shapefile), envir = envir)
  ZOIsShapefile_List = list()
  for (UniqueID in Shapefile$UniqueID){
    ZOI_Individual <- Shapefile[Shapefile$UniqueID == UniqueID,]
    ZOI_Cropped = raster::crop(Shapefile, raster::extent(ZOI_Individual))
    rgdal::writeOGR(ZOI_Cropped, ExportPath, paste0("/ZOI_", UniqueID), driver = "ESRI Shapefile")
    print(noquote(paste0("Cropped and Exported ZOI "), as.integer(UniqueID)))
    ZOIsShapefile_List[[Iterator]] = ZOI_Cropped
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Cropped and Exported ZOIs to ", ExportPath)))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent cropping and exporting the shapefile (HH:MM:SS): "), noquote(TotalTime)))
  return(ZOIsShapefile_List)
}
