#' Cuts and Masks a list of rasters to the zone of a shapefile; Cutting the satellite images to the required zones in the beginning saves a LOT of time; Prints time spent;
#' Might produced an error: "Error in x$.self$finalize() : attempt to apply non-function" but that's an R problem and it doesnt affect the results;
#'
#' @param BrickList a list of Bricks, raster objects, to be reduced, as in cut, to the area of a shapefile;
#' @param ShapefilePath a string representing a path to a shapefile;
#' @param ShapeName a string representing the shapefiles name;
#'
#' @return a list of rasters cut to the zone of the shapefile presented in the arguments;
#' @export
#'
#' @examples Data too large, check Vignette.
CropAndMask = function(BrickList, ShapefilePath, ShapeName){
  StartTime = Sys.time()
  CroppedMaskedList = list()
  Shapefile = read_sf(ShapefilePath, ShapeName)
  ShapefileXY = st_zm(Shapefile)
  Iterator = 1
  for (File in BrickList){
    Cropped = crop(File, extent(ShapefileXY))
    Masked = mask(Cropped, ShapefileXY)
    print(noquote(paste0("Cropped and Masked Brick ", Iterator)))
    CroppedMaskedList[[Iterator]] = Masked
    Iterator = Iterator + 1
  }
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating Albufeira_X_Threshold_X_Threshold_X Folders (HH:MM:SS): "), noquote(TotalTime)))
  return(CroppedMaskedList)
}
