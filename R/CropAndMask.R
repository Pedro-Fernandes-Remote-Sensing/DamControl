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
