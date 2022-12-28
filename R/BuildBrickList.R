#' Build a "Brick" object list from a List of FilePaths, and prints time spent;
#'
#' @param FilePathList a list of file paths. This is usually obtained through list.files with path = location_of_files, pattern = string_to_detect_files, full.names = TRUE
#'
#' @return a list of "Bricks", an object usable by R that represents a file with several layers, usually a raster;
#' @export
#'
#' @examples Data is too large, check Vignette.
BuildBrickList = function(FilePathList){
  StartTime = Sys.time()
  CompositesBrickList = list()
  Iterator = 1
  for (File in FilePathList) {
    CompositesBrickList[[Iterator]] = brick(File)
    print(noquote(paste0("Built Brick ", Iterator)))
    Iterator = Iterator + 1
  }
  print(noquote("All Bricks Built"))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent Building Brick Lists (HH:MM:SS): "), noquote(TotalTime)))
  return(CompositesBrickList)
}


