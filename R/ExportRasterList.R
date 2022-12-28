#' Exports a list of rasters and appends the month name to the end, assuming the rasters are ordered by agricultural year; Also prints time spent;
#'
#' @param RasterList the raster list to be exported as GTiff;
#' @param Path a string representing the path to export to;
#' @param GeneralFileName a file name for the files, usually xxxx_ as the function will append the month name at the end;
#' @param StringFinished_One a string to be printed as each raster is exported;
#' @param StringFinished_All a string to be printed in the end;
#'
#' @return doesnt return anything. Meant to be used as an export tool;
#' @export
#'
#' @examples Data too large, check Vignette.
ExportRasterList = function(RasterList, Path, GeneralFileName, StringFinished_One, StringFinished_All){
  StartTime = Sys.time()
  MonthNames = list("(1)Outubro","(2)Novembro", "(3)Dezembro", "(4)Janeiro", "(5)Fevereiro", "(6)Marco", "(7)Abril", "(8)Maio", "(9)Junho", "(10)Julho", "(11)Agosto", "(12)Setembro")
  Iterator = 1
  for (Raster in RasterList) {
    writeRaster(Raster, filename = file.path(Path,paste0(GeneralFileName, MonthNames[Iterator])), format = "GTiff")
    print(noquote(paste0(StringFinished_One, Iterator)))
    Iterator = Iterator + 1
  }
  print(noquote(paste0(StringFinished_All, Path)))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent Exporting Raster List (HH:MM:SS): "), noquote(TotalTime)))
}



