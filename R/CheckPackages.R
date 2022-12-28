#' Checks if required packages are installed and loaded and if they're not, installs them and loads them. This is usually not needed as R should install any dependencies; Prints time spent;
#'
#' @return has no return. Meant to called without arguments;
#' @export
#'
#' @examples CheckPackages()
CheckPackages = function(){
  StartTime = Sys.time()
  if (!require('raster')) install.packages('raster'); library ('raster')
  if (!require('rgdal')) install.packages('rgdal'); library ('rgdal')
  if (!require('sf')) install.packages('sf'); library ('sf')
  if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
  if (!require('patchwork')) install.packages('patchwork'); library ('patchwork')
  if (!require('dplyr')) install.packages('dplyr'); library ('dplyr')
  if (!require('reshape2')) install.packages('reshape2'); library ('reshape2')
  if (!require('rgeos')) install.packages('rgeos'); library ('rgeos')
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent checking Packages (HH:MM:SS): "), noquote(TotalTime)))
}
