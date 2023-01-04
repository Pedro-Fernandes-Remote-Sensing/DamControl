#' Adds two raster lists together
#'
#' @param BinaryNDVI_List a list of BinaryNDVI rasters
#' @param BinaryRGBSum_List a list of BinaryRGBSum rasters
#'
#' @return returns a list of rasters, each representing an individual sum betweent BinaryNDVI and BinaryRGBSum, with values 0 = no hit on either, 1 = hit on one, 2 = hit on both;
#' @importFrom raster stack
#' @importFrom raster calc
#' @export
CalculateCombo = function(BinaryNDVI_List, BinaryRGBSum_List){
  StartTime = Sys.time()
  Combo_List = list()
  Temporary_List = list()
  Iterator = 1
  for (i in 1:length(BinaryNDVI_List)){
    Temporary_List[[1]] = BinaryNDVI_List[[Iterator]]
    Temporary_List[[2]] = BinaryRGBSum_List[[Iterator]]
    Temporary_Stack = stack(Temporary_List)
    Combo_List[[Iterator]] = calc(Temporary_Stack, sum)
    print(noquote(paste0("Calculated Combo Sum ", Iterator)))
    Iterator = Iterator + 1
  }
  print(noquote(paste0("Calculated All Combo Sums")))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent Calculating NDVI and RGB Combo (HH:MM:SS): "), noquote(TotalTime)))
  return(Combo_List)
}

