#' Adds two raster lists together, first element with first element and so on, and prints time spent;
#'
#' @param BinaryNDVI_List a list of rasters with binary NDVIs, produced earlier based on some Threshold (presumably 12 elements, representing 1 agricultural year);
#' @param BinaryRGBSum_List a list of binary rasters with the sum of the RGB bands, produced earlier based on some Threshold (presumably 12 elements, representing 1 agricultural year);
#'
#' @return a list of the sum between the two arguments. Its original use is to return a raster with 3 values, 0 = that pixel hit none of the thresholds, 1 = pixel hit either the NDVI threshold or the RGBSum threshold and 2 = pixel hit both  thresholds;
#' @export
#'
#' @examples Data is too large, check Vignette.
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
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent Calculating NDVI and RGB Combo (HH:MM:SS): "), noquote(TotalTime)))
  return(Combo_List)
}
