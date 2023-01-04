#' Reclassifies a list of rasters acording to the vector provided. The Vectors are generated with the correct formatting by using the function VectorBuilder. Vector1 has NDVI threshold, Vetor 2 has RGMSum threshold and Vetor 3 has the combo to used when reclassifyng the Combo;
#'
#' @param RasterList a raster list to be reclassified acording to vetor provided;
#' @param Vetor vetor to orient the reclassification of the raster; Should be presented as follows: Vetor = c(-Inf, ThresholdNumber, 1, ThresholdNumber, Inf, 0);
#' @param StringFinished_One a string to print as each raster is reclassified;
#' @param StringFinished_All a string to print in the end;
#'
#' @return a list of reclassified rasters acording to the Vetor provided in the arguments;
#' @importFrom raster reclassify
#' @export
ReclassRasterList = function(RasterList, Vetor, StringFinished_One, StringFinished_All){
  StartTime = Sys.time()
  MatrizReclassificacao = matrix(Vetor, ncol = 3, byrow = TRUE) #byrow enche a matriz por linhas em vez de colunas
  RasterReclassList = list()
  Iterator = 1
  for (Raster in RasterList){
    RasterReclassList[[Iterator]] = raster::reclassify(Raster, MatrizReclassificacao, include.lowest = TRUE) #include.lowest faz com que seja >= -1 ate < 0 seja 1 em vez de >-1 e <=0
    print(noquote(paste0(StringFinished_One, Iterator)))
    Iterator = Iterator + 1
  }
  print(noquote(StringFinished_All))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent reclassifying Raster List (HH:MM:SS): "), noquote(TotalTime)))
  return(RasterReclassList)
}
