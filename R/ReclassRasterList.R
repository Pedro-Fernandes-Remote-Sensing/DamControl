ReclassRasterList = function(RasterList, Vetor, StringFinished_One, StringFinished_All){
  StartTime = Sys.time()
  MatrizReclassificacao = matrix(Vetor, ncol = 3, byrow = TRUE) #byrow enche a matriz por linhas em vez de colunas
  RasterReclassList = list()
  Iterator = 1
  for (Raster in RasterList){
    RasterReclassList[[Iterator]] = reclassify(Raster, MatrizReclassificacao, include.lowest = TRUE) #include.lowest faz com que seja >= -1 at? < 0 seja 1 em vez de >-1 e <=0
    print(noquote(paste0(StringFinished_One, Iterator)))
    Iterator = Iterator + 1
  }
  print(noquote(StringFinished_All))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent reclassifying Raster List (HH:MM:SS): "), noquote(TotalTime)))
  return(RasterReclassList)
}
