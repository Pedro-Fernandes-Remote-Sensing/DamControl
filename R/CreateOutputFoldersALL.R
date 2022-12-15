CreateOutputFoldersALL = function(Year_List, OUTPUT_Path){
  for (Year in Year_List){
    StartTime = Sys.time()
    dir.create(paste0(OUTPUT_Path, "/AA_", Year))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/1_NDVI_Albufeiras_Todas"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/2_Composito_Albufeiras_Todas"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/3_RGB_Sum_Albufeiras_Todas"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/4_NDVI_Binario_Albufeiras_Todas"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/5_RGB_Sum_Binario_Albufeiras_Todas"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/6_Combo_Binario_Albufeiras_Todas"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/7_NDVI_Albufeiras_Individuais"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/8_Composito_Albufeiras_Individuais"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/9_RGB_Sum_Albufeiras_Individuais"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/10_NDVI_Binario_Albufeiras_Individuais"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/11_RGB_Sum_Binario_Albufeiras_Individuais"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/12_Combo_Binario_Albufeiras_Individuais"))
    dir.create(paste0(OUTPUT_Path, "/AA_", Year, "/13_CSV_Albufeiras_Individuais"))
  }
  dir.create(paste0(OUTPUT_Path, "/Albufeiras_Individuais_Graficos"))
  dir.create(paste0(OUTPUT_Path, "/Albufeiras_Individuais_Graficos_Overlap"))
  dir.create(paste0(OUTPUT_Path, "/Albufeiras_Individuais_Shapefiles"))
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, unit = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent creating folders (HH:MM:SS): "), noquote(TotalTime)))
}

