#' GenerateExportPaths takes in a list of years and a general OutputPath, that should be the same provided to the function CreateOutputFoldersALL, and automatically defines
#' in a global environment all the export paths needed for the package to export its several steps.
#'
#' @param Year_List A list of years.
#' @param OutputPath A path to a folder where you would like the exports to go. Should be the same as the one provided to CreateOutputFoldersALL
#'
#' @return This function does not return anything.
#' @export
GenerateExportPaths = function(Year_List, OutputPath){
   Iterator = 1
   pos = 1
   envir = as.environment(pos)
   for (Year in Year_List) {
     # 1 - NDVI_ALL
     var.name = paste0("ExportPath_NDVI_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/NDVIs", "/NDVI_ZOI_All"), envir = envir)
     # 2 - Composite_ALL
     var.name = paste0("ExportPath_Composite_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/Composites", "/Composite_ZOI_All"), envir = envir)
     # 3 - RGBSum_ALL
     var.name = paste0("ExportPath_RGBSum_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/RGBSums", "/RGB_Sum_ZOI_All"), envir = envir)
     # 4 - NDVI_Binary_ALL
     var.name = paste0("ExportPath_NDVI_Binary_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/NDVIs", "/NDVI_Binary_ZOI_All"), envir = envir)
     # 5 - RGBSum_Binary_ALL
     var.name = paste0("ExportPath_RGBSum_Binary_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/RGBSums", "/RGB_Sum_Binary_ZOI_All"), envir = envir)
     # 6 - Algorithm_ALL
     var.name = paste0("ExportPath_Algorithm_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/Algorithm", "/Algorithm_Binary_ZOI_All"), envir = envir)
     # 7 - NDVI_Individual
     var.name = paste0("ExportPath_NDVI_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/NDVIs", "/NDVI_ZOI_Individual"), envir = envir)
     # 8 - Composite_Individual
     var.name = paste0("ExportPath_Composite_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/Composites", "/Composite_ZOI_Individual"), envir = envir)
     # 9 - RGBSum_Individual
     var.name = paste0("ExportPath_RGBSum_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/RGBSums", "/RGB_Sum_ZOI_Individual"), envir = envir)
     # 10 - NDVI_Binary_Individual
     var.name = paste0("ExportPath_NDVI_Binary_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/NDVIs", "/NDVI_Binary_ZOI_Individual"), envir = envir)
     # 11 - RGBSum_Binary_Individual
     var.name = paste0("ExportPath_RGBSum_Binary_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/RGBSums", "/RGB_Sum_Binary_ZOI_Individual"), envir = envir)
     # 12 - Algorithm_Individual
     var.name = paste0("ExportPath_Algorithm_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/Algorithm", "/Algorithm_Binary_ZOI_Individual"), envir = envir)
     # 13 - CSV_Individual
     var.name = paste0("ExportPath_CSV_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/Agricultural_Year_", Year_List[Iterator], "/CSVs", "/CSV_ZOI_Individual"), envir = envir)
     Iterator = Iterator + 1
   }
   # Individual_ZOI_Graphs
   var.name = paste0("ExportPath_Graphs_Individual")
   assign(var.name, paste0(OutputPath, "/ZOI_Individual_Graphs"), envir = envir)
   # Individual_Graphs_Overlap
   var.name = paste0("ExportPath_Graphs_Overlap_Individual")
   assign(var.name, paste0(OutputPath, "/ZOI_Individual_Graphs_Overlap"), envir = envir)
   # Individual Shapefiles
   var.name = paste0("ExportPath_Shapefiles_Individual")
   assign(var.name, paste0(OutputPath, "/ZOI_Individual_Shapefiles"), envir = envir)
}

