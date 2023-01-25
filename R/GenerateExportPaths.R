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
     # NDVI_ALL
     var.name = paste0("ExportPath_NDVI_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/1_NDVI_ZOI_All"), envir = envir)
     # Composite_ALL
     var.name = paste0("ExportPath_Composite_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/2_Composite_ZOI_All"), envir = envir)
     # RGBSum_ALL
     var.name = paste0("ExportPath_RGBSum_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/3_RGB_Sum_ZOI_All"), envir = envir)
     # NDVI_Binary_ALL
     var.name = paste0("ExportPath_NDVI_Binary_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/4_NDVI_Binary_ZOI_All"), envir = envir)
     # RGBSum_Binary_ALL
     var.name = paste0("ExportPath_RGBSum_Binary_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/5_RGB_Sum_Binary_ZOI_All"), envir = envir)
     # Algorithm_ALL
     var.name = paste0("ExportPath_Algorithm_All_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/7_NDVI_ZOI_Individual"), envir = envir)
     # NDVI_Individual
     var.name = paste0("ExportPath_NDVI_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/8_Composite_ZOI_Individual"), envir = envir)
     # Composite_Individual
     var.name = paste0("ExportPath_Composite_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/9_RGB_Sum_ZOI_Individual"), envir = envir)
     # NDVI_Binary_Individual
     var.name = paste0("ExportPath_NDVI_Binary_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/10_NDVI_Binary_ZOI_Individual"), envir = envir)
     # RGBSum_Binary_Individual
     var.name = paste0("ExportPath_RGBSum_Binary_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/11_RGB_Sum_Binary_ZOI_Individual"), envir = envir)
     # Algorithm_Individual
     var.name = paste0("ExportPath_Algorithm_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/12_Algorithm_Binary_ZOI_Individual"), envir = envir)
     # CSV_Individual
     var.name = paste0("ExportPath_CSV_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/13_CSV_ZOI_Individual"), envir = envir)
     # Individual_ZOI_Graphs
     var.name = paste0("ExportPath_Graphs_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/ZOI_Individual_Graphs"), envir = envir)
     # Individual_Graphs_Overlap
     var.name = paste0("ExportPath_Graphs_Overlap_Individual_", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/ZOI_Individual_Graphs_Overlap"), envir = envir)
     # Individual Shapefiles
     var.name = paste0("ExportPath_Shapefiles_Individual", Year_List[Iterator])
     assign(var.name, paste0(OutputPath, "/AA_", Year_List[Iterator], "/ZOI_Individual_Shapefiles"), envir = envir)
     Iterator = Iterator + 1
   }
}

