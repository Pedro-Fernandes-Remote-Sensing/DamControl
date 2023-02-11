#' Function that automates the WaterDetection algorithm. With proper input, everything should be automated. Remenber, NDVI inputs should be named "NDVIListYearXXXX" and Composite inputs
#' should be named "CompositeListYearXXXX"
#'
#' @param Year_List a list of years to be calculated;
#' @param ShapefilePath path to the original shapefile that contains all the ZOIs to be evaluated;
#' @param ShapefileName name of the original shapefile that contains all the ZOIs to be evaluated;
#' @param Output_Path general output path where files should be exported;
#' @param NDVI_Threshold threshold to be used with NDVI for water detection, recommended zero, 0;
#' @param RGBSum_Threshold threshold to be used with the sum of the RGB bands for water detection, recommended eight-hundred, 800;
#'
#' @return this function doesnt return anything, but exports a lot of things.
#' @export
RunAlgorithm = function(Year_List, ShapefilePath, ShapefileName, Output_Path, NDVI_Threshold, RGBSum_Threshold){
  # Start the clock to measure time spent
  StartTime = Sys.time()
  # Define global enviorment
  pos = 1
  envir = as.environment(pos)
  # Define final lists of Dataframes
  Algorithm_ZOI_Dataframe_List_All = list()
  BinaryNDVI_ZOI_Dataframe_List_All = list()
  Index = 1
  # 10 - Create Basic Output Folders
  CreateOutputFoldersALL(Year_List, Output_Path)
  # 11 - Define variables in memory containing export paths
  GenerateExportPaths(Year_List, Output_Path)
  # 12 - Cut our ZOI shapefile into Individual Shapefiles and Export Them
  NameHolder = "ExportPath_Shapefiles_Individual"
  ZOI_Shapefile_List = CropShapefile_ExportToPath(ShapefilePath, ShapefileName, get(NameHolder))
  # 13 - Build Vectors using defined thresholds (to reclassify rasters later)
  VectorBuilder(NDVI_Threshold[[1]], RGBSum_Threshold[[1]])
  for (Year in Year_List) {
    # 14 - Build a list of NDVI Bricks
    NameHolder = paste0("NDVIList", Year)
    NDVI_YearX_BL = BuildBrickList(get(NameHolder))

    # 15 - Crop And Mask our NDVI imagery to the ZOI in our Shapefile
    NDVI_YearX_ZOI = CropAndMask(NDVI_YearX_BL, ShapefilePath, ShapefileName)

    # 16 - Export the Cropped and Masked NDVI
    NameHolder = paste0("ExportPath_NDVI_All_", Year)
    ExportRasterList(NDVI_YearX_ZOI, get(NameHolder), paste0("NDVI_All_", Year, "_"), "Finished Exporting NDVI ", "Finished Exporting all NDVI")

    # 17 - Create folders for the NDVI of each Individual ZOI
    NameHolder = paste0("ExportPath_NDVI_Individual_", Year)
    CreateIndividualFolders(get(NameHolder), ID_list)

    # 18 - Cut and Export Individual ZOI NDVI and store those in a matrix in memory
    NDVI_YearX_IndividualZOI_ListOfLists = IndividualZOIProduct(NDVI_YearX_ZOI, ZOI_Shapefile_List, get(NameHolder), ID_list, paste0("NDVI_", Year, "_ZOI_"), "Singular NDVI Exported ", "Completed ZOI ", "Completed All ZOI")

    # 19 - Reclassify the NDVI with NDVI_Threshold (not the individual ZOI matrix)
    NDVI_YearX_ZOI_Binary = ReclassRasterList(NDVI_YearX_ZOI, VectorNDVI, "Reclassfied NDVI ", "Reclassified all NDVI")

    # 20 - Export the recently reclassified NDVI
    NameHolder = paste0("ExportPath_NDVI_Binary_All_", Year)
    ExportRasterList(NDVI_YearX_ZOI_Binary, get(NameHolder), paste0("Binary_NDVI_All_", Year, "_"), "Finished Exporting Binary NDVI ", "Finished Exporting all Binary NDVI")

    # 21 - Create folders for the Binary NDVI of each Individual ZOI
    NameHolder = paste0("ExportPath_NDVI_Binary_Individual_", Year)
    CreateIndividualFolders2(get(NameHolder), ID_list, NDVI_Threshold)

    # 22 - Cut and Export Individual ZOI Binary NDVI and store those in a matrix in memory
    NDVI_Binary_YearX_IndividualZOI_ListofLists = IndividualZOIProduct2(NDVI_YearX_ZOI_Binary, ZOI_Shapefile_List, get(NameHolder), ID_list, NDVI_Threshold[[1]], paste0("Binary_NDVI_", Year, "_ZOI_"), "Singular Binary NDVI Exported ", "Completed ZOI ", "Completed All ZOI")

    # 23 - Build Composite Bricks
    NameHolder = paste0("CompositeList", Year)
    Composite_YearX_BL = BuildBrickList(get(NameHolder))

    # 24 - Crop And Mask our Composite imagery to the ZOI in our Shapefile
    Composite_YearX_ZOI = CropAndMask(Composite_YearX_BL, ShapefilePath, ShapefileName)

    # 25 - Export the Cropped and Masked Composites
    NameHolder = paste0("ExportPath_Composite_All_", Year)
    ExportRasterList(Composite_YearX_ZOI, get(NameHolder), paste0("Composite_All_", Year, "_"), "Finished Exporting Composite ", "Finished Exporting all Composites")

    # 26 - Create folders for the Composite of each Individual ZOI
    NameHolder = paste0("ExportPath_Composite_Individual_", Year)
    CreateIndividualFolders(get(NameHolder), ID_list)

    # 27 - Cut and Export Individual ZOI Composites and store those in a matrix in memory
    Composite_YearX_IndividualZOI_ListOfLists = IndividualZOIProduct(Composite_YearX_ZOI, ZOI_Shapefile_List, get(NameHolder), ID_list, paste0("Composite_", Year, "_ZOI_"), "Singular Composite Exported ", "Completed ZOI ", "Completed All ZOI")

    # 28 - Calculate a sum of the RGB bands on our Composites (not the individual matrix)
    RGBSum_YearX_ZOI = CalculateRGBSum(Composite_YearX_ZOI)

    # 29 - Export the recently calculated sum of RGB bands
    NameHolder = paste0("ExportPath_RGBSum_All_", Year)
    ExportRasterList(RGBSum_YearX_ZOI, get(NameHolder), paste0("RGBSum_All_", Year, "_"), "Finished Exporting RGBSum ", "Finished Exporting all RGBSum")

    # 30 - Create folders for the RGBSum of each Individual ZOI
    NameHolder = paste0("ExportPath_RGBSum_Individual_", Year)
    CreateIndividualFolders(get(NameHolder), ID_list)

    # 31 - Cut and Export Individual ZOI RGBSum and store those in a matrix in memory
    RGBSum_YearX_IndividualZOI_ListOfLists = IndividualZOIProduct(RGBSum_YearX_ZOI, ZOI_Shapefile_List, get(NameHolder), ID_list, paste0("RGBSum", Year, "_ZOI_"), "Singular RGBSum Exported ", "Completed RGBSum ", "Completed All RGBSum")

    # 32 - Reclassify our RGBSums
    RGBSum_YearX_ZOI_Binary = ReclassRasterList(RGBSum_YearX_ZOI, VectorRGBSum, "Reclassfied RGBSum ", "Reclassified all RGBSum")

    # 33 - Export our recently reclassified RGBSums
    NameHolder = paste0("ExportPath_RGBSum_Binary_All_", Year)
    ExportRasterList(RGBSum_YearX_ZOI_Binary, get(NameHolder), paste0("RGBSum_Binary_All_", Year, "_"), "Finished Exporting Binary RGBSum ", "Finished Exporting all Binary RGBSum")

    # 34 - Create Individual Folders for Binary RGBSum
    NameHolder = paste0("ExportPath_RGBSum_Binary_Individual_", Year)
    CreateIndividualFolders2(get(NameHolder), ID_list, RGBSum_Threshold)

    # 35 - Cut and Export Individual ZOI Binary RGBSum and store those in a matrix in memory
    Binary_RGBSum_YearX_IndividualZOI_ListOfLists = IndividualZOIProduct2(RGBSum_YearX_ZOI_Binary, ZOI_Shapefile_List, get(NameHolder), ID_list, RGBSum_Threshold[[1]], paste0("RGBSum_Binary", Year, "_ZOI_"), "Singular Binary RGBSum Exported ", "Finished Exporting Individual Binary RGBSum ", "Finished Exporting All Individual Binary RGBSum")

    # 36 - Calculate Algorithm
    Algorithm_YearX_ZOI = CalculateAlgorithm(NDVI_YearX_ZOI_Binary,RGBSum_YearX_ZOI_Binary)

    # 37 - Reclassify Algorithm
    Algorithm_YearX_ZOI_Binary = ReclassRasterList(Algorithm_YearX_ZOI, VectorAlgorithm, "Reclassfied Algorithm ", "Reclassified all Algorithm")

    # 38 - Export reclassified Algorithm
    NameHolder = paste0("ExportPath_Algorithm_All_", Year)
    ExportRasterList(Algorithm_YearX_ZOI_Binary, get(NameHolder), paste0("Algorithm_Binary_All_", Year, "_"), "Finished Exporting Binary Algorithm ", "Finished Exporting all Binary Algorithm")

    # 39 - Create Individual Folders for Algorithm
    NameHolder = paste0("ExportPath_Algorithm_Individual_", Year)
    CreateIndividualFolders3(get(NameHolder), ID_list, NDVI_Threshold, RGBSum_Threshold)

    # 40 - Organize your Algorithm Data into lists;
    Binary_Algorithm_YearX_List = list()
    Binary_Algorithm_YearX_TY_List = list()
    Binary_Algorithm_YearX_TY_List[[1]] = Algorithm_YearX_ZOI_Binary
    Binary_Algorithm_YearX_List[[1]] = Binary_Algorithm_YearX_TY_List

    # 40 - [OPTIONAL] Organize your BinaryNDVI Data into lists;
    Binary_NDVI_YearX_List = list()
    Binary_NDVI_YearX_TY_List = list()
    Binary_NDVI_YearX_TY_List[[1]] = NDVI_YearX_ZOI_Binary
    Binary_NDVI_YearX_List[[1]] = Binary_NDVI_YearX_TY_List

    # 41 - Cut and Export Individual ZOI Binary Algorithm and store those in a matrix in memory
    Binary_Algorithm_YearX_IndividualZOI_ListOfLists = IndividualZOIProduct3(ProductList_AllThreshold = Binary_Algorithm_YearX_List, ZOIShapefileList= ZOI_Shapefile_List, ExportPath = get(NameHolder), ID_list = ID_list, Threshold_List = NDVI_Threshold[[1]], Threshold_List_2 = RGBSum_Threshold[[1]], StringName = paste0("Algorithm_Binary_", Year, "_ZOI_"), StringOne = "Exported Binary Algorithm ", StringOneZOI = "Completed ZOI ", StringAllOne = "Completed All ZOI ", StringAllAll = "Completed All ZOI with all Thresholds")

    # 42 - Generate and export some .csv for further use. This will also return a Dataframe
    NameHolder = paste0("ExportPath_CSV_", Year)
    Algorithm_ZOI_Dataframe_List_YearX = GenerateCSV2(Binary_Algorithm_YearX_IndividualZOI_ListOfLists, get(NameHolder), paste0("Agricultural ", Year), ID_list, "Algorithm", NDVI_Threshold[[1]], RGBSum_Threshold[[1]])

    # 43 - [OPTIONAL] Generate and export some .csv for further use. This will also return a Dataframe
    NameHolder = paste0("ExportPath_CSV_", Year)
    BinaryNDVI_ZOI_Dataframe_List_YearX =  GenerateCSV(NDVI_Binary_YearX_IndividualZOI_ListofLists, get(NameHolder), paste0("Agricultural ", Year), ID_list, "BinaryNDVI", NDVI_Threshold[[1]])

    # 44 and 45 - Organize your Dataframes into lists
    Algorithm_ZOI_Dataframe_List_All[[Index]] = Algorithm_ZOI_Dataframe_List_YearX
    BinaryNDVI_ZOI_Dataframe_List_All[[Index]] = BinaryNDVI_ZOI_Dataframe_List_YearX

    # Advance Index
    Index = Index + 1
  }
  # 46 and 47 - Re-organize our structure from Year-Based to ZOI-Based
  Algorithm_All_ZOI_ByYear = Create_ZOI_AllYears_List(Algorithm_ZOI_Dataframe_List_All)
  BinaryNDVI_All_ZOI_ByYear = Create_ZOI_AllYears_List(BinaryNDVI_ZOI_Dataframe_List_All)

  # 48 and 49 - Finally, it’s time to plot.
  NameHolder = "ExportPath_Graphs_Individual"
  PlotAllYears_All_ZOIs(Algorithm_All_ZOI_ByYear, get(NameHolder))
  NameHolder = "ExportPath_Graphs_Overlap_Individual"
  PlotAllYears_All_ZOIs2(BinaryNDVI_All_ZOI_ByYear, Algorithm_All_ZOI_ByYear, get(NameHolder))

  # Time Spent
  EndTime = Sys.time()
  TimeinSeconds <- as.numeric(difftime(EndTime, StartTime, units = "secs"))
  HoursSpent <- floor(TimeinSeconds / 3600)
  MinutesSpent <- floor((TimeinSeconds - 3600 * HoursSpent) / 60)
  SecondsSpent <- TimeinSeconds - 3600*HoursSpent - 60*MinutesSpent
  TotalTime = paste0(sapply(c(HoursSpent, MinutesSpent, SecondsSpent), function(x) {formatC(x, width = 2, format = "d", flag = "0")}), collapse = ":")
  print(paste0(noquote("Total Time spent Run the Algorithm and Exporting everything (HH:MM:SS): "), noquote(TotalTime)))
}
