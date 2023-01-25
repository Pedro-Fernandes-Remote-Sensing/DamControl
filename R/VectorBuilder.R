if(getRversion() >= "2.15.1")  utils::globalVariables(c("VectorNDVI", "VectorRGBSum", "VectorAlgorithm"))
#' Produces global variables VectorNDVI, VectorRGBSum and VectorAlgorithm
#'
#' @param Threshold_NDVI a number representing a threshold to be used to divide NDVI into a binary raster
#' @param Threshold_RGMSum a number representing a threshold to be used to divide RGBSum into a binary raster
#'
#' @return Returns nothing, used to produce Vector1, 2 and 3.
#' @export
VectorBuilder = function(Threshold_NDVI, Threshold_RGMSum){
  VectorNDVI <<- c(-Inf, Threshold_NDVI, 1, Threshold_NDVI, Inf, 0)
  VectorRGBSum <<- c(-Inf, Threshold_RGMSum, 1, Threshold_RGMSum, Inf, 0)
  VectorAlgorithm <<- c(-Inf, 0, 0, 1, 1, 1, 1.9, Inf, 1)
}
