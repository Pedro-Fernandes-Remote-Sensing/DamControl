VectorBuilder = function(Threshold_NDVI, Threshold_RGMSum){
  Vector1 <<- c(-Inf, Threshold_NDVI, 1, Threshold_NDVI, Inf, 0)
  Vector2 <<- c(-Inf, Threshold_RGMSum, 1, Threshold_RGMSum, Inf, 0)
  Vector3 <<- c(-Inf, 0, 0, 1, 1, 1, 1.9, Inf, 1)
}
