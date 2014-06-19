#'@name ipdw
#'@title Inverse Path Distance Weighting
#'@author Joseph Stachelek
#'@param spdf SpatialPointsDataFrame object
#'@param costras RasterLayer. Cost raster
#'@param range numeric. Range of interpolation neighborhood
#'@param paramlist character. String representing parameter names
#'@param overlapped logical. Default is FALSE, specify TRUE if some points lie on top of barriers
#'@param yearmon character. String specifying the name of the spdf
#'@param removefile logical. Remove files after processing?
#'@param step numeric. Number of sub loops to manage memory during raster processing.
#'@return RasterStack
#'@details This is a high level function that interpolates a SpatialPointsDataFrame object in a single pass. 
#'@import raster
#'@import gdistance
#'@export
#'@examples
#' #see vignette


###this is the highest level function that combines path distance generation with interpolation

'ipdw'<-function(spdf,costras,range,paramlist,overlapped=FALSE,yearmon="default",removefile=TRUE,step=16){
  
  #pathdistGen
  pathdists<-pathdistGen(spdf,costras,range,step,yearmon)
  
  
    
  #ipdwInterp
  final.ipdw<-ipdwInterp(spdf,pathdists,paramlist,yearmon,removefile=TRUE)
  
  return(final.ipdw)  
  
}