#' compute distance between ha and its nearest POI(parallel).
#'
#' function which compute distance between ha and its nearest POI.
#' @param ha SpatialPointDataframe. a ha spatial points data
#' @param poi SpatialPointDataframe. a POI spatial points data
#' @export
#' @return RasterLayer
#' @details The unit of distance is meters(great circle distance).This function will utilize parallel package to speedup the computation.
ha_poi_mindisti.par<-function(ha,poi){
  library(parallel)
  cl=makeCluster(detectCores()/2)
  dist.i=function(i){
    1e3*spDists(ha[i,],poi)%>%
      as.numeric%>%min(na.rm = T)
  }
  clusterEvalQ(cl, {
    ## set up each worker.  Could also use clusterExport()
    library(dplyr)
    library(rgdal)
  })
  clusterExport(cl, c("ha","poi",'dist.i'),envir=environment())
  df7 = parSapply(cl, X = 1:nrow(ha),FUN=dist.i)
  stopCluster(cl)
  df7
}
#' function which compute minimum distance raster between specified POI and ha.
#' @param poi_name character. name of POI SpatialPointDataframe object
#' @param ext extent of output raster
#' @param list list object.If not NULL, variable will be extracted from list by the name of obj_name.
#' @export
#' @return dataframe
getMindist<-function(poi_name,ha=ha_poi.sp,list=NULL){
  cat('compute minimum distance between ha and ',poi_name,'\n')
  if(is.null(list)){
    poi.curr<-get(poi_name)# get object by a string
  }else{
    poi.curr<-list[[poi_name]]# get object from a list
  }
  if(nrow(ha)<=50){
      # cat('serial computing')
      ha_poi_mindisti.for(ha = ha,poi =poi.curr)
  }else{
      # cat('parallel computing')
      ha_poi_mindisti.par(ha = ha,poi =poi.curr)
  }
}

ha_poi_mindisti.for<-function(ha,poi){
  dist.mat<-1:nrow(ha)
  for(i in 1:nrow(ha)){
    dist.mat[i]<-1e3*spDists(ha[i,],poi)%>%
      as.numeric%>%min(na.rm = T)
  }
  dist.mat
}
ha_poi_mindisti.app<-function(ha,poi){
  dist.i=function(i){
    1e3*spDists(ha[i,],poi)%>%
      as.numeric%>%min(na.rm = T)
  }
  sapply(1:nrow(ha),dist.i)
}

# system.time(ha_busi.dist.1<-
#                 ha_poi_mindist(ha = ha_poi.sp,poi =busi.poi.sp ))
# system.time(ha_busi.dist.2<-
#                 ha_poi_mindist(ha = ha_poi.sp,poi =busi.poi.sp ))
# system.time(ha_busi.dist.3<-
#                 ha_poi_mindisti.par(ha = ha_poi.sp,poi =busi.poi.sp))
# cat(identical(ha_busi.dist.1,ha_busi.dist.2))
# cat(identical(ha_busi.dist.1,ha_busi.dist.3))
