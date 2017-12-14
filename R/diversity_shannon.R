#' Surface modeling of shannon diversity index.
#'
#' This function calculate shannon diversity index raster according to a SpatialPointDataframe object(POI).
#' @param x a SpatialPointsDataframe object
#' @param poi_type character.column name indicating POI types
#' @param extent.ras extent object.spaital extent of output raster.If missing,the extent of x will be used.
#' @param proj character.the project string of output.The defalut value is epsg:3857
#' @export
#' @return RasterLayer
diversity_shannon<-function(x,poi_type=NULL,res=500,extent.ras=NULL,
                            proj='+init=epsg:3857'){
    library(dplyr)
    library(rgdal)
    library(maptools)
    library(raster)
    library(vegan)
    type_fld<-x@data[,poi_type]
    poi.data<-x[!is.na(type_fld)&type_fld!='',]
    poi.data.3857<-poi.data%>%spTransform(proj)
    unique_type<-unique(poi.data.3857@data[,poi_type])
    poi.unique<-lapply(unique_type, function(x) poi.data.3857[
        poi.data.3857@data[,poi_type]==x,])
    if(is.null(extent.ras)){
        ras<-raster(poi.data.3857,resolution=res)
    }else{
        ras<-raster(poi.data.3857,resolution=res,ext=extent.ras)
    }
    count.type<-stack(lapply(poi.unique, function(x){
        rasterize(x,ras,field=1,fun='count',background=0)
    }    )      )
    count.type.val<-getValues(count.type)
    H<-diversity(count.type.val)
    H.sp<-cbind(coordinates(ras),H)%>%data.frame
    coordinates(H.sp)<-~x+y
    H.ras<-rasterize(H.sp,ras,field='H',fun=mean)
    crs(H.ras)<-proj
    H.ras
}
