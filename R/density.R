#' Surface modeling of kernel density.
#'
#' This function calculate the kernel density raster according to a SpatialPointDataframe object.
#' @param x a SpatialPointsDataframe object
#' @param resto numeric.resolution of output raster
#' @param extent.ras extent object.spaital extent of output raster.If missing,the extent of x will be used.
#' @param negative.keep logical value indicating whether to keep negative value.If false, all negative value will be assigned to zero.
#' @param adjust numerical value indicating adjust value multiplied to bandwidth.The default value is 1.
#' @export
#' @return RasterLayer
#' @examples
#' n<-100
#' x<-rnorm(n,mean=100)
#' y<-rnorm(n,mean=40)
#' xy<-cbind(x,y)
#' vals<-1:n
#' p<-data.frame(xy,name=vals)
#' coordinates(p)<-~x+y
#' proj4string(p)<-'+init=epsg:4326'
#' ras.dens<-density_ras(p)
#' plot(ras.dens)
density_ras<-function(x,resto=500,extent.ras=NULL,proj='+init=epsg:3857',negative.keep=F,adjust=1,...){
    library(raster)
    library(spatstat)
    library(dplyr)
    library(maptools)
    library(rgdal)
    if(is.na(proj4string(x))){
        warning('Spatial reference system of x is NA! Suppose it is WGS84!!')
        proj4string(x)<-'+init=epsg:4326'
    }
    x.proj<-x%>%spTransform(proj)
    if(is.null(extent.ras)){
        extent.x<-extent(x.proj)
        if(nrow(x)<2&extent.x@xmin==extent.x@xmax) {
            warning('Missing extent of the whole region!The coverage extent of points will be used!')
            extent.x@xmax<-extent.x@xmin+resto
            extent.x@ymax<-extent.x@ymin+resto
        }
        ras.res<-raster(resolution=resto,ext=extent.x)
        extent.ras<-extent.x
    }else{
        ras.res<-raster(resolution=resto,ext=extent.ras)
    }
    crs(ras.res)<-proj
    if(nrow(x)<2) {
        ras.res[]<-0
        return (ras.res)
    }
    pts.ppp<-as.ppp(x.proj)
    bw1<-bw.diggle(pts.ppp)*adjust
    ras<-density(pts.ppp,sigma=bw1,...)%>%raster
    ress<-res(ras)
    ras.res<-(ras*resto*resto)
    crs(ras.res)<-proj
    ras.res.dsn<-file.path(tempdir(),'ras_res.img')
    writeRaster(ras.res,ras.res.dsn,'HFA',overwrite=T)
    library(gdalUtils)
    ras.res.resample<-file.path(tempdir(),'ras_res_r.img')
    gdalwarp(ras.res.dsn,ras.res.resample,of='HFA',tr=c(resto,resto),
             te=c(extent.ras@xmin,extent.ras@ymin,extent.ras@xmax,extent.ras@ymax),
             overwrite=T,dstnodata = 32768)
    ras.res<-raster(ras.res.resample)
    # resample(y=ras.res)
    # resto * resto is map unit scale factor, and the map unit of 3857 is meter
    crs(ras.res)<-proj
    ras.res[is.na(ras.res[])]<-0
    if(!negative.keep){
        ras.res[ras.res[]<0]<-0
    }
    ras.res
}
#' compute density raster for a POI.
#'
#' function which compute density raster for a POI specified by a string.
#' @param obj_name character. name of POI SpatialPointDataframe object
#' @param list list object.If not NULL, variable will be extracted from list by the name of obj_name.
#' @param ... other parameters to be passed to \code{\link{density_ras}}
#' @seealso \code{\link{density_ras}}
#' @export
#' @return RasterLayer
getDensity<-function(obj_name,list=NULL,...){

    cat('compute density of ',obj_name,'\n')
    if(is.null(list)){
        poi.curr<-get(obj_name)# get object by a string
    }else{
        poi.curr<-list[[obj_name]]# get object from a list
    }
    ras.res<-density_ras(poi.curr,...)# function in R/density_ras.R
    ras.res
}
