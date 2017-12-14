#' extentd the extent by the distance(unit meters)
#'
#' function which extentd a specified extent object by the distance.
#' @param extent.city extent object
#' @param distance a numerica value which indicates the distance will be extended to the extent of a city.
#' @export
#' @return a extended extent object
#' @details The unit of distance is meters.
extend.extent<-function(extent.city=NULL,distance=5e4){
    extent.city@xmin<-extent.city@xmin-distance
    extent.city@xmax<-extent.city@xmax+distance
    extent.city@ymin<-extent.city@ymin-distance
    extent.city@ymax<-extent.city@ymax+distance
    extent.city
}


#' generating of POI rasters
#'
#' This function will computing distances between ha and POI, density ,and diversity raster of POI
#' @param month_offset offset of month for loading data.The default is -2 indiciting reading data in the previous moth before the last month
#' @param resto interger value indicing resolution of raster
#' @param con connection of db
#' @export
#' @return list of POI rasters

getpoi.ras<-function(month_offset = -2,resto=500,
                     con=NULL,
                     tabln.vec=NULL
                     ){
    library(dplyr)
    if(is.null(con)){
        con<-dbconnect()
    }
    if(is.null(tabln.vec)){
        tabln.vec<-loadData(con=con,month_offset = month_offset)
    }
    # extent for current city
    library(raster)
    ha_info.sp<-tabln.vec$ha_info.sp
    extent.city<-ha_info.sp%>%
        spTransform(CRSobj = '+init=epsg:3857')%>%extent
    extent.city<-extend.extent(extent.city = extent.city)
    library(stringr)
    # extract all poi spatial dataframe
    which.poi.sp<-stringr::str_detect(names(tabln.vec),pattern = '\\.poi\\.sp')
    poi.sp.names<-names(tabln.vec)[which.poi.sp]
    # get all POI density
    poi.dens<-sapply(X = poi.sp.names, getDensity
                     ,list=tabln.vec,resto=resto,extent.ras=extent.city)
    #extract value from raster(density surface) to points(ha_info.sp)
    poi.dens.pts<-sapply(poi.dens,raster::extract,y=ha_info.sp,sp=F)
    colnames(poi.dens.pts)<-paste0(colnames(poi.dens.pts),'_dens')
    # compute shannon's index of POI
    ha_poi.sp<-tabln.vec$ha_poi.sp
    poi.diversity<-
        diversity_shannon(ha_poi.sp,poi_type ='ha_cl_name_child',
                          extent.ras =extent.city )
    #extract value from raster(diversity surface ) to points(ha_info.sp)
    poi.diversity.pts<-raster::extract(x = poi.diversity,y=ha_info.sp,sp=F)
    # get all POI mindist with parallel computation
    poi.mindist<-sapply(X = poi.sp.names, getMindist,ha=ha_info.sp,list=tabln.vec)
    colnames(poi.mindist)<-paste0(colnames(poi.mindist),'_mindist')
    ha_info.sp@data<-cbind(ha_info.sp@data,
                           poi.dens.pts,
                           poi.diversity.pts,
                           poi.mindist)%>%as.data.frame
    poi.data<-list(poi.dens,poi.dens.pts,poi.diversity,poi.diversity.pts,poi.mindist,ha_info.sp)
    names(poi.data)<-c('poi.dens','poi.dens.pts','poi.diversity','poi.diversity.pts','poi.mindist','ha_info.sp')
    poi.data
}

#' generating of POI rasters(ver.2)
#'
#' This function will computing distances between ha and POI, density ,and diversity raster of POI
#' @param yearmonth character values indicting year and month. The default is previous month before the last month.
#' @param resto interger value indicing resolution of raster
#' @param con connection of db
#' @export
#' @return list of POI data

getpoi.ras2<-function(yearmonth=paste(
    as.numeric(format( seq(Sys.Date(), by = "-2 month", length = 2)[2],'%Y')),
    as.numeric(format( seq(Sys.Date(), by = "-2 month", length = 2)[2],'%m')),
    sep = '-'),    resto=500,
                     con=NULL,
                     tabln.vec=NULL
){
        library(magrittr)
        library(stringr)
        yearmonth.ymid<-stringr::str_extract(yearmonth,'[0-9][0-9][0-9][0-9]')%>%as.numeric()*12+
            stringr::str_replace(yearmonth,'[0-9][0-9][0-9][0-9]-','')%>%as.numeric()
        cur.ymid<-as.numeric(format(Sys.Date(),'%m'))+as.numeric(format(Sys.Date(),'%Y'))*12
        month_offset<-yearmonth.ymid-cur.ymid
        getpoi.ras(resto=resto,con=con,month_offset = month_offset,tabln.vec = tabln.vec)

}
