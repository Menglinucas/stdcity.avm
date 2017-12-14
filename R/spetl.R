#' Out-of-China tesing.
#'
#' THis function will be useful for cleanning data out of china
#'
#' @param lat lat,required
#' @param lon lon,required
#' @return FALSE indiciting not out of china
#' @export
#' @examples
#' outofChina(lat = 120,lon =30 )
#' outofChina(lon = 120,lat = 30 )
outofChina <- function(lat, lon){
  if(lon < 72.004 | lon > 137.8347) return(TRUE)
  if(lat < 0.8293 | lat > 55.8271) return(TRUE)
  return(FALSE)
}

#' finding which rows of coordinates are inside China.
#'
#' This function is for finding which rows of coordinates are inside China.
#'
#' @param x SpatialPointDataFrame
#' @return index of which data are inside China.
#' @export
#' @examples
#' data(meuse.grid)
#' coordinates(meuse.grid) <- ~x+y
#' which.in.china(meuse.grid)
which.in.china<-function(x){
  coords.xy<-coordinates(x)
  sapply(1:nrow(x), function(i){
    return (!outofChina(coords.xy[i,1],lat=coords.xy[i,2]))
  })
}


#' vectorize points.
#'
#' This function is useful for spatialize raw table data with coordinates
#'
#' @param data_ raw data. When \code{data_} is not null, \code{con} will be ignored
#' @param con connection of db. Either \code{data_} or \code{con} must not be null.
#' @param x fieldname of lon.The default is 'x'
#' @param y fieldname of lat,The default is 'y'
#' @param sqlcmd sql query statement
#' @param remove.dup logic parameter indicting wherther to delete duplicated coordinates,The defalut is T
#' @return SpatialPointDataFrame
#' @import dplyr
#' @import sp
#' @export
#' @examples
#' plot(sp.pts(x='x',y='y'))
sp.pts<-function(data_=NULL,con=dbconnect(),x='x',y='y',sqlcmd=NULL,remove.dup=T){
    library(dplyr)
    if(is.null(data_)&&is.null(con)){
        stop('必须提供data或数据库连接con中其中一个!')
    }
    if(is.null(data_)){
        data_<-dbGetQuery(con,sqlcmd)
    }
    data_$x_<-data_[,x]
    data_$y_<-data_[,y]
    data_<-data_%>%
        dplyr::filter(!is.na(x_)&!is.na(y_))
    data_<-data_[which.in.china(cbind(data_$x_,data_$y_)),]
    # data_<-data_%>%
    #     group_by(ha_code,name,dist_code,dist_name,ha_cl_code,ha_cl_name,cl_code,ha_cl_name_child)%>%
    #     summarise(x_=mean(x_,trim=0.2),y_=mean(y_,trim=0.2))%>%as.data.frame
    coordinates(data_)<- ~x_+y_
    proj4string(data_)<-'+init=epsg:4326'
    if(remove.dup){
        return(remove.duplicates(data_))
    }else{
        return(data_)
    }
}

# tables.names<-function(){
#   c('ha','ha_kindred','ha_cl','district',
#     'ha_bldg_type','bldg_type',
#     'ha_position','ha_phases',
#     'HaPrice_3',
#     'traffic_route','traffic_station','traffic_vehicle')
# }

sql.etl<-function(month_offset=-1,
                  ymid=as.numeric(format(Sys.Date(),'%m'))+month_offset+as.numeric(format(Sys.Date(),'%Y'))*12){
    sqlcmd.pre<-vector(mode = 'character',length = 8)
    sqlcmd.pre.names<-c('ha_info','ha_bldg','ha_gps','ha_phase',
                        'ha_price','ha_poi','ha_poi_gps','routes')
    sqlcmd.pre[1]<-"
        select ha.ha_code, ha_kindred.name, lower(ha.ha_cl_code) as ha_cl_code, ha_cl.ha_cl_name,
            ha.dist_code, dist_name, volume_rate,greening_rate
        from ha
        inner join ha_kindred on ha.ha_code = ha_kindred.ha_code
         and lower(ha_kindred.kindred_cl_code) = 'rn'
        left join ha_cl on ha.ha_cl_code = ha_cl.ha_cl_code
        left join district on district.dist_code = ha.dist_code
        where lower(ha.ha_cl_code) = 'pa' or lower(ha.ha_cl_code) = 'ob';"
    sqlcmd.pre[2]<-
    # "
    #     select ha_code,ha_bldg_type.bldg_code,bldg_type.bldg_type
    #         from ha_bldg_type
    #     left join bldg_type on ha_bldg_type.bldg_code = bldg_type.bldg_code"
    ##### keep  ha with the only one build type#################
    "select ff.* from (select ha_code,ha_bldg_type.bldg_code,bldg_type.bldg_type
            from ha_bldg_type
        left join bldg_type on ha_bldg_type.bldg_code = bldg_type.bldg_code
        GROUP by ha_code) ff
        left join (SELECT ha_code, count(bldg_code) cnt from ha_bldg_type GROUP by ha_code) cnt
        on cnt.ha_code=ff.ha_code and cnt.cnt=1
        where ((bldg_code>=11 AND bldg_code <=13) or (bldg_code>=21 AND bldg_code <=22))
    "
    sqlcmd.pre[3]<-"
        select ha.ha_code,avg(x) x,avg(y) y
            from ha
        left join ha_position on ha.ha_code = ha_position.ha_code
        where ha.ha_cl_code = 'pa' or ha.ha_cl_code = 'ob'
        group by ha_code"
    sqlcmd.pre[4]<-"
        select ha.ha_code,year(begin_time) buildyear
            from ha
        left join ha_phases on ha.ha_code = ha_phases.ha_code
        where ha.ha_cl_code = 'pa' or ha.ha_cl_code = 'ob'
        and ha_phase_code=3 and confirm_flag > 0;"
    sqlcmd.pre[5]<-"
        select s.hacode as ha_code,s.unitprice as saleprice,p.proptype,p.buildyear,bldgcode as bldg_code
        from qd_forsale s,qd_prop p
        where s.propcode=p.propcode
        and date_format(offertm,'%Y-%m')=date_format(now(),'%Y-%m')
        and s.showorder=1
        and s.Flag>=1
        and ((p.bldgcode>=11 AND p.bldgcode <=13) or (p.bldgcode>=21 AND p.bldgcode <=22))"
    # select ymid,ha_code,proptype,saleprice,
    #     saletotalprice,salebldgarea,salecount,rentprice,
    #     renttotalprice,rentbldgarea,rentCount
    # from HaPrice_3
    # sqlcmd.pre[5]<-paste(sqlcmd.pre[5],' where ymid=',ymid)
    sqlcmd.pre[6]<-"
        select ha.ha_code, ha_kindred.name,ha.dist_code, dist_name, ha.cl_code,
            ha_cl.ha_cl_name, lower(ha.ha_cl_code) as ha_cl_code, ha_cl_child.ha_cl_name as ha_cl_name_child
        from ha
        inner join ha_kindred on ha.ha_code = ha_kindred.ha_code
        and lower(ha_kindred.kindred_cl_code) = 'rn'
        left join ha_cl on ha.ha_cl_code = ha_cl.ha_cl_code
        left join ha_cl_child on ha.cl_code=ha_cl_child.cl_code
        left join district on district.dist_code = ha.dist_code
        where lower(ha.ha_cl_code) != 'pa' and lower(ha.ha_cl_code) != 'ob'
    "
    sqlcmd.pre[7]<-"
        select ha.ha_code, avg(x) x, avg(y) y
        from ha
        left join ha_position on ha.ha_code = ha_position.ha_code
        where lower(ha.ha_cl_code) != 'pa' and lower(ha.ha_cl_code) != 'ob'
        group by ha_code
    "
    sqlcmd.pre[8]<-"
    select traffic_route.route_code, traffic_route.route_name, traffic_station.station_code,
        traffic_station.station_name, traffic_station.longitude, traffic_station.latitude,
        traffic_vehicle.vehiclecode
    from traffic_route
    left join traffic_station on traffic_route.route_code = traffic_station.route_code
    left join traffic_vehicle on traffic_route.route_code = traffic_vehicle.routecode
    "
    data.frame(names=sqlcmd.pre.names,sqlcmd=sqlcmd.pre,stringsAsFactors = F)
}
#' loading data(ver.2)
#'
#' Loding data for gwr.avm
#' @param con connection od db server
#' @param proptype type code of property.The default is 11 indicing housing
#' @param yearmonth character values indicting year and month. The default is previous month before the last month.
#' @param remove.dup logic parameter indicting wherther to delete duplicated coordinates,The defalut is T
#' @export
#' @return basic data
loadData2<-function(con=dbconnect(),
                    proptype='11',
                    yearmonth=paste(
                        as.numeric(format( seq(Sys.Date(), by = "-2 month", length = 2)[2],'%Y')),
                        as.numeric(format( seq(Sys.Date(), by = "-2 month", length = 2)[2],'%m')),
                        sep = '-'),
                    remove.dup=T){
    library(magrittr)
    library(stringr)
    yearmonth.ymid<-stringr::str_extract(yearmonth,'[0-9][0-9][0-9][0-9]')%>%as.numeric()*12+
        stringr::str_replace(yearmonth,'[0-9][0-9][0-9][0-9]-','')%>%as.numeric()
    cur.ymid<-as.numeric(format(Sys.Date(),'%m'))+as.numeric(format(Sys.Date(),'%Y'))*12
    month_offset<-yearmonth.ymid-cur.ymid
    loadData(proptype=proptype,con=con,month_offset = month_offset,remove.dup = remove.dup)
}
#' loading data
#'
#' Loding data for gwr.avm
#' @param proptype type code of property.The default is 11 indicing housing
#' @param con connection od db server
#' @param month_offset offset value of month.The defalut is -1 which indicing reading data in the previous month
#' @param remove.dup logic parameter indicting wherther to delete duplicated coordinates,The defalut is T
#' @export
#' @return basic data list
loadData<-function(proptype='11',con=dbconnect(),month_offset=-1,remove.dup=T){
    needclose<-F
    if(is.null(con)){
        warning('trying connect database with default parameters because con is null!')
        con<-dbconnect()
        needclose<-T
    }
    sql.etl.all<-sql.etl(month_offset=month_offset)
    tabln.vec<-vector(mode='list',nrow(sql.etl.all))
    tables.names<-sql.etl.all$names
    names(tabln.vec)<-tables.names
    for (ii in 1:length(tables.names)){
        tbn<-tables.names[ii]
        cat('loading for ',tbn,'\n')
        sqlcmd<-sql.etl.all$sqlcmd[ii]
        tabln.vec[[ii]]<-dbGetQuery(con,sqlcmd)
    }
    # '22' is merged to '21'
    idx <- which(tabln.vec$ha_bldg$bldg_code=='22')
    tabln.vec$ha_bldg$bldg_code[idx] <- '21'
    tabln.vec$ha_bldg$bldg_type[idx] <- '别墅'
    tabln.vec$ha_bldg <- tabln.vec$ha_bldg[!duplicated(tabln.vec$ha_bldg),]
    tabln.vec$ha_bldg$bc11 <- 0
    tabln.vec$ha_bldg$bc12 <- 0
    tabln.vec$ha_bldg$bc13 <- 0
    tabln.vec$ha_bldg$bc21 <- 0
    idy <- sapply(1:nrow(tabln.vec$ha_bldg), 
                  function(i,df=tabln.vec$ha_bldg){
                        id <- which(substr(names(df),3,4)==df[i,'bldg_code'])}
                  )
    for (i in 1:nrow(tabln.vec$ha_bldg)){
      tabln.vec$ha_bldg[i,idy[i]] <- 1
    }
    # delete no data of gps
    tabln.vec$ha_gps <- na.omit(tabln.vec$ha_gps)
    # To simplified handling with data, we can compute mean value of coordinates.
    tabln.vec$ha_gps<-tabln.vec$ha_gps%>%
        dplyr::filter(!is.na(x)&!is.na(y))%>%
        group_by(ha_code)%>%
        summarise(x=mean(x,trim=0.2),y=mean(y,trim=0.2))%>%as.data.frame

    # # To simplified handling with buildyear,we can compute mean value of buildyear.
    # tabln.vec$ha_phase<-tabln.vec$ha_phase%>%
    #     dplyr::filter(!is.na(buildyear))%>%
    #     group_by(ha_code)%>%
    #     summarise(buildyear=round(mean(buildyear,trim=0.2)))%>%as.data.frame

    #  notice: some ha have no coordinates
    cat('create ha_info.sp','\n')
    if(proptype=='11'){
        tabln.vec$ha_info.sp<-merge(tabln.vec$ha_info,tabln.vec$ha_gps,by='ha_code')%>%
                              dplyr::filter(ha_cl_code=='pa')
    }else{
        tabln.vec$ha_info.sp<-merge(tabln.vec$ha_info,tabln.vec$ha_gps,by='ha_code')%>%
                              dplyr::filter(ha_cl_code=='pa' | ha_cl_code=='ob')
    }
    
    # commonly ha_price is not used here, this data is absolutely deleted in pssale.R
    ymid=as.numeric(format(Sys.Date(),'%m'))+month_offset+as.numeric(format(Sys.Date(),'%Y'))*12
    tabln.vec$ha_price<-tabln.vec$ha_price%>%mutate(
        year=ifelse(ymid%%12,floor(ymid/12),floor(ymid/12)-1),
        month=ifelse(ymid%%12,ymid-floor(ymid/12)*12,12))
    
    # # add ha_phase to ha_info.sp
    # tabln.vec$ha_info.sp<-merge(tabln.vec$ha_info.sp,tabln.vec$ha_phase, all.x=T, by='ha_code') # join buildyear
    # # add ha_bldg to ha_info.sp
    # bldg <- table(tabln.vec$ha_bldg[1:2]) %>% unclass() %>% as.data.frame()
    # names(bldg) <- paste0("bc",names(bldg))
    # bldg$ha_code <- row.names(bldg)
    # row.names(bldg) <- c(1:nrow(bldg))
    # # tabln.vec$ha_bldg$bldg_code<-as.character(tabln.vec$ha_bldg$bldg_code)
    # tabln.vec$ha_info.sp<-merge(tabln.vec$ha_info.sp,bldg, all.x=T, by='ha_code') # join bldg_type
    
    # convert dataframe to spatial
    tabln.vec$ha_info.sp<-sp.pts(data_ =tabln.vec$ha_info.sp,remove.dup=remove.dup)
    
    cat('create ha_poi_gps','\n')
    tabln.vec$ha_poi.sp<-tabln.vec$ha_poi_gps%>%
        filter(!is.na(x)&!is.na(y))%>%
        group_by(ha_code)%>%
        summarise(x=mean(x),y=mean(y))%>%as.data.frame
    tabln.vec$ha_poi.sp<-merge(tabln.vec$ha_poi,
                               tabln.vec$ha_poi.sp,by='ha_code', all.x=T)
    tabln.vec$ha_poi.sp<-sp.pts(tabln.vec$ha_poi.sp,remove.dup=remove.dup)
    cat('created edu.poi.sp','\n')
    tabln.vec$edu.poi.sp<-tabln.vec$ha_poi.sp[
        !is.na(tabln.vec$ha_poi.sp$ha_cl_name)&
        !is.na(tabln.vec$ha_poi.sp$ha_cl_name_child)&
        tabln.vec$ha_poi.sp$ha_cl_name=='教育'&
            (tabln.vec$ha_poi.sp$ha_cl_name_child=='中学'|
                 tabln.vec$ha_poi.sp$ha_cl_name_child=='小学'|
                 tabln.vec$ha_poi.sp$ha_cl_name_child=='幼儿园'),]
    tabln.vec$hosi.poi.sp<-
        tabln.vec$ha_poi.sp[!is.na(tabln.vec$ha_poi.sp$ha_cl_name_child)&
                                tabln.vec$ha_poi.sp$ha_cl_name_child=='医院',]
    cat('created transportation POI(trans.poi.sp)','\n')
    if(nrow(tabln.vec$routes)>0){
        tabln.vec$trans.poi.sp<-tabln.vec$routes%>%
            dplyr::filter(!is.na(longitude)&!is.na(latitude))
        # coordinates(tabln.vec$trans.poi.sp)<-~longitude+latitude
        # proj4string(tabln.vec$trans.poi.sp)<-'+init=epsg:4326'
        tabln.vec$trans.poi.sp<-sp.pts(tabln.vec$trans.poi.sp,x='longitude',y='latitude',remove.dup=remove.dup)
    }else{
        warning('have no routes!!!')
        # tabln.vec$trans.poi.sp<-NULL
    }

    cat('created business POI(busi.poi.sp)','\n')
    tabln.vec$busi.poi.sp<-
        tabln.vec$ha_poi.sp[
            !is.na(tabln.vec$ha_poi.sp$ha_cl_name_child)&
                (tabln.vec$ha_poi.sp$ha_cl_name_child=='商圈'|
                     tabln.vec$ha_poi.sp$ha_cl_name_child=='商业街'|
                     tabln.vec$ha_poi.sp$ha_cl_name_child=='商场'|
                     tabln.vec$ha_poi.sp$ha_cl_name_child=='市场'),]
    if(needclose) dbDisconnect(con)
    tabln.vec

}
# convert.bldg<-function(bldg_code=NULL){
#     for(i in 1:length(bldg_code)){
#         if(bldg_code[i]=='22') bldg_code[i]='21'
#         if(bldg_code[i]=='23'|
#            bldg_code[i]=='24') bldg_code[i]='99'
#     }
#     bldg_code
# }
