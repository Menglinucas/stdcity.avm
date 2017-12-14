#' read data by a sql statement
#'
#' This function will read data by given sql query statement
#' @param con connection of db server,which returned by \code{\link{dbconnect}} or similar function in the RMySQL
#' @param sql_cmd sql query statement
#' @export
#' @return model list of avm
read.sql<-function(con=NULL,sql_cmd=NULL){
    if(is.null(con)|is.null(sql_cmd)){
        stop('con or sql_cmd can not be NULL')
    }
    dbGetQuery(con,sqlcmd)
}

#' read deal data from database
#'
#' This function read deal data according given parameters
#' @param con connection of db server,which returned by \code{\link{dbconnect}} or similar function in the RMySQL
#' @param table.names table names of dead data. The defalut is 'qd_forsale'
#' @param year year,the default value is the year of date with the previous month of the last month
#' @param month month,,the default value is the month of date with the previous month of the last month
#' @param proptype type code of property.The default is 11
#' @param minsale Allowed minimum value of saleprice.The defalut is 2000
#' @param minrent Allowed minimum value of rentprice.The default is 1
#' @export
#' @return list of AVM models
read.deal<-function(con=NULL,table.names='qd_forsale',
                    year=as.numeric(format(seq(Sys.Date(), by = "-2 month",length = 2)[2],'%Y')),
                    month=as.numeric(format(seq(Sys.Date(),by = "-2 month",length = 2)[2],'%m')),
                    proptype='11',
                    minsale=2000,
                    minrent=1,
                    var.except=NULL){
    if(is.null(con)){
        stop('con can not be NULL')
    }
    unitprice.min<-ifelse(table.names=='qd_forsale',minsale,minrent)
    sql_cmd<-paste("
    select s.ID, s.hacode as ha_code,s.unitprice,s.offertm,p.proptype,p.bldgcode,
    p.buildyear as buildyear_d,p.bheight,p.floor,p.br,p.lr,p.bldgarea,
    p.strucode,p.facecode,p.int_deco,p.propRT,s.dealcode
        from ",table.names," s,qd_prop p
    where s.propcode=p.propcode
        and MONTH(offertm)=",month," and YEAR(offertm)=",year,"
        and proptype=",proptype,"
        and unitprice >=",unitprice.min,"
        and s.showorder=1
        and s.Flag=1
    ")
    data.deal<-dbGetQuery(con,sql_cmd)
    if(!is.null(var.except)){
        data.deal<-data.deal%>%dplyr::select(-one_of(var.except))
    }
    data.deal
}


# This function calculates distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2)
{
    rad <- pi/180
    a1 <- lat1 * rad
    a2 <- long1 * rad
    b1 <- lat2 * rad
    b2 <- long2 * rad
    dlon <- b2 - a2
    dlat <- b1 - a1
    a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R <- 6378.145
    d <- R * c
    return(d)
}

#' remove spatial outliers
#'
#' This function help to drop sptaital outliers
#' @param object data object, can be dataframe or spaital dataframe.
#' @param city_code the code of city to find boundary of city,required .
#' @param x field name of lon,the default is 'x'
#' @param y field name of lat,the default is 'y'
#' @param probs prob of quantile for detecting abnormal coordiantes
#' @param type type of quantile, see type in \code{\link{quantile}}
#' @param xtrim trim for x in quantile
#' @param ytrim trim for y in quantile
#' @export
#' @return data cleaned
remove.spatial.outlier<-function(object,city_code=NULL,x="x",y="y",
                                 probs=0.9995,type=8,xtrim=0.2,ytrim=0.2){
    xx=NULL
    yy=NULL
    if(is.null(city_code)){
        stop('city_code cannot be NULL!!!')
    }
    if(is(object,'Spatial')){
        xx=coordinates(object)[,1]
        yy=coordinates(object)[,2]
    }else{
        xx=object[,x]
        yy=object[,y]
    }
    if( !city_code %in% city.poly$city_code){
        dist<-earth.dist(xx,yy,mean(xx,trim=xtrim),mean(yy,trim=ytrim))
        idx<-dist< quantile(dist,probs=probs,type=type)
        warning(city_code,'not existing in city_code, using distance quantile detection...')
    }else{
        extent.city<-extent(city.poly[!is.na(city.poly$city_code) & city.poly$city_code==city_code,])
        idx<- xx>= extent.city@xmin & xx <= extent.city@xmax  &  yy>= extent.city@ymin & yy <= extent.city@ymax
    }
    object[idx,]
}
