#' DB connection for testing
#'
#' connection for testing. It will print "It Works!" if success.
#'
#' @param user username for connection, default value is avm
#' @param password password for connection, default value is gwr
#' @param host host of db server
#' @param port port number of db server
#' @param dbname dbname of db server
#' @import RMySQL
#' @export
#' @examples
#' con<-dbconnect()
#' dbListTable(conn = con)
#' dbDisconnect(con)
dbconnect<-function(user='avm',password='gwr',host='db',port=3306,dbname='cityre'){
  options(stringsAsFactors = F)
  library(RMySQL)
  con <- NULL
  con <- dbConnect(MySQL(),user=user,password=password,host=host,port=port,dbname=dbname)
  # dbListTables(conn = con)
  if(!is.null(con)){
    cat('connected!','\n')
  }else{
    stop('Cant connect to MySQL')
  }
  # if(fixdebug && host=='db'){
  #     if(!is.null(con)){
  #         cat('fixing routes','\n')
  #     }
  # }
  dbExecute(con,'set names utf8')
  return (con)
}


#' dbconnect2
#'
#' connection for testing. It will print "It Works!" if success.
#'
#' @param user usename
#' @param password password
#' @param host host
#' @param dbname dbname
#' @param port port
#' @import RMySQL
#' @export
#' @examples
#' con<-dbconnect2()
#' dbListTable(conn = con)
#' dbDisconnect(con)
dbconnect2<-function(user = 'wangcl',
                     password = 'wangchenliang_CR_2017',
                     host='10.11.10.34',
                     dbname='cityre_beijing',
                     port=3307){
    con<-dbconnect(user=user,password = password,host=host,dbname=dbname,port=port)
    return (con)
}



#' setting up dependencies
#'
#' This function will install any dependencies of this package
#'
#' @import RMySQL
#' @export
#' @examples
#' install.dependencies()
install.dependencies<-function(){
   packages.names<-c(
       'devtools','RMySQL','sp','spgwr','raster','rgdal','dplyr','spatstat','permute','vegan','formula.tools','stringr',
       'randomForest','e1071','MASS','gbm','DEoptimR', 'robustbase', 'RcppArmadillo', 'GWmodel', 'xgboost', 'lubridate'
   )
   for(i in 1:length(packages.names)){
       if(!packages.names[i] %in% installed.packages()[,'Package'])
       install.packages(packages.names[i])
   }
   devtools::install_github('drsimonj/twidlr')
}
#' kill all db connections
#'
#' This function is useful for cleanning up any MySQL connections.
#'
#' @import RMySQL
#' @export
#' @examples
#' killDbConnections()
killDbConnections <- function () {
    library(RMySQL)
    all_cons <- dbListConnections(MySQL())
    print(all_cons)
    for(con in all_cons)
        dbDisconnect(con)
    cat(paste(length(all_cons), " connections killed."),'\n')
}

