#' @title AVM modeling of each city
#'
#' @description AVM modeling of each city
#' @param city_code City's abbr
#' @param proptype Property type of house area, 11---residence, and else like offical area
#' @param bestmodel if use the best model(TRUE) or all models
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param modelpath1 The path storing models and tables with ha/poi information
#' @param modelpath2 The path storing tif
#' @export
#' @return write tif files
avm_city <- function(city_code,proptype,model.list,bestmodel,host,port,user,password,dbname,modelpath1,modelpath2){
  # avm_city(city_code='qd',proptype='11',model.list=list(ols=T,rlm=T,svm=T,rft=T,gwr=F,xgb=T),bestmodel=TRUE,
  # host="192.168.3.142",port=3306,user="edi", password="cityhouse",dbname="cityre_qingdao",modelpath1 = ".",modelpath2 = "./models.avm")
  # link the database
  con<-dbconnect(host=host, port=port,user=user, password=password, dbname=dbname)
  # con <- dbconnect(host="192.168.3.142",port=3306,user="edi",password="cityhouse",dbname="cityre_qingdao")
  # city_code <- "qd"; proptype <- "11"; modelpath1 <- "."; modelpath2 <- "./models.avm"; month_offset <- "-2"
  # read data from database
  tabln.vec<-loadData(con=con,month_offset = 0)
  # delete the data out of a city
  tabln.vec$ha_info.sp<-remove.spatial.outlier(object = tabln.vec$ha_info.sp,city_code = city_code)
  # poi raster
  poi.data.list<-getpoi.ras(con=con,tabln.vec = tabln.vec)
  # put ha_info to ha_info.sp
  tabln.vec$ha_info.sp<-poi.data.list$ha_info.sp
  # prepare the data set for modeling
  data.sets.all<-avm.data.train(tabln.vec,proptype = proptype, modelpath1 = modelpath1, city_code = city_code)
  # split the training data and testing data. Maybe the testing data will not be used
  data.sets<-data.split(data =data.sets.all,train.rate = 0.8)
  # create the model
  avm.models<-avm.fit(data.train = data.sets$train,dependentY = 'saleprice',ntree = 500,nround = 1000, 
                      model.list = model.list)
  # close all databases
  killDbConnections()
  ##################################################################################################################
  # ensure modelpath2 exist
  if (!file.exists(modelpath2)) dir.create(modelpath2)
  # save the model
  saveRDS(avm.models,paste0(modelpath2,"/",city_code,"_saleprice_",proptype,"_",format(Sys.Date(),"%Y%m")))
  # save the ha_info and poi_info
  saveRDS(tabln.vec,paste0(modelpath2,"/",city_code,"_",format(Sys.Date(),"%Y%m"),"_","tabha.rds"))
  saveRDS(poi.data.list,paste0(modelpath2,"/",city_code,"_",format(Sys.Date(),"%Y%m"),"_","poiha.rds"))
  ##################################################################################################################
  # Predicating all valid area
  # check the valid bldg_code
  # bldgcodes <- c("11","12","13","21")
  # bldgcodes <- bldgcodes[paste0("bc",bldgcodes) %in% names(coefficients(avm.models$ols))]
  # if using the best model or all models?
  if (bestmodel == TRUE){
    bestmn <- select_model(avm.models,data.sets$test,model.list)
    # model.list[which(names(model.list)!=bestmn)] <- F
    bestm <- avm.models[which(names(model.list)==bestmn)]
    saveRDS(bestm,paste0(modelpath2,"/",city_code,"_saleprice_",proptype,"_",format(Sys.Date(),"%Y%m")))
  }
  # if (length(bldgcodes)>0){
  #   for (i in 1:length(bldgcodes))
  #   {
  #     rasterpred <- avm.predall(avm.models,tabln.vec,poi.data.list,data.sets.all,
  #                               model.list = model.list,bldg_code = bldgcodes[i])
  #     # save raster as type tif
  #     writeRaster(rasterpred,paste0(modelpath2,"/",city_code,"_",format(Sys.Date(),"%Y%m"),"_pty",proptype,"_bc",bldgcodes[i],"best.tif"),
  #                 format="GTiff",overwrite=TRUE)
  #   }
  # }else{
  #   rasterpred <- avm.predall(avm.models,tabln.vec,poi.data.list,data.sets.all,
  #                             model.list = model.list,bldg_code = "11")
  #   # save raster as type tif
  #   writeRaster(rasterpred,paste0(modelpath2,"/",city_code,"_",format(Sys.Date(),"%Y%m"),"_pty",proptype,"_bc",".tif"),
  #               format="GTiff",overwrite=TRUE)
  # }
  # 
  # the result：log(saleprice)
}

