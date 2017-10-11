
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- model parametres setting ---------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

# ============= ~~ setmodelPar  ======

#' modelPar.default
#' 
#' get a \bold{modelPar} object with default parametres
#' @return a \bold{modelPar} object with default parametres
#' @author Ruifei.Yin
#' @export
#' @family modelPar setting functions
modelPar.default <- function(){
  modelPar <- list()
  modelPar <- modelPar.title(modelPar)
  modelPar <- modelPar.univ(modelPar)
  modelPar <- modelPar.time(modelPar)
  modelPar <- modelPar.factor(modelPar)
  return(modelPar)
}


#' modelPar.title
#' 
#' set the title related parametres of the model 
#' 
#' The different between \code{modelPar.title} and \code{setmodelPar.title} is that the all pars of the former have default values, and the latter have not. The former is ussually used to to build the modelPar integrally, and the latter is ussually used to set the parametre individually.
#' @param modelPar a \bold{modelPar} object
#' @param modelID 
#' @param modelName
#' @param modelDesc
#' @return a \bold{modelPar} object
#' @author Ruifei.Yin
#' @export
#' @family modelPar setting functions
#' @examples
#' modelPar <- modelPar.default()
#' modelPar.title(modelPar,modelID="M_000001",modelName="HS300-eqwgt-momentumn")
modelPar.title <- function(modelPar   = modelPar.default(),
                           modelID   = ""    ,
                           modelName = ""    ,
                           modelDesc = ""){
  modelPar$title <- list(modelID   = modelID  ,
                         modelName = modelName,
                         modelDesc = modelDesc)
  return(modelPar)
}
#' @rdname modelPar.title
#' @export
setmodelPar.title <- function(modelPar,
                              modelID,
                              modelName,
                              modelDesc){
  if(!missing(modelID)) modelPar$title$modelID <- modelID
  if(!missing(modelName)) modelPar$title$modelName <- modelName
  if(!missing(modelDesc)) modelPar$title$modelDesc <- modelDesc
  return(modelPar)
}

#' modelPar.time
#' 
#' set the rebalancing date related parametres of the model
#' 
#' The different between \code{modelPar.time} and \code{setmodelPar.time} is that the all pars of the former have default values, and the latter have not. The former is ussually used to to build the modelPar integrally, and the latter is ussually used to set the parametre individually.
#' @param modelPar a \bold{modelPar} object
#' @param begT the begin date
#' @param endT the end date
#' @param rebFreq an interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s". See \code{\link{cut.Date}} for detail.
#' @param shiftby a integer,how many days the rebalancing date be shifted afterward
#' @param dure
#' @param dates an arbitrary vector of trading date, with class of Date. If param \code{dates} is not null, then all the other params will be invalidated.
#' @return a \bold{modelPar} object
#' @seealso \code{\link{getRebDates}}
#' @author Ruifei.Yin
#' @export
#' @family modelPar setting functions
modelPar.time <- function(modelPar = modelPar.default() ,
                          begT = Sys.Date()-lubridate::years(1),
                          endT = Sys.Date()-1 ,
                          rebFreq = "month" ,
                          shiftby = 0,
                          dure = NULL,
                          dates = NULL){ 
  modelPar$time <-list(begT     = begT,
                       endT     = endT,
                       rebFreq  = rebFreq,
                       shiftby  = shiftby,
                       dure     = dure,
                       dates    = dates)  
  return(modelPar)
}
#' @rdname modelPar.time
#' @export
setmodelPar.time <- function(modelPar,                          
                             begT,
                             endT,
                             rebFreq,
                             shiftby,
                             dure,
                             dates){ 
  if(!missing(begT)) modelPar$time$begT <- begT
  if(!missing(endT)) modelPar$time$endT <- endT
  if(!missing(rebFreq)) modelPar$time$rebFreq <- rebFreq
  if(!missing(shiftby)) modelPar$time$shiftby <- shiftby 
  if(!missing(dure)) modelPar$time$dure <- dure 
  if(!missing(dates)) modelPar$time$dates <- dates
  return(modelPar)
}


#' modelPar.univ
#' 
#' set the universe related parametres of the model 
#' 
#' The different between \code{modelPar.univ} and \code{setmodelPar.univ} is that the all pars of the former have default values, and the latter have not. The former is ussually used to to build the modelPar integrally, and the latter is ussually used to set the parametre individually.
#' @param modelPar a \bold{modelPar} object
#' @param indexID  the ID of the index("EIxxxx"), sector("ESxxxx"), or plate("EPxxxx"). The function will recognize the universe type through the first two character of \code{indexID}.
#' @param stocks
#' @param rm
#' @return a \bold{modelPar} object
#' @seealso \code{\link{getTS}}
#' @author Ruifei.Yin
#' @export
#' @family modelPar setting functions
modelPar.univ <- function(modelPar  = modelPar.default(),
                          indexID   = "EI000300",
                          stocks    = NULL,
                          rm        = NULL){
  modelPar$univ <- list(indexID  = indexID,
                        stocks   = stocks,
                        rm       = rm)
  return(modelPar)
}
#' @rdname modelPar.univ
#' @export
setmodelPar.univ <- function(modelPar, indexID, stocks, rm){
  if(!missing(indexID)) modelPar$univ$indexID <- indexID 
  if(!missing(stocks)) modelPar$univ$stocks <- stocks 
  if(!missing(rm)) modelPar$univ$rm <- rm 
  return(modelPar)
}




#' modelPar.factor
#' 
#' set the factor related parametres of the model 
#' 
#' The different between \code{modelPar.factor} and \code{setmodelPar.factor} is that the all pars of the former have default values, and the latter have not. The former is ussually used to to build the modelPar integrally, and the latter is ussually used to set the parametre individually.
#' @param modelPar a \bold{modelPar} object
#' @param factorFun a non-empty character string naming the function to get the factor scores.(See \code{\link{getTSF}})
#' @param factorPar a list, or a character string, containing the parameters of the \code{factorFun}.(See \code{\link{getTSF}})
#' @param factorDir a integer,should be 1 or -1 (1 for the positive factor,-1 for the negative one).(See \code{\link{getTSF}})
#' @param factorRefine a list. See detail in \code{\link{refinePar_default}}
#' @param factorName a character string. IF missing, then take a default name by function \code{default.factorName}. 
#' @param factorID
#' @param factorType
#' @param factorDesc
#' @param FactorList a list containing all the factor related parametres in it. See more detail in \code{\link{buildFactorList}}.
#' @return a \bold{modelPar} object.
#' @note if the parammeter \code{factorFun} is \code{"gf_lcfs"}, then the factorDir, factorName, factorID, factorType and factorDesc will be mapped automatically by \code{\link{CT_FactorLists}}.
#' @details if \code{FactorList} is missing, then the factor related parametres is set by the auguments separately, else, all the parametres is set by the \code{FactorList} once and for all.
#' @seealso \code{\link{getTSF}}
#' @author Ruifei.Yin
#' @export
#' @family modelPar setting functions
#' @examples
#' modelPar <- modelPar.default()
#' modelPar <- modelPar.factor(modelPar,
#'                            factorFun="gf.pct_chg_per",
#'                            factorPar=list(N=25),
#'                            factorDir=-1,
#'                            factorRefine= setrefinePar(refinePar_default("robust"),std_method="norm",na_method="median",outlier_method="mad"))
modelPar.factor <- function(modelPar = modelPar.default() , 
                            factorFun = "gf_demo" ,
                            factorPar  = list() ,
                            factorDir  = 1    ,
                            factorRefine  = refinePar_default("none") ,
                            factorName = default.factorName(factorFun,factorPar,factorDir), 
                            factorID = "" ,
                            factorType = ""   ,
                            factorDesc = ""   ,
                            FactorList ){
  if(missing(FactorList)){        
    FactorList <- buildFactorList(factorFun  = factorFun ,
                                  factorPar   = factorPar  ,  
                                  factorDir   = factorDir  ,
                                  factorRefine   = factorRefine ,
                                  factorName  = factorName , 
                                  factorID = factorID,
                                  factorType  = factorType ,
                                  factorDesc = factorDesc  )
  } 
  modelPar$factor <- FactorList
  return(modelPar)
}
#' @rdname modelPar.factor
#' @export
setmodelPar.factor <- function(modelPar ,                            
                               factorFun,
                               factorPar,
                               factorDir,
                               factorRefine,
                               factorName, 
                               factorID,
                               factorType,
                               factorDesc,
                               FactorList ){
  if(!missing(FactorList)){        
    modelPar$factor <- FactorList
  } else { 
    Fun <- if(!missing(factorFun)) factorFun else modelPar$factor$factorFun
    Par <- if(!missing(factorPar)) factorPar else modelPar$factor$factorPar
    Dir <- if(!missing(factorDir)) factorDir else modelPar$factor$factorDir
    # Firstly, if factorFun, factorPar or factorDir is not missing, automatically reset the factorName.    
    if(!missing(factorFun) || !missing(factorPar) || !missing(factorDir)){
      modelPar$factor$factorName <- default.factorName(Fun,Par,Dir)
    }
    # Secondly, if factorfun is 'gf_lcfs', automatically map the pars of factorDir, factorName, factorID, factorType and factorDesc and reset them. 
    if(Fun == "gf_lcfs"){        
      ID <- if(is.list(Par)) unlist(Par) else gsub("\"","",Par)
      tmpdf <- CT_FactorLists(factorID = ID)  
      if(NROW(tmpdf) == 0L) {
        warning(paste("Factor",QT(ID),"not exist in table QT_FactorScore!"))
      } else {
        modelPar$factor$factorName <- tmpdf$factorName
        modelPar$factor$factorDir <- tmpdf$factorDir
        modelPar$factor$factorID <- tmpdf$factorID
        modelPar$factor$factorType <- tmpdf$factorType
        modelPar$factor$factorDesc <- tmpdf$factorDesc
      }
    }    
    # Lastly, if any of pars is not missing, then it will be reset manually.
    if(!missing(factorName)) modelPar$factor$factorName <- factorName 
    if(!missing(factorDir)) modelPar$factor$factorDir <- factorDir 
    if(!missing(factorID)) modelPar$factor$factorID <- factorID 
    if(!missing(factorType)) modelPar$factor$factorType <- factorType
    if(!missing(factorDesc)) modelPar$factor$factorDesc <- factorDesc      
    if(!missing(factorFun)) modelPar$factor$factorFun <- factorFun 
    if(!missing(factorPar)) modelPar$factor$factorPar <- factorPar 
    if(!missing(factorRefine)) modelPar$factor$factorRefine <- factorRefine 
  }  
  return(modelPar)
}




#' @rdname modelPar.factor
#' @param factorLists See \code{\link{buildFactorLists}}.
#' @param wgts
#' @note \code{setmodelPar.factor_combi} set \code{modelPar} by \code{factorFun="getMultiFactor"} and  \code{factorPar=list(factorLists,wgts)}.  It is used usually in testing the multi-factor-model. See \code{\link{getMultiFactor}}.
#' @export
#' @examples
#' # -- setmodelPar.factor_combi
#' mp <- modelPar.default()
#' factorIDs <- c("F000008","F000001","F000006")
#' factorLists <- buildFactorLists_lcfs(factorIDs)
#' wgts <- c(0.5, 0.3, 0.2)
#' mp_m <- setmodelPar.factor_combi(mp, factorLists, wgts)
#' # -- setmodelPar.factor_combi by \code{buildFactorlist_combi}
#' factorlist_combi <- buildFactorList_combi(factorLists,wgts)
#' mp_m2 <- setmodelPar.factor(mp,FactorList = factorlist_combi)
setmodelPar.factor_combi <- function(modelPar,
                                     factorLists, wgts, 
                                     factorDir,
                                     factorRefine,
                                     factorName = "combi_factor", 
                                     factorID,
                                     factorType,
                                     factorDesc){
  
  mp_m <- setmodelPar.factor(modelPar=modelPar,
                             factorFun="getMultiFactor",
                             factorPar=list(factorLists,wgts), 
                             factorDir,
                             factorRefine,
                             factorName, 
                             factorID,
                             factorType,
                             factorDesc)
  return(mp_m)
}


#' @rdname modelPar.factor
#' @return buildFactorList return a object of \bold{FactorList}, a list of parametres of factor setting
#' @export
#' @examples
#' # -- set modelPar.factor by a factorlist
#' FactorList <- buildFactorList(factorFun="gf.pct_chg_per",factorPar=list(N=20))
#' modelPar <- modelPar.factor(modelPar,FactorList=FactorList)
buildFactorList <- function(factorFun = "gf_demo" ,
                            factorPar  = list() ,
                            factorDir  = 1 ,
                            factorRefine  = refinePar_default("none") ,
                            factorName = default.factorName(factorFun,factorPar,factorDir), 
                            factorID ="" ,
                            factorType = ""  ,
                            factorDesc = "" ){
  re <-list(factorFun  = factorFun ,
            factorPar   = factorPar  ,  
            factorDir   = factorDir  ,
            factorRefine   = factorRefine  ,   
            factorName  = factorName ,  
            factorID  = factorID,
            factorType  = factorType,
            factorDesc = factorDesc)   
  return(re)
}

#' @rdname modelPar.factor
#' @param factorID a character string of factorID, available in table \code{CT_FactorLists()} .
#' @return buildFactorList_lcfs get a object of \bold{FactorList} through "lcfs". See more detail in \code{\link{buildFactorLists_lcfs}}. 
#' @export
#' @examples
#' # -- build a factorlist through "lcfs"
#' FactorList2 <- buildFactorList_lcfs(factorID="F000001")
buildFactorList_lcfs <- function(factorID, factorRefine  = refinePar_default("none")
){
  re <- buildFactorList(factorFun = "gf_lcfs",
                        factorPar = list(factorID),
                        factorDir = CT_FactorLists(factorID=factorID)$factorDir,
                        factorRefine = factorRefine,
                        factorName = CT_FactorLists(factorID=factorID)$factorName,
                        factorID = factorID,
                        factorType = CT_FactorLists(factorID=factorID)$factorType,
                        factorDesc = CT_FactorLists(factorID=factorID)$factorDesc
  )
  return(re)
}

#' @rdname modelPar.factor
#' @export
buildFactorList_combi <- function(factorLists, wgts, 
                                  factorDir  = 1 ,
                                  factorRefine  = refinePar_default("none"),
                                  factorName = "combi_factor",
                                  factorID ="",
                                  factorType ="",
                                  factorDesc =""
                                  ){
  re <-list(factorFun="getMultiFactor",
            factorPar=list(factorLists,wgts),  
            factorDir   = factorDir  ,
            factorRefine   = factorRefine  ,
            factorName  = factorName ,  
            factorID  = factorID,
            factorType  = factorType,
            factorDesc = factorDesc)   
  return(re)
}

#' buildFactorLists
#' 
#' get a list of \bold{FactorList}, which is offen used in "multi-factor model builbing" or in "multifactor comparison".
#' @param ... one or more FactorList. See more detail in \code{\link{buildFactorList}}.
#' @note In function \code{buildFactorLists}, the settings of factorRefine will be changed samely by the given arguments, if they are not missing. 
#' @return a list of \bold{FactorList}
#' @export
#' @examples
#' FactorLists <- buildFactorLists(
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=60),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.float_cap",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.PE_ttm",
#'                   factorPar=list(),
#'                   factorDir=-1)
#' )
#' # - change the factor-refining method.
#' FactorLists2 <- buildFactorLists(
#'   buildFactorList(factorFun="gf.pct_chg_per",
#'                   factorPar=list(N=60),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.float_cap",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   buildFactorList(factorFun="gf.PE_ttm",
#'                   factorPar=list(),
#'                   factorDir=-1),
#'   factorRefine = refinePar_default()
#' )
buildFactorLists <- function(... , factorRefine){
  re <- list(...)
  if(!missing(factorRefine)){
    re <- lapply(re,function(x){x$factorRefine <- factorRefine;return(x)})
  }
  return(re)
}

setFactorListsRefinePar <- function(factorlists, factorRefine){
  re <- lapply(factorlists,function(x){x$factorRefine <- factorRefine;return(x)})
  return(re)
}


#' @rdname buildFactorLists
#' @param factorIDs a character vector of factorID, available in table \code{CT_FactorLists()} .
#' @note function \code{buildFactorLists_lcfs} give a simply and direct way to build a \bold{list of factorlist} through "lcfs", given a vector of factorID. Here, the settings of factorRefine will be set samely by the given arguments, and the other settings will be get through "lcfs" (\code{CT_FactorLists()}). See also \code{\link{buildFactorList_lcfs}}.
#' @export
#' @examples
#' # - through lcfs
#' factorIDs <- c("F000001","F000002","F000005")
#' FactorLists3 <- buildFactorLists_lcfs(factorIDs)
buildFactorLists_lcfs <- function(factorIDs, factorRefine = refinePar_default("none")){
  re <- lapply(factorIDs, buildFactorList_lcfs, 
               factorRefine=factorRefine)
  return(re)
}













#' default.factorName
#' 
#' @examples
#' factorFun = "gf.pct_chg_per"
#' default.factorName(factorFun,"2,3")
#' default.factorName(factorFun,"2,3",-1)
#' default.factorName(factorFun,list(N=60,M=30))
#' default.factorName(factorFun,"20,\"IF00\"")
#' default.factorName(factorFun,"lag=20,id=\"IF00\"")
default.factorName <- function (factorFun, factorPar, factorDir) {
  f.fun <- substring(factorFun,4)
  if(is.list(factorPar)){
    f.par <- paste(factorPar,collapse="_")
  } else if(is.character(factorPar)){
    factorPar <- gsub("\\w*=","",factorPar)
    f.par <- gsub("\'","",gsub("\"","",gsub(",","_",factorPar)))
  } else {
    stop("The factorPar must be a list or a character string!")
  }
  factorName <- if(f.par != "") paste(f.fun,f.par,sep="_") else f.fun
  f.dir <- if(missing(factorDir) || factorDir==1L) "" else "_"
  factorName <- paste(factorName,f.dir,sep="")
  return(factorName)
}


#' A demo function of factor-getting 
#' 
#' You can get different result by adjusting the param \code{rho}.
#' 
#' @param TS
#' @param rho a numeric. the desired correlation between Periodrtn and factorscore. See \code{\link{[QUtilily]getBiCop}}
#' @return a TSF object
#' @export
#' @author Ruifei.Yin
gf_demo <- function(TS,rho=0, dure=NULL, date_end_pad){
  check.TS(TS)
  rows <- dim(TS)[1]
  rtn <- getTSR(TS, dure = dure, date_end_pad = date_end_pad)$periodrtn
  na.len <- length(rtn[is.na(rtn)])
  na.replace <- rnorm(na.len)
  rtn[is.na(rtn)] <- na.replace
  factorscore <- getBiCop(n=rows, rho=rho, x=rtn, drop.x=TRUE)  
  TSF <- cbind(TS,data.frame(factorscore=factorscore))
  return(TSF)
}









# setmodelPar <- function(modelPar, item, item_sub, value){
#   
# }


# ============= ~~ getmodelPar  ======


#' getmodelPar
#' 
#' extract the slot of the \bold{modelPar} object.
#' 
#' there are two types of extracting methods: \code{getmodelPar} can extract any slot and any sub-slot, where as \code{getmodelPar.foo} can extract the foo solt and the sub-slots of foo only.
#' @param modelPar a \bold{modelPar} object
#' @param item character string, giving the slot name of the modelPar
#' @param item_sub character string, giving the sub-slot name of the specific slot of modelPar.If missing, then the whole slot will be extracted.
#' @return the slots or sub-slots of the \bold{modelPar} object.
#' @export
#' @author Ruifei.Yin
#' @examples
#' modelPar <- modelPar.default()
#' getmodelPar(modelPar,"title","modelID")
#' getmodelPar(modelPar,"title")
#' getmodelPar.title(modelPar,"modelID")
#' getmodelPar.title(modelPar)
getmodelPar <- function(modelPar,item,item_sub){
  if(missing(item_sub)){
    re <- modelPar[[item]]
  } else {
    re <- modelPar[[item]][[item_sub]]
  }
  return(re)
}
#' @rdname getmodelPar
#' @export
getmodelPar.title <- function(modelPar,item_sub){
  if(missing(item_sub)){
    re <- getmodelPar(modelPar,"title")
  } else {
    re <- getmodelPar(modelPar,"title",item_sub)
  }
  return(re)
}
#' @rdname getmodelPar
#' @export
getmodelPar.time <- function(modelPar,item_sub){
  if(missing(item_sub)){
    re <- getmodelPar(modelPar,"time")
  } else {
    re <- getmodelPar(modelPar,"time",item_sub)
  }
  return(re)
}
#' @rdname getmodelPar
#' @export
getmodelPar.univ <- function(modelPar,item_sub){
  if(missing(item_sub)){
    re <- getmodelPar(modelPar,"univ")
  } else {
    re <- getmodelPar(modelPar,"univ",item_sub)
  }
  return(re)
}
#' @rdname getmodelPar
#' @export
getmodelPar.factor <- function(modelPar,item_sub){
  if(missing(item_sub)){
    re <- getmodelPar(modelPar,"factor")
  } else {
    re <- getmodelPar(modelPar,"factor",item_sub)
  }
  return(re)
}






# ============= ~~ get MPs  ======

#' get modelPar-lists
#' 
#' Get ModelPar-lists by the given params. Usually used for mdoel testing with different parameters. 
#' @aliases getMPs_FactorLists
#' @rdname getMPs
#' @name getMPs
#' @param modelPar  a \bold{modelPar} object
#' @param FactorLists a list of \bold{FactorList}. See more details in \code{\link{buildFactorLists}}.
#' @param nm a character string, specificating the names of the returned list 
#' @return a \bold{MPs} object, which is a list of \bold{modelPar}
#' @seealso \code{\link{Model.TSFs}}, \code{\link{Model.TSFRs}}
#' @export
#' @examples 
#' mp = modelPar.default()
#' 
#' # -- getMPs_FactorLists
#' factorIDs <- c("F000001","F000002","F000005")
#' FactorLists <- buildFactorLists_lcfs(factorIDs)
#' re1 <- getMPs_FactorLists(FactorLists,modelPar=mp)
getMPs_FactorLists <- function(FactorLists, 
                               modelPar,
                               nm = plyr::laply(FactorLists, function(x) x$factorName)){
  MPs <- plyr::llply(FactorLists,function(x) setmodelPar.factor(modelPar,FactorList=x))
  names(MPs) <- nm
  return(MPs)
}


#' @rdname getMPs
#' @export
#' @param intervals a dataframe with cols 'begT' and 'endT'. Note that 'begT' must be the 1st col and 'endT' must be the 2nd col.
#' @examples
#' # -- getMPs_time.intervals
#' begTs <- seq(as.Date("2010-01-01"),as.Date("2015-01-01"),"year")
#' endTs <- begTs + months(1)
#' intervals <- data.frame(begTs,endTs)
#' re2 <- getMPs_time.intervals(intervals,mp)
getMPs_time.intervals <- function(intervals,
                                  modelPar,
                                  nm=paste(intervals[,1],intervals[,2],sep="~")){
  MPs <- plyr::alply(intervals, 1, function(x){setmodelPar.time(modelPar,begT=x[[1]],endT=x[[2]])})
  names(MPs) <- nm
  return(MPs)
}


#' @rdname getMPs
#' @export
#' @param rebFreqs a charactor vector of \code{rebFreq}. See \code{\link{setmodelPar.time}}
#' @examples
#' # -- getMPs_time.rebFreqs
#' rebFreqs <- c("day","month","2 month")
#' re3 <- getMPs_time.rebFreqs(rebFreqs,mp)
getMPs_time.rebFreqs <- function(rebFreqs,
                                 modelPar,
                                 nm = rebFreqs){
  MPs <- plyr::lapply(rebFreqs, function(x){setmodelPar.time(modelPar,rebFreq=x)})
  names(MPs) <- nm
  return(MPs)
}

#' @rdname getMPs
#' @export
#' @param shiftbys a integer vector of \code{shiftby}.
#' @examples
#' # -- getMPs_time.shiftbys
#' shiftbys <- c(1,3,5,10)
#' re4 <- getMPs_time.shiftbys(shiftbys,mp)
getMPs_time.shiftbys <- function(shiftbys,
                                 modelPar,
                                 nm = as.character(shiftbys)){
  MPs <- plyr::lapply(shiftbys, function(x) setmodelPar.time(modelPar, shiftby=x))
  names(MPs) <- nm
  return(MPs)
}

#' @rdname getMPs
#' @export
#' @param univs a charactor vector of \code{univ}. See \code{\link{setmodelPar.univ}}.
#' @examples
#' # -- getMPs_univs
#' univs <- c("EI000300","EI000905","EI000904")
#' re5 <- getMPs_univs(shiftbys,mp)
getMPs_univs <- function(univs,modelPar,
                         nm=univs){
  MPs <- plyr::alply(univs, .fun=function(x) setmodelPar.univ(modelPar, indexID=x))
  names(MPs) <- nm
  return(MPs)
}




# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- backtesting parametres setting -----------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============


# ============= ~~ setbacktestPar  ======

#' backtestPar.default
#' 
#' set the backtesting with default parametres
#' @return a \bold{backtestPar} object with default parametres
#' @author Ruifei.Yin
#' @export
#' @family backtestPar setting functions
backtestPar.default <- function(){
  backtestPar <- list()
  backtestPar <- backtestPar.IC(backtestPar)
  backtestPar <- backtestPar.reg(backtestPar)
  backtestPar <- backtestPar.Ngroup(backtestPar)
  backtestPar <- backtestPar.longshort(backtestPar)
  backtestPar <- backtestPar.fee(backtestPar)
  return(backtestPar)
} 

#' backtestPar.IC
#' 
#' set the IC related parametres of the backtesting 
#' @param backtestPar a \bold{backtestPar} object
#' @param stat character string,indicating the methods to compute IC,could be "pearson" or "spearman".
#' @return a \bold{backtestPar} object
#' @seealso \code{\link{seri.IC}}
#' @author Ruifei.Yin
#' @export
#' @family backtestPar setting functions
backtestPar.IC <- function(backtestPar=backtestPar.default(),
                           stat=c("pearson","spearman")){ 
  stat <- match.arg(stat)
  backtestPar$IC <-list(stat=stat)
  return(backtestPar)
}
#' @rdname backtestPar.IC
#' @export
setbacktestPar.IC <- function(backtestPar,
                              stat){ 
  if(!missing(stat)) backtestPar$IC$stat <- stat
  return(backtestPar)
}

backtestPar.reg <- function(backtestPar=list()){ 
  backtestPar$reg <-list()
  return(backtestPar)
}

#' backtestPar.Ngroup
#' 
#' set the Ngroup related parametres of the backtesting
#' @param backtestPar a \bold{backtestPar} object
#' @param N the number of the groups the universe is cut to
#' @param stat a character string,indicating the statistic of the return center of each group,could be "mean" or "median".
#' @param sectorNe
#' @param sectorAttr
#' @param turnoverType
#' @return a \bold{backtestPar} object
#' @seealso \code{\link{seri.Ngroup.rtn}} \code{\link{seri.Ngroup.turnover}}
#' @author Ruifei.Yin
#' @export
#' @family backtestPar setting functions
backtestPar.Ngroup <- function(backtestPar = backtestPar.default() ,
                               N           = 5  ,
                               stat        = c("mean","median"),
                               sectorNe    = FALSE,
                               sectorAttr  = defaultSectorAttr(),
                               turnoverType =  c("num","wgt")){ 
  stat <- match.arg(stat)
  turnoverType <- match.arg(turnoverType)
  backtestPar$Ngroup <-list(N    = N,
                            stat = stat,
                            sectorNe =sectorNe,
                            sectorAttr = sectorAttr,
                            turnoverType=turnoverType)
  return(backtestPar)
}
#' @rdname backtestPar.Ngroup
#' @export
setbacktestPar.Ngroup <- function(backtestPar ,
                                  N,
                                  stat,
                                  sectorNe,
                                  sectorAttr,
                                  turnoverType){ 
  if(!missing(N)) backtestPar$Ngroup$N <- N
  if(!missing(stat)) backtestPar$Ngroup$stat <- stat
  if(!missing(sectorNe)) backtestPar$Ngroup$sectorNe <- sectorNe
  if(!missing(sectorAttr)) backtestPar$Ngroup$sectorAttr <- sectorAttr
  if(!missing(turnoverType)) backtestPar$Ngroup$turnoverType <- turnoverType
  return(backtestPar)
}


#' backtestPar.longshort
#' 
#' set the long-short related parametres of the backtesting 
#' @param backtestPar a \bold{backtestPar} object
#' @param topN an integer,giving the numbers of the assets to be selected into the portfolio.
#' @param topQ a numeric,giving the percentage of the assets to be selected into the portfolio.
#' @param pick.sectorNe
#' @param sectorAttr
#' @param force_in
#' @param buffer_keep
#' @param init_port
#' @param holdingEndT the ending date of the holding portfolio.
#' @param wgtType a character string, giving the weighting type of portfolio,which could be "eq"(equal),"fv"(floatValue),"fvsqrt"(sqrt of floatValue) or "custom".
#' @param wgt.sectorNe a logic. If true, the wgt will be neutralized by sector.
#' @param wgt.max a integer, giving the max weight limit which could be set on a single stock. If NA(the default value), with no limit. See \code{\link{port.substitute}}.
#' @param bmk a character string,giving the stockID of the benchmark index, eg. "EI000300".
#' @param hedge.rebFreq giving the rebalance freq when computing the hedged rtn.
#' @param hedege.posi a numeric, giving the position of the hedging assets
#' @param hitFreq indicating the interval when computing the hitRatio of rtn. An interval specification, one of "day", "week", "month", "quarter" and "year", optionally preceded by an integer and a space, or followed by "s".See \code{\link{cut.Date}} for detail.
#' @return a \bold{backtestPar} object
#' @seealso \code{\link{getPort}}, \code{\link{addwgt2port}}, \code{\link{port.substitute}}, \code{\link{port.backtest}}, \code{\link{getrtn.LSH}}, \code{\link{getrtn.LBH}}, \code{\link{tables.longshort}}
#' @author Ruifei.Yin
#' @export
#' @family backtestPar setting functions
backtestPar.longshort <- function(backtestPar = backtestPar.default(),
                                  topN = 50,
                                  topQ = NA,
                                  pick.sectorNe=FALSE, 
                                  sectorAttr=defaultSectorAttr(),
                                  force_in=0, 
                                  buffer_keep=0,
                                  buffer_rate=0,
                                  init_port=NULL,
                                  holdingEndT = Sys.Date(),
                                  wgtType = "eq", 
                                  wgt.sectorNe = FALSE,
                                  wgt.max = NA,
                                  bmk="EI000300",
                                  hedge.rebFreq="month",
                                  hedge.posi=1,
                                  hitFreq="month"){ 
  backtestPar$longshort <-list(topN         = topN,
                               topQ         = topQ,
                               pick.sectorNe= pick.sectorNe,
                               sectorAttr   = sectorAttr,
                               force_in    = force_in,
                               buffer_keep  = buffer_keep,
                               buffer_rate  = buffer_rate,
                               init_port    = init_port,
                               holdingEndT  = holdingEndT,
                               wgtType      = wgtType,
                               wgt.sectorNe = wgt.sectorNe,
                               wgt.max      = wgt.max,                               
                               bmk          = bmk,
                               hedge.rebFreq= hedge.rebFreq,
                               hedge.posi   = hedge.posi,
                               hitFreq      = hitFreq)
  return(backtestPar)
}
#' @rdname backtestPar.longshort
#' @export
setbacktestPar.longshort <- function(backtestPar,
                                     topN,
                                     topQ,
                                     pick.sectorNe,
                                     sectorAttr,
                                     force_in,
                                     buffer_keep,
                                     buffer_rate,
                                     init_port,
                                     holdingEndT,
                                     wgtType,
                                     wgt.sectorNe,
                                     wgt.max ,
                                     bmk,
                                     hedge.rebFreq,
                                     hedge.posi,
                                     hitFreq){ 
  if(!missing(topN)) {
    backtestPar$longshort$topN <- topN
    # backtestPar$longshort$topQ <- NA
  }
  if(!missing(topQ)) {
    backtestPar$longshort$topQ <- topQ  
    # backtestPar$longshort$topN <- NA
  }
  if(!missing(pick.sectorNe)) backtestPar$longshort$pick.sectorNe <- pick.sectorNe  
  if(!missing(sectorAttr)) backtestPar$longshort$sectorAttr <- sectorAttr
  if(!missing(force_in)) backtestPar$longshort$force_in <- force_in
  if(!missing(buffer_keep)) backtestPar$longshort$buffer_keep <- buffer_keep
  if(!missing(buffer_rate)) backtestPar$longshort$buffer_rate <- buffer_rate
  if(!missing(init_port)) backtestPar$longshort$init_port <- init_port
  if(!missing(holdingEndT)) backtestPar$longshort$holdingEndT <- holdingEndT
  if(!missing(wgtType)) backtestPar$longshort$wgtType <- wgtType
  if(!missing(wgt.sectorNe)) backtestPar$longshort$wgt.sectorNe <- wgt.sectorNe
  if(!missing(wgt.max)) backtestPar$longshort$wgt.max <- wgt.max
  if(!missing(bmk)) backtestPar$longshort$bmk <- bmk
  if(!missing(hedge.rebFreq)) backtestPar$longshort$hedge.rebFreq <- hedge.rebFreq
  if(!missing(hedge.posi)) backtestPar$longshort$hedge.posi <- hedge.posi
  if(!missing(hitFreq)) backtestPar$longshort$hitFreq <- hitFreq
  return(backtestPar)
}

#' backtestPar.fee
#' 
#' set the fee related parametres of the backtesting 
#' @param backtestPar a \bold{backtestPar} object
#' @param secu a numeric, giving buy and sell (one side) fee of the security
#' @param future a numeric, giving  buy and sell (one side) fee of the future
#' @return a \bold{backtestPar} object
#' @seealso \code{\link{port.backtest}},\code{\link{getrtn.LSH}},\code{\link{getrtn.LBH}}
#' @author Ruifei.Yin
#' @export
#' @family backtestPar setting functions
backtestPar.fee <- function(backtestPar = backtestPar.default(),
                            secu = 0,
                            future = 0){ 
  backtestPar$fee <-list(secu = secu,
                         future = future)
  return(backtestPar)
}
#' @rdname backtestPar.fee
#' @export
setbacktestPar.fee <- function(backtestPar,
                               secu,
                               future){ 
  if(!missing(secu)) backtestPar$fee$secu <- secu
  if(!missing(future)) backtestPar$fee$future <- future
  return(backtestPar)
}


# ============= ~~ getbacktestPar  ======

#' getbacktestPar
#' 
#' extract the slot of the \bold{backtestPar} object.
#' 
#' there are two types of extracting methods: \code{getbacktestPar} can extract any slot and any sub-slot, where as \code{getbacktestPar.foo} can extract the foo solt and the sub-slots of foo only.
#' @param backtestPar a \bold{backtestPar} object
#' @param item character string, giving the slot name of the backtestPar
#' @param item_sub character string, giving the sub-slot name of the specific slot of backtestPar.If missing, then the whole slot will be extracted.
#' @return the slots or sub-slots of the \bold{backtestPar} object.
#' @export
#' @author Ruifei.Yin
#' @examples
#' backtestPar <- backtestPar.default()
#' getbacktestPar(backtestPar,"longshort","wgtType")
#' getbacktestPar(backtestPar,"longshort")
#' getbacktestPar.longshort(backtestPar,"wgtType")
#' getbacktestPar.longshort(backtestPar)
getbacktestPar <- function(backtestPar,item,item_sub){
  if(missing(item_sub)){
    re <- backtestPar[[item]]
  } else {
    re <- backtestPar[[item]][[item_sub]]
  }
  return(re)
}
#' @rdname getbacktestPar
#' @export
getbacktestPar.IC <- function(backtestPar,item_sub){
  if(missing(item_sub)){
    re <- getbacktestPar(backtestPar,"IC")
  } else {
    re <- getbacktestPar(backtestPar,"IC",item_sub)
  }
  return(re)
}
#' @rdname getbacktestPar
#' @export
getbacktestPar.reg <- function(backtestPar,item_sub){
  if(missing(item_sub)){
    re <- getbacktestPar(backtestPar,"reg")
  } else {
    re <- getbacktestPar(backtestPar,"reg",item_sub)
  }
  return(re)
}
#' @rdname getbacktestPar
#' @export
getbacktestPar.Ngroup <- function(backtestPar,item_sub){
  if(missing(item_sub)){
    re <- getbacktestPar(backtestPar,"Ngroup")
  } else {
    re <- getbacktestPar(backtestPar,"Ngroup",item_sub)
  }
  return(re)
}
#' @rdname getbacktestPar
#' @export
getbacktestPar.longshort <- function(backtestPar,item_sub){
  if(missing(item_sub)){
    re <- getbacktestPar(backtestPar,"longshort")
  } else {
    re <- getbacktestPar(backtestPar,"longshort",item_sub)
  }
  return(re)
}
#' @rdname getbacktestPar
#' @export
getbacktestPar.fee <- function(backtestPar,item_sub){
  if(missing(item_sub)){
    re <- getbacktestPar(backtestPar,"fee")
  } else {
    re <- getbacktestPar(backtestPar,"fee",item_sub)
  }
  return(re)
}

# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============
# --------------------- plotting parametres setting --------------------
# ===================== xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx ==============

# ============= ~~ setplotPar  ======

#' plotPar.default
#' 
#' set the plotting with default parametres
#' @return a \bold{plotPar} object with default parametres
#' @author Ruifei.Yin
#' @export
#' @family plotPar setting functions
plotPar.default <- function(){
  plotPar <- list()
  plotPar <- plotPar.TSFRScatter(plotPar)
  plotPar <- plotPar.IC(plotPar)
  plotPar <- plotPar.reg(plotPar)
  plotPar <- plotPar.Ngroup(plotPar)
  plotPar <- plotPar.longshort(plotPar)
  plotPar <- plotPar.MC(plotPar)
  return(plotPar)
} 



#' plotPar.TSFRScatter
#' 
#' set the TSFRScatter related parametres of the ploting 
#' @param plotPar a \bold{plotPar} object
#' @param Nbin the number of the groups the timespan is cut to when plotting the scatter by time series.It could also be character of interval specification,See \code{\link{cut.Date}} for detail.
#' @return a \bold{plotPar} object
#' @seealso \code{\link{plot.TSFRScatter}}
#' @author Ruifei.Yin
#' @export
#' @family plotPar setting functions
plotPar.TSFRScatter <- function(plotPar = plotPar.default(),
                                Nbin    = "day"){ 
  plotPar$TSFRScatter <-list(Nbin=Nbin)
  return(plotPar)
}


#' plotPar.IC
#' 
#' set the IC related parametres of the ploting 
#' @param plotPar a \bold{plotPar} object
#' @param Nbin the number of the groups the timespan is cut to, when plotting the IC series.It could also be character of interval specification,See \code{\link{cut.Date}} for detail.
#' @param stat a character string,indicating the methods to compute IC,could be "pearson" or "spearman".
#' @return a \bold{plotPar} object
#' @seealso \code{\link{chart.IC}}
#' @author Ruifei.Yin
#' @export
#' @family plotPar setting functions
plotPar.IC <- function(plotPar = plotPar.default(),
                       Nbin    = "day",
                       stat    = c("pearson","spearman")){ 
  stat <- match.arg(stat)
  plotPar$IC <-list(Nbin = Nbin,
                    stat = stat )
  return(plotPar)
}
#' @rdname plotPar.IC
#' @export
setplotPar.IC <- function(plotPar,
                          Nbin,
                          stat){ 
  if(!missing(Nbin)) plotPar$IC$Nbin <- Nbin
  if(!missing(stat)) plotPar$IC$stat <- stat
  return(plotPar)
}

#' plotPar.reg
plotPar.reg <- function(plotPar=plotPar.default()){ 
  plotPar$reg <-list()
  return(plotPar)
}

#' plotPar.Ngroup
#' 
#' set the Ngroup related parametres of the ploting 
#' @param plotPar a \bold{plotPar} object
#' @param Nbin the number of the groups the timespan is cut to, when plotting the "date.grp".It could also be character of interval specification,See \code{\link{cut.Date}} for detail.
#' @param N the number of the groups the universe is cut to
#' @param stat a character string,indicating the statistic of the return center of each group,could be "mean" or "median".
#' @param turnoverType a character string,indicating the method to calculate the turnover,could be "num" or "wgt".
#' @return a \bold{plotPar} object
#' @seealso \code{\link{chart.Ngroup.overall}},\code{\link{chart.Ngroup.seri}}
#' @author Ruifei.Yin
#' @export
#' @family plotPar setting functions
plotPar.Ngroup <- function(plotPar = plotPar.default() ,
                           Nbin    = "day"   ,
                           N       = 5 ,                           
                           stat    = c("mean","median"),
                           turnoverType = c("num","wgt")){ 
  stat <- match.arg(stat)
  turnoverType <- match.arg(turnoverType)
  plotPar$Ngroup <-list(Nbin = Nbin,
                        N    = N,
                        stat = stat,
                        turnoverType=turnoverType)
  return(plotPar)
}
#' @rdname plotPar.Ngroup
#' @export
setplotPar.Ngroup <- function(plotPar ,
                              Nbin,
                              N,                           
                              stat,
                              turnoverType){ 
  if(!missing(Nbin)) plotPar$Ngroup$Nbin <- Nbin
  if(!missing(N)) plotPar$Ngroup$N <- N
  if(!missing(stat)) plotPar$Ngroup$stat <- stat
  if(!missing(turnoverType)) plotPar$Ngroup$turnoverType <- turnoverType
  return(plotPar)
}

#' plotPar.longshort
#' 
#' set the long-short related parametres of the ploting 
#' @param plotPar a \bold{plotPar} object
#' @param bar.freq the freq of the per-period performance bar chart
#' @param roll.width the width argument for rolling performance chart
#' @param roll.by the by argument for rolling performance chart
#' @return a \bold{plotPar} object
#' @seealso \code{\link{plot.longshort}}
#' @author Ruifei.Yin
#' @export
#' @family plotPar setting functions
plotPar.longshort <- function(plotPar = plotPar.default(),
                              bar.freq   = "month",
                              roll.width = 250,
                              roll.by    = 30){ 
  plotPar$longshort <-list(bar.freq   = bar.freq,
                           roll.width = roll.width,
                           roll.by    = roll.by)
  return(plotPar)
}
#' @rdname plotPar.longshort
#' @export
setplotPar.longshort <- function(plotPar,
                                 bar.freq,
                                 roll.width,
                                 roll.by){ 
  if(!missing(bar.freq)) plotPar$longshort$bar.freq <- bar.freq
  if(!missing(roll.width)) plotPar$longshort$roll.width <- roll.width  
  if(!missing(roll.by)) plotPar$longshort$roll.by <- roll.by
  return(plotPar)
}

#' plotPar.MC
#' 
#' set the multi-comparison related parametres of the ploting 
#' @param plotPar a \bold{plotPar} object
#' @param ncol.IC 
#' @param ncol.IC.decay
#' @param ncol.Ngroup
#' @return a \bold{plotPar} object
#' @author Ruifei.Yin
#' @export
#' @family plotPar setting functions
plotPar.MC <- function(plotPar = plotPar.default(),
                       ncol.IC = 3,
                       ncol.IC.decay = 3,
                       ncol.Ngroup =3 ){
  plotPar$MC <-list(ncol.IC = ncol.IC,
                    ncol.IC.decay = ncol.IC.decay,
                    ncol.Ngroup = ncol.Ngroup)
  return(plotPar)
}
#' @rdname plotPar.MC
#' @export
setplotPar.MC <- function(plotPar ,
                          ncol.IC,
                          ncol.IC.decay,
                          ncol.Ngroup){
  if(!missing(ncol.IC)) plotPar$MC$ncol.IC <- ncol.IC
  if(!missing(ncol.IC.decay)) plotPar$MC$ncol.IC.decay <- ncol.IC.decay
  if(!missing(ncol.IC.Ngroup)) plotPar$MC$ncol.Ngroup <- ncol.Ngroup
  return(plotPar)
}


# ============= ~~ getplotPar  ======

#' getplotPar
#' 
#' extract the slot of the \bold{plotPar} object.
#' 
#' there are two types of extracting methods: \code{getplotPar} can extract any slot and any sub-slot, where as \code{getplotPar.foo} can extract the foo solt and the sub-slots of foo only.
#' @param plotPar a \bold{plotPar} object
#' @param item character string, giving the slot name of the plotPar
#' @param item_sub character string, giving the sub-slot name of the specific slot of plotPar.If missing, then the whole slot will be extracted.
#' @return the slots or sub-slots of the \bold{plotPar} object.
#' @export
#' @author Ruifei.Yin
#' @examples
#' plotPar <- plotPar.default()
#' getplotPar(plotPar,"Ngroup","Nbin")
#' getplotPar(plotPar,"Ngroup")
#' getplotPar.Ngroup(plotPar,"Nbin")
#' getplotPar.Ngroup(plotPar)
getplotPar <- function(plotPar,item,item_sub){
  if(missing(item_sub)){
    re <- plotPar[[item]]
  } else {
    re <- plotPar[[item]][[item_sub]]
  }
  return(re)
}
#' @rdname getplotPar
#' @export
getplotPar.TSFRScatter <- function(plotPar,item_sub){
  if(missing(item_sub)){
    re <- getplotPar(plotPar,"TSFRScatter")
  } else {
    re <- getplotPar(plotPar,"TSFRScatter",item_sub)
  }
  return(re)
}
#' @rdname getplotPar
#' @export
getplotPar.IC <- function(plotPar,item_sub){
  if(missing(item_sub)){
    re <- getplotPar(plotPar,"IC")
  } else {
    re <- getplotPar(plotPar,"IC",item_sub)
  }
  return(re)
}
#' @rdname getplotPar
#' @export
getplotPar.reg <- function(plotPar,item_sub){
  if(missing(item_sub)){
    re <- getplotPar(plotPar,"reg")
  } else {
    re <- getplotPar(plotPar,"reg",item_sub)
  }
  return(re)
}
#' @rdname getplotPar
#' @export
getplotPar.Ngroup <- function(plotPar,item_sub){
  if(missing(item_sub)){
    re <- getplotPar(plotPar,"Ngroup")
  } else {
    re <- getplotPar(plotPar,"Ngroup",item_sub)
  }
  return(re)
}
#' @rdname getplotPar
#' @export
getplotPar.longshort <- function(plotPar,item_sub){
  if(missing(item_sub)){
    re <- getplotPar(plotPar,"longshort")
  } else {
    re <- getplotPar(plotPar,"longshort",item_sub)
  }
  return(re)
}
#' @rdname getplotPar
#' @export
getplotPar.MC <- function(plotPar,item_sub){
  if(missing(item_sub)){
    re <- getplotPar(plotPar,"MC")
  } else {
    re <- getplotPar(plotPar,"MC",item_sub)
  }
  return(re)
}