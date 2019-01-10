
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
#' @param modelID string
#' @param modelName string
#' @param modelDesc string
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
#' @param shiftby a integer,how many days the rebalancing date be shifted foreward
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
#'                            factorRefine= setrefinePar(refinePar_default("scale"))
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


