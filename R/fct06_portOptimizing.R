
# ---  build, set, add, clear constrains  ------

#' opt_constrain
#' 
#' build, set, add, clear constrains
#' 
#' @name opt_constrain
#' @param constr
#' @param relative integer of 0,1,2
#' @return a list contain the constrains 
#' @export
#' @rdname opt_constrain
#' @examples 
#' constr <- constr_default()
constr_default <- function(position=c(1,1), box_each=c(0,1)){
  constr <- list()
  emptydf <- data.frame(ID = character(0), 
                        min = numeric(0), 
                        max = numeric(0), 
                        relative=integer(0),
                        stringsAsFactors = FALSE)
  constr$position <- emptydf
  constr$box <- emptydf
  constr$group <- emptydf
  constr$fctExp_sector <- tibble(ID = character(0), 
                                 min = numeric(0), 
                                 max = numeric(0), 
                                 relative=integer(0),
                                 sectorAttr = list())
  constr$fctExp_style <- tibble(ID = character(0), 
                                min = numeric(0), 
                                max = numeric(0), 
                                relative=integer(0),
                                factorlist = list())
  constr$turnover <- data.frame(ID = character(0), 
                                target = numeric(0),
                                method=character(0), 
                                stringsAsFactors = FALSE)
  constr$trackingerror <- data.frame(ID = character(0), 
                                target = numeric(0),
                                method=character(0), 
                                stringsAsFactors = FALSE)
  constr <- addConstr_position(constr, position = position, relative = 0)
  constr <- addConstr_box(constr, each = box_each, relative = 0)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' clearConstr(constr,"box")
clearConstr <- function(constr,item){
  constr[[item]] <- constr[[item]][0,]
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_fctExp_sector(constr, each=c(-0.05,0.05),ES33480000=c(-0.15,0.15))
addConstr_fctExp_sector <- function(constr, each, ..., relative=2,
                                    sectorAttr=defaultSectorAttr()){
  if(missing(each)){
    sector_each <- tibble()
  } else {
    sector_each <- tibble(ID="each",min=each[1],max=each[2])
  }
  cons <- list(...)
  if(length(cons)>0){
    cons <- plyr::ldply(cons)
    colnames(cons) <- c('ID','min','max')
    if(!all(cons$ID %in% CT_industryList(std = sectorAttr$std,level = sectorAttr$level)$IndustryID)){
      stop("The sectorID does no match the sectorAttr!")
    }
    cons <- rbind(sector_each,cons)
  } else {
    cons <- sector_each
  }
  cons$relative <- as.integer(relative)
  cons$sectorAttr <- list(sectorAttr)
  check_constr(cons)
  constr$fctExp_sector <- rbind(constr$fctExp_sector,cons)
  return(constr)
}
# addConstr_fctExp_style_old(constr,mkt_cap_=c(-0.01,0.01),IVR_ = c(-0.01, 1))
# addConstr_fctExp_style_old <- function(constr, ..., relative=1){
#   cons <- list(...)
#   if(length(cons)>0){
#     cons <- plyr::ldply(cons)
#     colnames(cons) <- c('ID','min','max')
#   } else {
#     cons <- data.frame(stringsAsFactors = FALSE)
#   }
#   cons$relative <- as.integer(relative)
#   check_constr(cons)
#   constr$fctExp_style <- rbind(constr$fctExp_style,cons)
#   return(constr)
# }

#' @export
#' @rdname opt_constrain
#' @param FactorLists a factorlists
#' @param min a vector with the same length of FactorLists, or a scalar
#' @param max a vector with the same length of FactorLists, or a scalar
#' @examples
#' factorlists <- buildFactorLists_lcfs(factorIDs = c("F000001","F000002"),factorStd = "sectorNe")
#' addConstr_fctExp_style(constr,factorlists,-0.1,0.1)
addConstr_fctExp_style <- function(constr,FactorLists,min,max,relative=1){
  if(length(FactorLists)>0){
    cons <- tibble(ID=sapply(FactorLists,"[[","factorName"),min=min,max=max,
                   relative=as.integer(relative),
                   factorlist=FactorLists)
    constr$fctExp_style <- rbind(constr$fctExp_style,cons)
  }
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_group(constr,EI000905=c(0, 0.8))
#' addConstr_group(constr,EI000001=c(0.5, 0.5))
#' addConstr_group(constr,ES33220000=c(-0.1, 0.1),relative = 1)
addConstr_group <- function(constr, ..., relative=0){
  cons <- list(...)
  if(length(cons)>0){
    cons <- plyr::ldply(cons)
    colnames(cons) <- c('ID','min','max')
  }else{
    cons <- data.frame(stringsAsFactors = FALSE)
  }
  cons$relative <- as.integer(relative)
  check_constr(cons)
  constr$group <- rbind(constr$group,cons)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_box(constr,each=c(0,0.02),EQ601318=c(0.05,0.15))
#' addConstr_box(constr,EQ601318=c(0.05,0.15),ES33370000=c(0,0.5))
addConstr_box <- function(constr,each, ..., relative=0){
  if(missing(each)){
    box_each <- data.frame(stringsAsFactors = FALSE)
  } else {
    box_each <- data.frame(ID="each",min=each[1],max=each[2],stringsAsFactors = FALSE)
  }
  cons <- list(...)
  if(length(cons)>0){
    cons <- plyr::ldply(cons)
    colnames(cons) <- c('ID','min','max')
    cons <- rbind(box_each,cons)
  } else {
    cons <- box_each
  }
  cons$relative <- as.integer(relative)
  check_constr(cons)
  constr$box <- rbind(constr$box,cons)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_position(constr,position=c(0.8,1))
addConstr_position <- function(constr,position,relative=0){
  if(missing(position)){
    cons <- data.frame(stringsAsFactors = FALSE)
  } else {
    cons <- data.frame(ID="position",min=position[1],max=position[2],stringsAsFactors = FALSE)
  }
  cons$relative <- as.integer(relative)
  check_constr(cons)
  constr$position <- rbind(constr$position,cons)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_turnover(constr,turnover_target=0.5)
addConstr_turnover <- function(constr,turnover_target=0.5,method=c('rmosek','matlab')){
  method <- match.arg(method)
  cons <- data.frame(ID="turnover",target=turnover_target,method=method,stringsAsFactors = FALSE)
  constr$turnover <- rbind(constr$turnover,cons)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_trackingerror(constr,trackingerror_ann=0.08)
addConstr_trackingerror <- function(constr,trackingerror_ann=0.08,method=c('matlab')){
  method <- match.arg(method)
  cons <- data.frame(ID="trackingerror",target=trackingerror_ann,method=method,stringsAsFactors = FALSE)
  constr$trackingerror <- rbind(constr$trackingerror,cons)
  return(constr)
}


#' @export
#' @rdname opt_constrain
#' @examples
setConstr_group <- function(constr, ..., relative){
  constr <- clearConstr(constr,"group")
  constr <- addConstr_group(constr = constr, ..., relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_box <- function(constr, each, ..., relative){
  constr <- clearConstr(constr,"box")
  constr <- addConstr_box(constr = constr, each = each, ..., relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_fctExp_sector <- function(constr, each, ..., relative, sectorAttr=defaultSectorAttr()){
  constr <- clearConstr(constr,"fctExp_sector")
  constr <- addConstr_fctExp_sector(constr = constr, each=each, ..., relative = relative, sectorAttr = sectorAttr)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_position <- function(constr, position, relative, sectorAttr=defaultSectorAttr()){
  constr <- clearConstr(constr,"position")
  constr <- addConstr_position(constr = constr, position = position, relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_fctExp_style <- function(constr,FactorLists,min,max,relative){
  constr <- clearConstr(constr,"fctExp_style")
  constr <- addConstr_position(constr = constr, FactorLists = FactorLists, min=min, max=max, relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_turnover <- function(constr,turnover_target,...){
  constr <- clearConstr(constr,"turnover")
  constr <- addConstr_turnover(constr,turnover_target,...)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_trackingerror <- function(constr,trackingerror_ann,...){
  constr <- clearConstr(constr,"trackingerror")
  constr <- addConstr_trackingerror(constr,trackingerror_ann,...)
  return(constr)
}


#' opt_object
#' 
#' build, set, add, clear objects
#' 
#' @name opt_object
#' @param obj
#' @return a list contain the objects 
#' @export
#' @rdname opt_object
#' @examples 
#' obj <- object_default()
#' obj <- addObj_risk(obj)
object_default <- function(){
  obj <- list()
  obj$return <- data.frame(method='mean',
                           stringsAsFactors = FALSE)
  obj$risk <- data.frame(method = character(0),
                         risk_aversion=integer(0),
                         stringsAsFactors = FALSE)
  return(obj)
}

#' @export
#' @rdname opt_object
#' @examples
#' clearObj(obj,"return")
clearObj <- function(obj,item){
  obj[[item]] <- obj[[item]][0,]
  return(obj)
}

#' @rdname opt_object
#' @export
addObj_return <- function(obj,method=c('mean','event')){
  method <- match.arg(method)
  obj$return <- data.frame(method=method,
                           stringsAsFactors = FALSE)
  return(obj)
}
#' @rdname opt_object
#' @export
addObj_risk <- function(obj,method=c('rmosek','solve.QP'),risk_aversion=1){
  method <- match.arg(method)
  obj$risk <- data.frame(method = method,
                         risk_aversion=risk_aversion,
                         stringsAsFactors = FALSE)
  return(obj)
}

# ---  get constrain result of matrixs and vectors in a single-period ------
get_constrMat_group <- function(TSF2, univFilter, cons){
  if(nrow(cons)==0){
    return(list(Amat=NULL,bvec=NULL))
  }
  for(i in 1:nrow(cons)){
    cons_ <- cons[i,]
    ID <- cons_$ID
    rela <- cons_$relative
    vec_ <- as.matrix(cons_[,c("min","max")])
    vec_mat <- calc_constr_vec(rela=rela, vec = vec_, matCols = ID, TSF2 = TSF2, univFilter = univFilter)
    if(i==1L){
      Amat <- vec_mat$mat
      bvec <- vec_mat$vec
    } else {
      Amat <- cbind(Amat,vec_mat$mat)
      bvec <- rbind(bvec,vec_mat$vec)
    }
  }
  return(list(Amat=Amat,bvec=bvec))
}

get_constrMat_fctExp_style <- function(TSF2, univFilter, cons){
  if(nrow(cons)==0){
    return(list(Amat=NULL,bvec=NULL))
  }
  for(i in 1:nrow(cons)){
    cons_ <- cons[i,]
    ID <- cons_$ID
    rela <- cons_$relative
    vec_ <- as.matrix(cons_[,c("min","max")])
    vec_mat <- calc_constr_vec(rela=rela, vec = vec_, matCols = ID, TSF2 = TSF2, univFilter = univFilter)
    if(i==1L){
      Amat <- vec_mat$mat
      bvec <- vec_mat$vec
    } else {
      Amat <- cbind(Amat,vec_mat$mat)
      bvec <- rbind(bvec,vec_mat$vec)
    }
  }
  return(list(Amat=Amat,bvec=bvec))
}

get_constrMat_position <- function(TSF2, univFilter, cons){
  if(nrow(cons)==0){
    return(list(Amat=NULL,bvec=NULL))
  }
  TSF2$position <- 1
  for(i in 1:nrow(cons)){
    cons_ <- cons[i,]
    ID <- cons_$ID
    rela <- cons_$relative
    vec_ <- as.matrix(cons_[,c("min","max")])
    vec_mat <- calc_constr_vec(rela=rela, vec = vec_, matCols = ID, TSF2 = TSF2, univFilter = univFilter)
    if(i==1L){
      Amat <- vec_mat$mat
      bvec <- vec_mat$vec
    } else {
      Amat <- cbind(Amat,vec_mat$mat)
      bvec <- rbind(bvec,vec_mat$vec)
    }
  }
  # rebind 
  Amat <- Amat[,1,drop=FALSE]
  colnames(Amat) <- "position"
  bvec <- matrix(c(max(bvec[,"min"]),min(bvec[,"max"])),nrow = 1)
  colnames(bvec) <- c("min","max")
  rownames(bvec) <- "position"
  return(list(Amat=Amat,bvec=bvec))
}

get_constrMat_fctExp_sector <- function(TSF2, univFilter, cons){
  if(nrow(cons)==0){
    return(list(Amat=NULL,bvec=NULL))
  }
  relas <- unique(cons$relative)
  for(i in 1:length(relas)){
    rela <- relas[i]
    cons_ <- cons[cons$relative==rela,]
    specified_sec <- cons_$ID[cons_$ID != "each"]
    cons_spec <- cons_[cons_$ID != "each",]
    cons_each <- cons_[cons_$ID == "each",]
    cons_re <- cons_spec[,c("ID","min","max")]
    if(dim(cons_each)[1]>0){
      for(j in 1:dim(cons_each)[1]){
        sectorAttr_j <- cons_each[[j,"sectorAttr"]]
        ID_each <- setdiff(CT_industryList(std=sectorAttr_j$std,level=sectorAttr_j$level)$IndustryID, specified_sec)
        ID_each <- intersect(ID_each,names(TSF2)) # sometimes not all the sectors is in the TSF2 univ.
        cons_re_each <- data.frame(ID=ID_each,min=cons_each[[j,"min"]],max=cons_each[[j,"max"]],stringsAsFactors = FALSE)
        cons_re <- rbind(cons_re,cons_re_each)
      }
    }
    vec_ <- as.matrix(cons_re[,c("min","max")])
    vec_mat <- calc_constr_vec(rela=rela, vec = vec_, matCols = cons_re$ID, TSF2 = TSF2, univFilter = univFilter)
    if(i==1L){
      Amat <- vec_mat$mat
      bvec <- vec_mat$vec
    } else {
      Amat <- cbind(Amat,vec_mat$mat)
      bvec <- rbind(bvec,vec_mat$vec)
    }
  }
  return(list(Amat=Amat,bvec=bvec))
}


# the high to low precedence is: spec_EQ, epec_sec and nonespec.
get_constrMat_box <- function(TSF2, univFilter, cons){
  if(nrow(cons)==0){
    return(list(Amat=NULL,bvec=NULL))
  }
  univ <- TSF2[univFilter,"stockID"]
  relas <- unique(cons$relative)
  for(i in 1:length(relas)){
    rela <- relas[i]
    cons_ <- cons[cons$relative==rela,]
    # 1.spec_EQ
    specified_EQ <- cons_$ID[substr(cons_$ID,1,2) %in% c("EQ")]
    cons_spec_EQ <- cons_[cons_$ID %in% specified_EQ,c("ID","min","max")]
    # 2.spec_sector
    specified_sec <- cons_$ID[substr(cons_$ID,1,2) %in% c("ES","EI")]
    if(length(specified_sec>0)){
      for(j in 1:length(specified_sec)){
        secID_ <- specified_sec[j]
        stockIDs_ <- TSF2[TSF2[[secID_]] & univFilter, "stockID"]
        stockIDs_ <- setdiff(stockIDs_,specified_EQ)
        cons_spec_sec_ <- data.frame(ID=stockIDs_,min=cons_[cons_$ID == secID_,"min"],max=cons_[cons_$ID == secID_,"max"],stringsAsFactors = FALSE)
        if(j==1L){
          cons_spec_sec <- cons_spec_sec_
        } else {
          cons_spec_sec <- rbind(cons_spec_sec,cons_spec_sec_)
        }
      }
    } else {
      cons_spec_sec <- data.frame(stringsAsFactors = FALSE)
    }
    # 3.nonespec stocks
    if("each" %in% cons_$ID){
      each_IDs <- setdiff(setdiff(univ,specified_EQ),unique(cons_spec_sec$ID))
      if(length(each_IDs)>0){
        cons_nonespec <- data.frame(ID=each_IDs,
                                    min=max(cons_[cons_$ID == "each","min"]),
                                    max=min(cons_[cons_$ID == "each","max"]),
                                    stringsAsFactors = FALSE)
      } else {
        cons_nonespec <- data.frame(stringsAsFactors = FALSE)
      }
    } else {
      cons_nonespec <- data.frame(stringsAsFactors = FALSE)
    }
    # 4.all
    cons_all <- rbind(cons_spec_EQ,cons_spec_sec,cons_nonespec)
    # 5.calc_constr_vec
    if(rela==1L){
      cons_all <- cons_all
    } else if(rela==2L){
      cons_all <- dplyr::left_join(cons_all,TSF2[,c("stockID","wgt_bmk")],by=c("ID"="stockID"))
      cons_all <- dplyr::mutate(cons_all,min=min*wgt_bmk,max=max*wgt_bmk)
    } else {
      cons_all <- dplyr::left_join(cons_all,TSF2[,c("stockID","wgt_bmk")],by=c("ID"="stockID"))
      cons_all <- dplyr::mutate(cons_all,min=min-wgt_bmk,max=max-wgt_bmk)
    }
    # 6.bind
    if(i==1L){
      cons_re <- cons_all[,c("ID","min","max")]
    } else {
      cons_re <- rbind(cons_re,cons_all[,c("ID","min","max")])
    }
  }
  # regroup by stockIDs
  cons_re <- data.table(cons_re,key = "ID")[,list(min=max(min),max=min(max)),by="ID"]
  cons_re <- dplyr::left_join(data.frame(ID=univ,stringsAsFactors = FALSE),cons_re,by="ID")
  cons_re[is.na(cons_re$min),"min"] <- -Inf
  cons_re[is.na(cons_re$max),"max"] <- Inf
  # result
  bvec <- as.matrix(cons_re[,c("min","max")])
  rownames(bvec) <- cons_re$ID
  Amat <- diag(1,nrow = length(univ))
  return(list(Amat=Amat,bvec=bvec))
}



# ---  inner utilitys  ------
check_constr <- function(cons){
  if(any(cons$max < cons$min)){
    stop('max less than min in the constrain!')
  }
}


calc_constr_vec <- function(rela, vec, matCols,TSF2, univFilter) {
  vec_cols <- ncol(vec)  # usually equals 2.
  wgt_bmk <- TSF2$wgt_bmk
  mat2 <- as.matrix(TSF2[,matCols,drop=FALSE])
  vec_bmk <- matrix(rep(wgt_bmk %*% mat2, vec_cols),ncol=vec_cols)
  
  # minus the 'vec-bmk' from constrain vec if rela not equals 1.
  if(rela==1L){
    vec <- vec
  } else if(rela==2L){
    vec <- vec * vec_bmk
  } else if(rela==0L){
    vec <- vec - vec_bmk
  } else {
    stop("uncorrect rela argument!")
  }
  
  # minus the 'vec-not-in-univ' from constrain vec
  vec_notinuniv <- matrix(rep(-wgt_bmk[!univFilter] %*% mat2[!univFilter,], vec_cols),ncol=vec_cols)
  vec <- vec-vec_notinuniv
  
  mat <- mat2[univFilter,,drop=FALSE]
  colnames(mat) <- paste(matCols,rela, sep = "_")
  rownames(vec) <- paste(matCols,rela, sep = "_")
  return(list(mat=mat,vec=vec))
}


get_bmk_wgt <- function(TS,bmk=NULL,byTS=TRUE,rmbmkSus=FALSE){
  if(is.null(bmk)){
    TS$wgt_bmk <- 0
  } else {
    benchdata <- getIndexCompWgt(indexID = bmk,endT = unique(TS$date))
    if(rmbmkSus){
      benchdata <- is_suspend(benchdata,nearby = 1)
      benchdata <- benchdata[benchdata$sus==FALSE,]
      benchdata <- dplyr::select(benchdata,-sus)
    }
    
    #deal with total weight not equal to 1
    benchdata <- port_wgt_roundto(benchdata)
    colnames(benchdata) <- c('date','stockID','wgt_bmk')
    if(byTS){
      TS <- merge.x(TS,benchdata,by=c('date','stockID'))
      TS[is.na(TS$wgt_bmk),'wgt_bmk'] <- 0
    }else{
      TS <- dplyr::full_join(TS,benchdata,by=c('date','stockID'))
      TS[is.na(TS$wgt_bmk),'wgt_bmk'] <- 0
    }
  }
  return(TS)
}

get_exp_rtn <- function(TSF){
  
}

port_wgt_roundto <- function(port,target=1,digits=5){
  add_date_tag <- FALSE
  if(!('date' %in% colnames(port))){
    port <- transform(port,date=Sys.Date())
    add_date_tag <- TRUE
  }
  
  newport <- dplyr::arrange(port,date,desc(wgt))
  newport <- newport %>% dplyr::group_by(date) %>% dplyr::mutate(wgt=wgt/sum(wgt)*target,id=seq(1,length(date))) %>% dplyr::ungroup()
  newport <- transform(newport,wgt=round(wgt,digits))
  errdata <- newport %>% dplyr::group_by(date) %>% dplyr::summarise(exwgt=round(sum(wgt)-target,digits)) %>% dplyr::ungroup()
  newport <- dplyr::left_join(newport,errdata,by='date')
  newport[,'wgt'] <- ifelse(newport$id==1,newport$wgt-newport$exwgt,newport$wgt)
  port <- dplyr::left_join(port[,c('date','stockID')],newport[,c('date','stockID','wgt')],by=c('date','stockID'))
  
  if(add_date_tag){
    port <- port[,c('stockID','wgt')]
  }
  return(port)
}



# ---  port optimizing  ------
#' getPort_opt
#' 
#' @param TSF
#' @param fRtn
#' @param fCov
#' @param exp_rtn
#' @param bmk
#' @param constr
#' @param obj
#' @param init_port 
#' @param delta 
#' @return a port
#' @export
#' @examples 
#' TS <- getTS(RebDates = as.Date("2017-03-31"),indexID = "EI000300")
#' TSF <- getTSF(TS,FactorList = buildFactorList_lcfs("F000006",factorRefine = refinePar_default("robust")))
#' TSF <- renameCol(TSF,"factorscore","combfct")
#' # constrain setting
#' constr <- constr_default(box_each = c(0,0.02))
#' constr <- addConstr_box(constr,each = c(0,0.02))
#' constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#' conslist <- buildFactorLists_lcfs("F000002",factorRefine = refinePar_default("robust"))
#' # with bmk
#' constr <- addConstr_fctExp_style(constr,conslist,-0.1,0.1)
#' port_opt <- getPort_opt(TSF,bmk = "EI399330",constr = constr,exp_rtn = 'combfct')
#' # without bmk
#' constr2 <- setConstr_fctExp_sector(constr,each = c(0,0.1),relative = 0)
#' port_opt2 <- getPort_opt(TSF,bmk=NULL,constr = constr2,exp_rtn = 'combfct')
#' # long-short port
#' constr3 <- setConstr_position(constr_default(),position = c(0,0),relative = 0)
#' constr3 <- setConstr_box(constr3,each=c(-0.05,0.05),relative=0)
#' port_opt3 <- getPort_opt(TSF,bmk="EI000300",constr = constr3,exp_rtn = 'combfct')
getPort_opt <- function(TSF,
                        fRtn,
                        fCov,
                        exp_rtn="exp_rtn",
                        bmk=NULL,
                        constr=constr_default(),
                        obj=object_default(),
                        init_port,
                        delta){
  
  fnames <- guess_factorNames(TSF)
  
  ### constrain data preparing
  if(dim(constr$turnover)[1]>0){
    TSF2 <- constr_data_prepare(TSF,bmk,constr,rmbmkSus = TRUE)
    
    if(missing(init_port)){
      init_port <- data.frame()
    }else{
      init_port <- init_port[,c('stockID','wgt')]
    } 
    init_port_sus <- data.frame()
    turnover_target <- constr$turnover[,'target']
  }else{
    TSF2 <- constr_data_prepare(TSF,bmk,constr)
  }
  
  # open matlab api
  openmatlab <- dim(constr$trackingerror)[1]>0 && constr$trackingerror[,'method']=='matlab'
  if(openmatlab==FALSE){
    openmatlab <- dim(constr$turnover)[1]>0 && constr$turnover[,'method']=='matlab'
  }
  if(openmatlab){
    require(R.matlab)
    R.matlab::Matlab$startServer()
    matlab <- R.matlab::Matlab()
    open(matlab)
  }
  
  # looping.....
  dates <- unique(TSF$date)
  port <- data.frame()
  for(i in 1:length(dates)){
    cat(rdate2int(dates[i]), "...\n")
    TSF_ <- TSF[TSF$date==dates[i],]
    TSF2_ <- TSF2[TSF2$date==dates[i],]
    
    if(dim(constr$turnover)[1]>0 && nrow(init_port)>0){
      #stock in initial portfolio suspend 
      init_port_sus <- data.frame(date=dates[i],init_port)
      init_port_sus <- is_suspend(init_port_sus,nearby = 1)
      init_port <- init_port_sus %>% dplyr::filter(sus==FALSE) %>% dplyr::select(-sus)
      init_port_sus <- init_port_sus %>% dplyr::filter(sus==TRUE) %>% dplyr::select(-sus)
      if(nrow(init_port_sus)>0){
        init_port <- port_wgt_roundto(init_port)
      }
      
      #stock in initial portfolio must sell
      init_port_sell <- init_port[!(init_port$stockID %in% TSF_$stockID),]
      init_port <- init_port[init_port$stockID %in% TSF_$stockID,]
      if(nrow(init_port_sell)>0){
        turnover_target <- turnover_target-sum(init_port_sell$wgt)
      }
      if(nrow(init_port_sus)>0){
        turnover_target <- turnover_target*1/(1-sum(init_port_sus$wgt))
      }
      
      #remove suspended stock
      TSF_ <- rm_suspend(TSF_)
      univFilter <- TSF2_$stockID %in% TSF_$stockID
      univ <- TSF2_[univFilter,"stockID"]
      
      init_wgt <- dplyr::left_join(TSF2_[,c('stockID','wgt_bmk')],init_port[,c('stockID','wgt')],by='stockID')
      init_wgt[is.na(init_wgt$wgt),'wgt'] <- 0
      init_wgt$wgt <- init_wgt$wgt-init_wgt$wgt_bmk
      init_wgt <- matrix(init_wgt[univFilter,'wgt'],ncol = 1)
      
    }else{
      #remove unqualified TS
      TSF_ <- rm_suspend(TSF_)
      univFilter <- TSF2_$stockID %in% TSF_$stockID
      univ <- TSF2_[univFilter,"stockID"]
    }
    
    
    # get 'dvec'
    if(dim(obj$return)[1]==0){
      dvec <- rep(0,length(univ)) #objects don't have return part
    }else{
      if(exp_rtn %in% colnames(TSF)){
        dvec <- TSF2_[univFilter,exp_rtn]
      } else {
        if('date' %in% colnames(fRtn)){
          fRtn_ <- fRtn %>% dplyr::filter(date==dates[i]) %>% dplyr::select(-date)
        }else{
          fRtn_ <- fRtn
        }
        rownames(fRtn_) <- fRtn_$fname
        alphamat <- as.matrix(TSF2_[univFilter,fnames,drop=FALSE])
        dvec <- as.vector(alphamat %*% as.matrix(fRtn_[fnames,'frtn']))
      }
    }
    
    #get 'Dmat'
    if(!missing(fCov)){
      if('date' %in% colnames(fCov)){
        fCov_ <- fCov %>% dplyr::filter(date==dates[i]) %>% dplyr::select(-date)
      }else{
        fCov_ <- fCov
      }
      rownames(fCov_) <- colnames(fCov_)
      Dmat <- alphamat %*% as.matrix(fCov_[fnames,fnames]) %*% t(alphamat)
      
      #turn Dmat to positive definite matrix 
      Dmat.eig <- eigen(Dmat)
      if(any(Dmat.eig$values<0)){
        Dmat.eig$values[Dmat.eig$values<1e-6] <- 1e-6
        Dmat <- Dmat.eig$vectors %*% diag(Dmat.eig$values) %*% t(Dmat.eig$vectors)
      }
    }
    
    
    # get 'Amat' & 'bvec'
    mat_group <- get_constrMat_group(TSF2_, univFilter, cons = constr$group)
    mat_box <- get_constrMat_box(TSF2_, univFilter, cons = constr$box)
    mat_position <- get_constrMat_position(TSF2_, univFilter, cons = constr$position)
    mat_fctExp_sector <- get_constrMat_fctExp_sector(TSF2_, univFilter, cons = constr$fctExp_sector)
    mat_fctExp_style <- get_constrMat_fctExp_style(TSF2_, univFilter, cons = constr$fctExp_style)
    
    mat_vec_list <- list(mat_group=mat_group,
                           mat_box=mat_box,
                           mat_position=mat_position,
                           mat_fctExp_sector=mat_fctExp_sector,
                           mat_fctExp_style=mat_fctExp_style)
    
    ##check Amat and bvec
    jumptag <- mat_constr_check(mat_vec_list)
    if(jumptag){
      next
    }
    
    # control tracking error
    if(dim(constr$trackingerror)[1]>0){
      if('date' %in% colnames(delta)){
        delta_ <- delta[delta$date==dates[i],-1]
      }else{
        delta_ <- delta
      }
      delta_mat <- data.frame(stockID=univ,stringsAsFactors = FALSE)
      delta_mat <- dplyr::left_join(delta_mat,delta_,by='stockID')
      delta_mat <- delta_mat %>% dplyr::mutate(var= ifelse(is.na(var), median(var, na.rm=TRUE), var))
      delta_mat <- diag(delta_mat$var)
      Dmat <- Dmat+delta_mat
      
      if(dim(constr$turnover)[1]>0 && nrow(init_port)>0){
        wgt_ <- try(solver_trackingerror_turnover(dvec,Dmat,mat_vec_list,constr,matlab,init_wgt,turnover_target),silent = TRUE)
        turnover_target <- constr$turnover[,'target']
      }else{
        wgt_ <- try(solver_trackingerror_simple(dvec,Dmat,mat_vec_list,constr,matlab=matlab),silent = TRUE)
      }
      
      
    }else if(dim(constr$turnover)[1]>0 && nrow(init_port)>0){
      # control turnover
      if(dim(obj$risk)[1]==0){
        Dmat <- NULL
      }
      if(openmatlab){
        wgt_ <- try(solver_turnover_simple(dvec,Dmat,mat_vec_list,obj,constr,init_wgt,turnover_target,matlab=matlab),silent = TRUE)
      }else{
        wgt_ <- try(solver_turnover_simple(dvec,Dmat,mat_vec_list,obj,constr,init_wgt,turnover_target),silent = TRUE)
      }
      turnover_target <- constr$turnover[,'target']

    }else if(dim(obj$risk)[1]>0){
      #solve quadprog optimization
      wgt_ <- try(solver_QP_balance(dvec,Dmat,mat_vec_list,obj),silent = TRUE)
      
    }else{
      #solve linear optimization
      dvec <- xts::xts(matrix(dvec,nrow=1),order.by = dates[i])
      wgt_ <- try(solver_lin_maxrtn(univ,dvec,mat_vec_list),silent = TRUE)
      
    }
    
    if(!inherits(wgt_, "try-error")){
      port_ <- data.frame(date=dates[i],stockID=univ,wgt=wgt_, stringsAsFactors = FALSE)
      port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
    }else next
    
    port_ <- port_[abs(port_$wgt)>0.001,]
    if(constr$position[,'min']==1 && constr$position[,'max']==1){
      port_ <- port_wgt_roundto(port_)
    }
    
    if(dim(constr$turnover)[1]>0){
      if(nrow(init_port_sus)>0){
        tmpwgt <- sum(init_port_sus$wgt)
        port_ <- port_wgt_roundto(port_,target = 1-tmpwgt)
        port_ <- rbind(port_,init_port_sus)
      }
      init_port <- port_[,c('stockID','wgt')]
    }
    port <- rbind(port,port_)
    
  }# for dates end
  
  if(openmatlab){
    close(matlab)
  }
  
  port <- transform(port,stockID=as.character(stockID))
  return(port)
}


#self defined optimizer
solver_lin_maxrtn <- function(univ,dvec,mat_vec_list){
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  require(PortfolioAnalytics)
  pspec <- portfolio.spec(assets=univ)
  pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=mat_position$bvec[,"min"], max_sum=mat_position$bvec[,"max"])
  pspec <- add.constraint(portfolio=pspec,type="box",min=mat_box$bvec[,"min"],max=mat_box$bvec[,"max"])
  Amat <- cbind(mat_group$Amat,mat_fctExp_sector$Amat,mat_fctExp_style$Amat)
  bvec <- rbind(mat_group$bvec,mat_fctExp_sector$bvec,mat_fctExp_style$bvec)
  pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                          B=Amat,lower=bvec[,"min"],upper=bvec[,"max"])
  pspec <- add.objective(portfolio=pspec,type='return',name='mean')
  res <- optimize.portfolio(R=dvec, portfolio=pspec,optimize_method="ROI",trace=TRUE)
  return(res$weights)
}

solver_QP_balance <- function(dvec,Dmat,mat_vec_list,obj){
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  stocknum <- dim(Dmat)[1]
  if(stocknum>1000) obj$risk[,'method'] <- "rmosek"
  
  if(obj$risk[,'method'] == "solve.QP"){
    Amat_bvec <- mat_vec_bind(dir='ge',mat_position,mat_box,mat_group,mat_fctExp_sector,mat_fctExp_style)
    Amat <- cbind(Amat_bvec$amat_eq,Amat_bvec$amat)
    bvec <- c(Amat_bvec$bvec_eq,Amat_bvec$bvec)
    meqnum <- dim(Amat_bvec$bvec_eq)[1]
    if(!is.null(meqnum)){
      res <- quadprog::solve.QP(Dmat * obj$risk[,'risk_aversion'],dvec,Amat,bvec,meq = meqnum)
    }else{
      res <- quadprog::solve.QP(Dmat * obj$risk[,'risk_aversion'],dvec,Amat,bvec)
    }
    
    return(res$solution)
    
  }else if(obj$risk[,'method'] == "rmosek"){
    F.mosek <-  Matrix::chol(Dmat * obj$risk[,'risk_aversion'])
    f.mosek <- -dvec
    
    Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
    A.mosek <- t(Amat_bvec$amat)
    b.mosek <- matrix(c(Amat_bvec$bvec),ncol=1)
    
    lb.mosek <- mat_box$bvec[,'min']
    ub.mosek <- mat_box$bvec[,'max']
    
    if(is.null(Amat_bvec$amat_eq)){
      prob <- Rmosek::mosek_qptoprob(F=F.mosek , f=f.mosek, A=A.mosek, b=b.mosek, lb=lb.mosek, ub=ub.mosek)
    }else{
      Aeq.mosek <- t(Amat_bvec$amat_eq)
      beq.mosek <- matrix(c(Amat_bvec$bvec_eq),ncol=1)
      prob <- Rmosek::mosek_qptoprob(F=F.mosek , f=f.mosek, A=A.mosek, b=b.mosek,Aeq=Aeq.mosek,beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
    }
    res <- Rmosek::mosek(prob)
    return(res$sol$itr$xx[1:stocknum])
  }
}


solver_turnover_simple <- function(dvec,Dmat,mat_vec_list,obj,constr,init_wgt,turnover_target,...){
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  stocknum <- length(dvec)
  if(constr$turnover[,'method']=='rmosek'){
    
    Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
    A1 <- t(Amat_bvec$amat)
    b1 <- matrix(c(Amat_bvec$bvec),ncol=1)
    
    A.mosek <- rbind(cbind(A1,-1*A1),1)
    b.mosek <- c(b1-A1%*%init_wgt,turnover_target)
    
    if(!is.null(Amat_bvec$amat_eq)){
      A1 <- t(Amat_bvec$amat_eq)
      b1 <- matrix(c(Amat_bvec$bvec_eq),ncol=1)
      
      Aeq.mosek <- cbind(A1,-1*A1)
      beq.mosek <- c(b1-A1%*%init_wgt)
    }
    
    C1 <- rbind(cbind(diag(1,stocknum),diag(-1,stocknum)),
                cbind(diag(-1,stocknum),diag(1,stocknum)))
    d1 <- c(mat_box$bvec[,'max']-init_wgt,
            -(mat_box$bvec[,'min']-init_wgt))
    A.mosek <- rbind(A.mosek,C1)
    b.mosek <- c(b.mosek,d1)
    lb.mosek <- rep(0,2*stocknum)
    ub.mosek <- rep(1,2*stocknum)
    if(is.null(Dmat)){
      f.mosek <- c(-dvec,dvec)
      if(is.null(Amat_bvec$amat_eq)){
        prob <- Rmosek::mosek_lptoprob(f=f.mosek, A=A.mosek, b=b.mosek, lb=lb.mosek, ub=ub.mosek)
      }else{
        prob <- Rmosek::mosek_lptoprob(f=f.mosek, A=A.mosek, b=b.mosek, Aeq=Aeq.mosek, beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
      }
    }else{
      COV <- rbind(cbind(Dmat,-1*Dmat),
                   cbind(-1*Dmat,Dmat))
      
      #turn cov to positive definite matrix
      COV.eig <- eigen(COV)
      if(any(COV.eig$values<0)){
        COV.eig$values[COV.eig$values<1e-6] <- 1e-6
        COV <- COV.eig$vectors %*% diag(COV.eig$values) %*% t(COV.eig$vectors)
      }
      
      F.mosek <-  Matrix::chol(COV)
      f.mosek <- c(-dvec+2* obj$risk[,'risk_aversion']*Dmat %*% init_wgt,
                   dvec-2* obj$risk[,'risk_aversion']*Dmat %*% init_wgt)
      if(is.null(Amat_bvec$amat_eq)){
        prob <- Rmosek::mosek_qptoprob(F =F.mosek , f=f.mosek, A=A.mosek, b=b.mosek, lb=lb.mosek, ub=ub.mosek)
      }else{
        prob <- Rmosek::mosek_qptoprob(F =F.mosek , f=f.mosek, A=A.mosek, b=b.mosek, Aeq=Aeq.mosek, beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
      }
    }
    res <- Rmosek::mosek(prob)
    return(res$sol$itr$xx[1:stocknum]-res$sol$itr$xx[(stocknum+1):(2*stocknum)]+init_wgt)

  }else if(constr$turnover[,'method']=='matlab'){
    matlab <- list(...)[[1]]
    Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
    R.matlab::setVariable(matlab, 
                          MU=matrix(dvec, ncol = 1),
                          A_leq=t(Amat_bvec$amat),
                          b_leq=Amat_bvec$bvec,
                          LB = matrix(data = mat_box$bvec[,'min'], ncol = 1), 
                          UB = matrix(data = mat_box$bvec[,'max'], ncol = 1),
                          W0 = init_wgt,
                          delta=turnover_target)
    evalformula <- "N=length(MU);cvx_begin ;  variable w(N) ;"
    if(!is.null(Dmat)){
      R.matlab::setVariable(matlab, COV=Dmat * obj$risk[,'risk_aversion'])
      evalformula <- paste(evalformula,"minimize(-MU'*w+w'*COV*w) ;")
    }else{
      evalformula <- paste(evalformula,"minimize(-MU'*w)  ;")
    }
    
    evalformula <- paste(evalformula,"subject to ;   A_leq*w<=b_leq ;     LB<=w<=UB ;    norm(w-W0,1)<=delta ;")
    
    if(!is.null(Amat_bvec$amat_eq)){
      R.matlab::setVariable(matlab, 
                            A_eq=t(Amat_bvec$amat_eq),
                            b_eq=Amat_bvec$bvec_eq)
      evalformula <- paste(evalformula,"A_eq*w==b_eq ;")

    }
    evalformula <- paste(evalformula,"cvx_end ;")
    R.matlab::evaluate(matlab, evalformula) 
    res <- R.matlab::getVariable(matlab, "w")
    
    return(res$w)
  }
}

solver_trackingerror_simple <- function(dvec,Dmat,mat_vec_list,constr,...){
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  dotlits <- list(...)
  
  if(constr$trackingerror[,'method']=='matlab'){
    matlab <- dotlits[['matlab']]
    eigdmt <- eigen(Dmat)
    Mmat <- diag(sqrt(eigdmt$values)) %*% t(eigdmt$vectors)
    
    Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
    R.matlab::setVariable(matlab, 
                          MU=matrix(dvec, ncol = 1),
                          A_leq=t(Amat_bvec$amat),
                          b_leq=Amat_bvec$bvec,
                          LB = matrix(data = mat_box$bvec[,'min'], ncol = 1), 
                          UB = matrix(data = mat_box$bvec[,'max'], ncol = 1),
                          TransMatrix=Mmat,
                          TargetTE=constr$trackingerror[,'target'])
    
    evalformula <- "N=length(MU) ;cvx_begin ;variable w(N) ;minimize(-MU'*w) ;subject to ;A_leq*w<=b_leq ;LB<=w<=UB ;                                            
                       norm(TransMatrix*w)<=TargetTE/sqrt(12) ;"                            
    
    if(!is.null(Amat_bvec$amat_eq)){
      R.matlab::setVariable(matlab, 
                            A_eq=t(Amat_bvec$amat_eq),
                            b_eq=Amat_bvec$bvec_eq)
      evalformula <- paste(evalformula,"  A_eq*w==b_eq ;")
    }
    
    evalformula <- paste(evalformula,"  cvx_end ;")
    R.matlab::evaluate(matlab, evalformula) 
    
    res <- R.matlab::getVariable(matlab, "w")
    return(res$w) 
    
  }else{
    
  }
}

solver_trackingerror_turnover <- function(dvec,Dmat,mat_vec_list,constr,matlab,init_wgt,turnover_target){
  
  eigdmt <- eigen(Dmat)
  Mmat <- diag(sqrt(eigdmt$values)) %*% t(eigdmt$vectors)
  
  Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
  R.matlab::setVariable(matlab, 
                        MU=matrix(dvec, ncol = 1),
                        A_leq=t(Amat_bvec$amat),
                        b_leq=Amat_bvec$bvec,
                        LB = matrix(data = mat_box$bvec[,'min'], ncol = 1), 
                        UB = matrix(data = mat_box$bvec[,'max'], ncol = 1),
                        TransMatrix=Mmat,
                        TargetTE=constr$trackingerror[,'target'])
  
  evalformula <- "N=length(MU) ;cvx_begin ;variable w(N) ;minimize(-MU'*w) ;subject to ;A_leq*w<=b_leq ;LB<=w<=UB ;                                            
  norm(TransMatrix*w)<=TargetTE/sqrt(12) ;"                            
  
  if(!is.null(Amat_bvec$amat_eq)){
    R.matlab::setVariable(matlab, 
                          A_eq=t(Amat_bvec$amat_eq),
                          b_eq=Amat_bvec$bvec_eq)
    evalformula <- paste(evalformula,"  A_eq*w==b_eq ;")
  }
  
  #add turnover constrain
  R.matlab::setVariable(matlab,W0 = init_wgt,
                        delta=turnover_target)
  evalformula <- paste(evalformula," norm(w-W0,1)<=delta ;")
  
  evalformula <- paste(evalformula,"  cvx_end ;")
  R.matlab::evaluate(matlab, evalformula) 
  
  res <- R.matlab::getVariable(matlab, "w")
  return(res$w) 

}


mat_vec_bind <- function(dir=c('ge','le','gl'),...){
  dir <- match.arg(dir)
  
  rawdata <- list(...)
  for(i in 1:length(rawdata)){
    if(i==1){
      amat <- rawdata[[i]]$Amat
      bvec <- rawdata[[i]]$bvec
    }else{
      amat <- cbind(amat,rawdata[[i]]$Amat)
      bvec <- rbind(bvec,rawdata[[i]]$bvec)
    }
    
  }
  
  eqindex <- unname(which(bvec[,'min']==bvec[,'max']))
  amat_eq <- NULL
  bvec_eq <- NULL
  if(length(eqindex)>0){
    amat_eq <- amat[,eqindex,drop=FALSE]
    bvec_eq <- bvec[eqindex,'min',drop=FALSE]
    
    amat <- amat[,-eqindex,drop=FALSE]
    bvec <- bvec[-eqindex,,drop=FALSE]
  }
  
  
  if(dir=='ge'){
    amat <- cbind(amat,amat*-1)
    bvec <- matrix(c(bvec[,'min'],bvec[,'max']*-1),ncol = 1)
  }else if(dir=='le'){
    amat <- cbind(amat*-1,amat)
    bvec <- matrix(c(bvec[,'min']*-1,bvec[,'max']),ncol = 1)
  }
  return(list(amat=amat,bvec=bvec,amat_eq=amat_eq,bvec_eq=bvec_eq))
  
}



mat_constr_check <- function(mat_vec_list){
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  
  conflicttag <- 0
  warnmessage <- ''
  #check position constrain
  if(!is.null(mat_box$bvec) && !is.null(mat_position$Amat)){
    pos_range <- t(mat_position$Amat) %*% mat_box$bvec
    if(max(pos_range[,'min'],mat_position$bvec[,'min'])>min(pos_range[,'max'],mat_position$bvec[,'max'])){
      warnmessage <- paste(warnmessage,'position constrain unqualified!\n')
      conflicttag <- conflicttag+1
    }
  }
  
  
  #check group constrain
  if(!is.null(mat_box$bvec) && !is.null(mat_group$Amat)){
    group_range <- t(mat_group$Amat) %*% mat_box$bvec
    for(j in 1:nrow(group_range)){
      if(max(group_range[j,'min'],mat_group$bvec[j,'min'])>min(group_range[j,'max'],mat_group$bvec[j,'max'])){
        warnmessage <- paste(warnmessage,'group:',rownames(group_range)[j],'constrain unqualified!\n')
        conflicttag <- conflicttag+1
      }
    }
  }
    
  #check fctExp_sector constrain
  if(!is.null(mat_box$bvec) && !is.null(mat_fctExp_sector$Amat)){
    sector_range <- t(mat_fctExp_sector$Amat) %*% mat_box$bvec
    for(j in 1:nrow(sector_range)){
      if(max(sector_range[j,'min'],mat_fctExp_sector$bvec[j,'min'])>min(sector_range[j,'max'],mat_fctExp_sector$bvec[j,'max'])){
        warnmessage <- paste(warnmessage,'sector:',rownames(sector_range)[j],'constrain unqualified!\n')
        conflicttag <- conflicttag+1
      }
    }
  }
  
  #check whether position constr and sector constr conflict
  if(!is.null(mat_position$Amat) && !is.null(mat_fctExp_sector$Amat)){
    sector_range <- matrix(colSums(mat_fctExp_sector$bvec),ncol = 2)
    colnames(sector_range) <- c('min','max')
    if(max(sector_range[,'min'],mat_position$bvec[,'min'])>min(sector_range[,'max'],mat_position$bvec[,'max'])+1e-3){
      warnmessage <- paste(warnmessage,'position constrain and sector constrain conflict!\n')
      conflicttag <- conflicttag+1
    }
  }
  
  if(conflicttag>0){
    warnmessage <- paste('conflict number:',conflicttag,'.\n',warnmessage)
    warning(warnmessage)
    conflicttag <- TRUE
  }else{
    conflicttag <- FALSE
  }
  
  return(conflicttag)
}


constr_data_prepare <- function(TSF,bmk,constr,rmbmkSus = FALSE){
  
  fnames <- guess_factorNames(TSF,silence = TRUE)
  # 1.add bmk_wgt (add 'bmk_wgt' column and some records that is in bmk but not in TSF)
  TSF2 <- get_bmk_wgt(TSF,bmk=bmk,byTS = FALSE,rmbmkSus = rmbmkSus)
  # 2.add sector constrain factors
  sectorAttr <- unique(constr$fctExp_sector[,"sectorAttr"])
  if(dim(sectorAttr)[1]>0){
    for(i in 1:dim(sectorAttr)[1]){
      sectorAttr_ <- sectorAttr[[i,1]]
      TSF2 <- gf_sector(TSF2,sectorAttr_)
      TSF2 <- dplyr::select(TSF2,-sector)
    }
  }
  # 3.add style-constrain factors
  fctlists <- unique(constr$fctExp_style[,"factorlist"])
  if(dim(fctlists)[1]>0){
    FactorLists <- fctlists$factorlist
    diff.fnames <- setdiff(sapply(FactorLists,'[[','factorName'),fnames)
    common.fnames <- intersect(sapply(FactorLists,'[[','factorName'),fnames)
    if(nrow(TSF2)>nrow(TSF)){
      if(length(common.fnames)>0) TSF2[,common.fnames] <- NULL
      TSF2 <- getMultiFactor(TSF2,FactorLists)
    }else{
      if(length(diff.fnames)>0){
        FactorLists <- FactorLists[sapply(FactorLists,function(x) x$factorName %in% diff.fnames)]
        TSF2 <- getMultiFactor(TSF2,FactorLists)
      }
    }
  }
  # 4.add group-constrain sector factors
  groupIDs <- unique(constr$group$ID)
  groupIDs <- setdiff(groupIDs,colnames(TSF2))
  if(length(groupIDs)>0){
    for(i in 1:length(groupIDs)){
      TSF2 <- is_component(TS=TSF2,sectorID = groupIDs[i])
      TSF2 <- renameCol(TSF2,"is_comp",groupIDs[i])
    }
  }
  # 5.add sector-specified box-constrain sector factors
  sectorIDs <- unique(constr$box$ID)
  sectorIDs <- sectorIDs[substr(sectorIDs,1,2) %in% c("EI","ES")]
  sectorIDs <- setdiff(sectorIDs,colnames(TSF2))
  if(length(sectorIDs)>0){
    for(i in 1:length(sectorIDs)){
      TSF2 <- is_component(TS=TSF2,sectorID = sectorIDs[i])
      TSF2 <- renameCol(TSF2,"is_comp",sectorIDs[i])
    }
  }
  
  # if('init_wgt' %in% colnames(TSF)){
  #   tmp <- na.omit(TSF2[TSF2$init_wgt>0,c('stockID','init_wgt')])
  #   if(nrow(tmp)>0){
  #     constr$box <- rbind(constr$box,data.frame(ID=tmp$stockID,min=tmp$init_wgt,max=tmp$init_wgt,relative=c(0)))
  #   } 
  #   TSF2 <- dplyr::select(TSF2,-init_wgt)
  #   return(list(TSF2=TSF2,constr=constr))
  # }else{
  #   return(TSF2)
  # }
  return(TSF2)
}

