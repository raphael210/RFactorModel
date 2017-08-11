
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
                                stringsAsFactors = FALSE)
  constr$trackingerror <- data.frame(ID = character(0), 
                                target = numeric(0),
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
addConstr_turnover <- function(constr,turnover_target=0.5){
  cons <- data.frame(ID="turnover",target=turnover_target,stringsAsFactors = FALSE)
  constr$turnover <- rbind(constr$turnover,cons)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_trackingerror(constr,trackingerror_ann=0.08)
addConstr_trackingerror <- function(constr,trackingerror_ann=0.08){
  cons <- data.frame(ID="trackingerror",target=trackingerror_ann,stringsAsFactors = FALSE)
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
setConstr_turnover <- function(constr,turnover_target){
  constr <- clearConstr(constr,"turnover")
  constr <- addConstr_turnover(constr,turnover_target)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
setConstr_trackingerror <- function(constr,trackingerror_ann){
  constr <- clearConstr(constr,"trackingerror")
  constr <- addConstr_trackingerror(constr,trackingerror_ann)
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
addObj_risk <- function(obj,method=c('solve.QP','ipop','matlab','rmosek'),risk_aversion=1){
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


get_bmk_wgt <- function(TS,bmk=NULL,byTS=TRUE){
  if(is.null(bmk)){
    TS$wgt_bmk <- 0
  } else {
    benchdata <- getIndexCompWgt(indexID = bmk,endT = unique(TS$date))
    colnames(benchdata) <- c('date','stockID','wgt_bmk')
    
    #deal with total weight not equal to 1
    benchdata <- dplyr::arrange(benchdata,date,desc(wgt_bmk))
    benchdata <- benchdata %>% dplyr::group_by(date) %>% mutate(id=seq(1,length(date))) %>% ungroup()
    errdata <- benchdata %>% dplyr::group_by(date) %>% dplyr::summarise(exwgt=sum(wgt_bmk)-1) %>% dplyr::ungroup()
    benchdata <- dplyr::left_join(benchdata,errdata,by='date')
    benchdata[,'wgt_bmk'] <- ifelse(benchdata$id==1,benchdata$wgt_bmk-benchdata$exwgt,benchdata$wgt_bmk)
    benchdata <- benchdata[,c('date','stockID','wgt_bmk')]
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
#' @param init_port if \code{\bold{turnover}} constraint is included,then \code{\bold{init_port}} is required. 
#' @param delta if \code{\bold{trackingerror}} constraint is included,then \code{\bold{delta}} is required. 
#' @return a port
#' @export
#' @examples 
#' TS <- getTS(RebDates = as.Date("2017-03-31"),indexID = "EI000300")
#' TSF <- getTSF(TS,FactorList = buildFactorList_lcfs("F000006",factorStd = "sectorNe",factorNA = "median"))
#' TSF <- renameCol(TSF,"factorscore","combfct")
#' # constrain setting
#' constr <- constr_default(box_each = c(0,0.02))
#' constr <- addConstr_box(constr,each = c(0,0.02))
#' constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#' conslist <- buildFactorLists_lcfs("F000002_1",factorStd = "norm",factorNA = "median")
#' # with bmk
#' constr <- addConstr_fctExp_style(constr,conslist,-0.1,0.1)
#' port_opt <- getPort_opt(TSF,bmk = "EI399330",constr = constr)
#' # without bmk
#' constr2 <- setConstr_fctExp_sector(constr,each = c(0,0.1),relative = 0)
#' port_opt2 <- getPort_opt(TSF,bmk=NULL,constr = constr2)
#' # long-short port
#' constr3 <- setConstr_position(constr_default(),position = c(0,0),relative = 0)
#' constr3 <- setConstr_box(constr3,each=c(-0.05,0.05),relative=0)
#' port_opt3 <- getPort_opt(TSF,bmk="EI000300",constr = constr3)
#' 
#' 
#' 
#' 
#' RebDates <- getRebDates(as.Date('2015-01-31'),as.Date('2017-06-30'))
#' TS <- getTS(RebDates,indexID = 'EI000906')
#' factorIDs <- c("F000002","F000006","F000008")
#' FactorLists <- buildFactorLists_lcfs(factorIDs,factorStd="norm",factorNA = "median")
#' TSF <- getMultiFactor(TS,FactorLists)
#' TSFR <- getTSR(TSF)
#' re <- reg.TSFR(TSFR)
#' rtn_cov_delta <- f_rtn_cov_delta(reg_results=re)
#' fRtn <- rtn_cov_delta$fRtn
#' fCov <- rtn_cov_delta$fCov
#' constr <- constr_default(box_each = c(0,0.02))
#' constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#' constr <- addConstr_fctExp_style(constr,FactorLists,-0.1,c(0.1,100,1),relative = 0)
#' TSFs <- filter(TSF,date==max(date))
#' TS <- TS.getTech_ts(TSFs[,1:2],'base(12017)',varname = 'ipo')
#' TS <- gf.PE_ttm(TS,fillna = FALSE)
#' TS <- rename(TS,pe_ttm=factorscore)
#' TS <- gf.PB_mrq(TS,fillna = FALSE)
#' TS <- filter(TS,(date-intdate2r(ipo))>730,pe_ttm>0,factorscore>0)
#' TSFs <- left_join(TS[,1:2],TSFs,by=c('date','stockID'))
#' 
#' port_opt4 <- getPort_opt(TSFs,fRtn = fRtn,bmk="EI000300",constr = constr)
#' obj <- object_default()
#' obj <- addObj_risk(obj)
#' port_opt5 <- getPort_opt(TSFs,fRtn = fRtn,fCov=fCov,bmk="EI000300",constr = constr,obj = obj)
#' 
#' # add turnover constraint
#' constr <- addConstr_turnover(constr)
#' init_port <- getIndexCompWgt(indexID = 'EI000906',endT = TSFs$date[1])
#' port_opt6 <- getPort_opt(TSFs,fRtn = fRtn,fCov=fCov,bmk="EI000905",constr = constr,obj = obj,init_port=init_port)
#' 
#' constr <- addConstr_trackingerror(constr)
#' 
getPort_opt <- function(TSF,
                        fRtn,
                        fCov,
                        exp_rtn="exp_rtn",
                        bmk=NULL,
                        constr=constr_default(),
                        obj=object_default(),
                        init_port = NULL,
                        delta=NULL){
  
  fnames <- guess_factorNames(TSF)

  ### constrain data preparing
  # 1.add bmk_wgt (add 'bmk_wgt' column and some records that is in bmk but not in TSF)
  TSF2 <- get_bmk_wgt(TSF,bmk=bmk,byTS = FALSE)
  # 2.add sector constrain factors
  sectorAttr <- unique(constr$fctExp_sector[,"sectorAttr"])
  if(dim(sectorAttr)[1]>0){
    for(i in 1:dim(sectorAttr)[1]){
      sectorAttr_ <- sectorAttr[[i,1]]
      TSF2 <- gf.sector(TSF2,sectorAttr_)
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
  
  if(dim(constr$trackingerror)[1]>0 | dim(constr$turnover)[1]>0 | obj$risk[,'method']=='matlab'){
    require(R.matlab)
    R.matlab::Matlab$startServer()
    matlab <- R.matlab::Matlab()
    open(matlab)
  }
  
  
  # looping.....
  dates <- unique(TSF$date)
  port <- data.frame()
  for(i in dates){
    cat(rdate2int(as.Date(i,origin = '1970-01-01')), "...\n")
    TSF2_ <- TSF2[TSF2$date==i,]
    TSF_ <- TSF[TSF$date==i,]
    
    #remove unqualified TS
    TSF_ <- rm_suspend(TSF_)
    
    univFilter <- TSF2_$stockID %in% TSF_$stockID
    univ <- TSF2_[univFilter,"stockID"]
    
    # get 'dvec'
    if(dim(obj$return)[1]==0){
      dvec <- rep(0,length(univ)) #objects don't have return part
    }else{
      if(exp_rtn %in% colnames(TSF)){
        dvec <- TSF2_[univFilter,exp_rtn]
      } else {
        if('date' %in% colnames(fRtn)){
          fRtn_ <- fRtn[fRtn$date==i,-1]
        }else{
          fRtn_ <- fRtn
        }
        rownames(fRtn_) <- fRtn_$fname
        alphamat <- as.matrix(TSF2_[univFilter,fnames,drop=FALSE])
        dvec <- as.vector(alphamat %*% as.matrix(fRtn_[fnames,'frtn']))
      }
    }

    
    # get 'Amat' & 'bvec'
    mat_group <- get_constrMat_group(TSF2_, univFilter, cons = constr$group)
    mat_box <- get_constrMat_box(TSF2_, univFilter, cons = constr$box)
    mat_position <- get_constrMat_position(TSF2_, univFilter, cons = constr$position)
    mat_fctExp_sector <- get_constrMat_fctExp_sector(TSF2_, univFilter, cons = constr$fctExp_sector)
    mat_fctExp_style <- get_constrMat_fctExp_style(TSF2_, univFilter, cons = constr$fctExp_style)
    
    ##check Amat and bvec
    if(!is.null(mat_box$bvec)){
      
      #check group constrain
      if(!is.null(mat_group$Amat)){
        group_range <- t(mat_group$Amat) %*% mat_box$bvec
        for(j in 1:nrow(group_range)){
          if(max(group_range[j,'min'],mat_group$bvec[j,'min'])>min(group_range[j,'max'],mat_group$bvec[j,'max'])){
            warning(paste('group',rownames(group_range)[j],'constrain unqualified!'))
            next
          }
        }
      }
      
      #check position constrain
      if(!is.null(mat_position$Amat)){
        pos_range <- t(mat_position$Amat) %*% mat_box$bvec
        if(max(pos_range[,'min'],mat_position$bvec[,'min'])>min(pos_range[,'max'],mat_position$bvec[,'max'])){
          warning('position constrain unqualified!')
          next
        }
      }
      
      #check fctExp_sector constrain
      if(!is.null(mat_fctExp_sector$Amat)){
        sector_range <- t(mat_fctExp_sector$Amat) %*% mat_box$bvec
        for(j in 1:nrow(sector_range)){
          if(max(sector_range[j,'min'],mat_fctExp_sector$bvec[j,'min'])>min(sector_range[j,'max'],mat_fctExp_sector$bvec[j,'max'])){
            warning(paste('sector',rownames(sector_range)[j],'constrain unqualified!'))
            next
          }
        }
      }
    }
    
    
    if(dim(constr$turnover)[1]>0 | dim(obj$risk)[1]>0){
      if('date' %in% colnames(fCov)){
        fCov_ <- fCov[fCov$date==i,-1]
      }else{
        fCov_ <- fCov
      }
      rownames(fCov_) <- colnames(fCov_)
      
      Fcovmat <- as.matrix(fCov_[fnames,fnames])
      Dmat <- alphamat %*% Fcovmat %*% t(alphamat)
      # Dmat <- (Dmat+t(Dmat))/2
      if(any(eigen(Dmat,only.values = T)[[1]]<=0)){
        tmp <- Matrix::nearPD(Dmat)
        Dmat <- tmp$mat
        Dmat <- matrix(Dmat,nrow = nrow(Dmat))
      }
    }

    
    
    if(dim(constr$trackingerror)[1]>0){
      system.time({
        R.matlab::setFunction(matlab, " function w=opt_turnover(MU,COV,N,A_leq,b_leq,LB,UB,W0,delta)  \
                    cvx_begin                                                     \
                        variable w(N)                                             \
                        minimize(-MU'*w+w'*COV*w)                          \
                        subject to                                                \
                          A_leq*w<=b_leq                                          \
                          LB<=w<=UB                                               \
                          norm(w-W0,1)<=delta                                    \
                    cvx_end                                                       \
                  ")
        
        MU.matlab <-  matrix(dvec, ncol = 1)
        COV.matlab <-  Dmat * obj$risk[,'risk_aversion']
        N.matlab <- length(univ)
        A.matlab <- matrix(t(cbind(mat_position$Amat*-1,mat_position$Amat,
                                   mat_group$Amat*-1,mat_group$Amat,
                                   mat_fctExp_sector$Amat*-1,mat_fctExp_sector$Amat,
                                   mat_fctExp_style$Amat*-1,mat_fctExp_style$Amat)),ncol = N.matlab)
        b.matlab <- matrix(c(mat_position$bvec[,'min']*-1,mat_position$bvec[,'max'],
                             mat_group$bvec[,'min']*-1,mat_group$bvec[,'max'],
                             mat_fctExp_sector$bvec[,'min']*-1,mat_fctExp_sector$bvec[,'max'],
                             mat_fctExp_style$bvec[,'min']*-1,mat_fctExp_style$bvec[,'max']),ncol=1)
        lb.matlab <- matrix(data = mat_box$bvec[,'min'], ncol = 1)
        ub.matlab <- matrix(data = mat_box$bvec[,'max'], ncol = 1)
        delta.matlab <- constr$turnover[,'target']
        
        R.matlab::setVariable(matlab, 
                              MU=MU.matlab,
                              COV=COV.matlab,
                              N=N.matlab,
                              A_leq=A.matlab,
                              b_leq=b.matlab,
                              LB = lb.matlab, 
                              UB = ub.matlab,
                              W0 = init_wgt,
                              delta=delta.matlab)
        R.matlab::evaluate(matlab, "w = opt_turnover(MU,COV,N,A_leq,b_leq,LB,UB,W0,delta);")
        res.tmp <- R.matlab::getVariable(matlab, "w")
      })
      

      
      
      
    }else if(dim(constr$turnover)[1]>0){

      if(i!=dates[1]){
        init_port <- port[port$date==max(port$date),c('stockID','wgt')]
      }
      TSF2_ <- dplyr::left_join(TSF2_,init_port,by='stockID')
      TSF2_$wgt <- TSF2_$wgt-TSF2_$wgt_bmk
      init_wgt <- matrix(TSF2_[univFilter,'wgt'],ncol = 1)
      
      require(Rmosek)
      COV <- rbind(cbind(Dmat,-1*Dmat),
                   cbind(-1*Dmat,Dmat))
      
      if(any(eigen(COV,only.values = T)[[1]]<=0)){
        tmp <- Matrix::nearPD(COV)
        COV <- tmp$mat
        COV <- matrix(COV,nrow = nrow(COV))
      }
      N <- length(univ)
      
      F.mosek <-  Matrix::chol(COV)
      f.mosek <- c(-dvec+2* obj$risk[,'risk_aversion']*Dmat %*% init_wgt,
                   dvec-2* obj$risk[,'risk_aversion']*Dmat %*% init_wgt)
      A1 <- t(cbind(mat_position$Amat*-1,mat_position$Amat,
                         mat_group$Amat*-1,mat_group$Amat,
                         mat_fctExp_sector$Amat*-1,mat_fctExp_sector$Amat,
                         mat_fctExp_style$Amat*-1,mat_fctExp_style$Amat))
      A.mosek <- rbind(cbind(A1,-1*A1),1)
      b1 <- matrix(c(mat_position$bvec[,'min']*-1,mat_position$bvec[,'max'],
                          mat_group$bvec[,'min']*-1,mat_group$bvec[,'max'],
                          mat_fctExp_sector$bvec[,'min']*-1,mat_fctExp_sector$bvec[,'max'],
                          mat_fctExp_style$bvec[,'min']*-1,mat_fctExp_style$bvec[,'max']),ncol=1)
      b.mosek <- c(b1-A1%*%init_wgt,constr$turnover[,'target'])
      
      C1 <- rbind(cbind(diag(1,N),diag(-1,N)),
                  cbind(diag(-1,N),diag(1,N)))
      d1 <- c(mat_box$bvec[,'max']-init_wgt,
              -(mat_box$bvec[,'min']-init_wgt))
      A.mosek <- rbind(A.mosek,C1)
      b.mosek <- c(b.mosek,d1)
      lb.mosek <- rep(0,2*N)
      ub.mosek <- rep(1,2*N)
      prob <- mosek_qptoprob(F =F.mosek , f=f.mosek, A=A.mosek, b=b.mosek, lb=lb.mosek, ub=ub.mosek)
      try(r <- mosek(prob))
      port_ <- data.frame(date=i,stockID=univ,
                          wgt=r$sol$itr$xx[1:N]-r$sol$itr$xx[(N+1):(2*N)]+init_wgt, 
                          stringsAsFactors = FALSE)
      port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
      
    }else{
      if(dim(obj$risk)[1]>0){
        
        if(obj$risk[,'method'] == "solve.QP"){
          Amat <- cbind(mat_position$Amat,mat_position$Amat*-1,
                        mat_box$Amat,mat_box$Amat*-1,
                        mat_group$Amat,mat_group$Amat*-1,
                        mat_fctExp_sector$Amat,mat_fctExp_sector$Amat*-1,
                        mat_fctExp_style$Amat,mat_fctExp_style$Amat*-1)
          bvec <- c(mat_position$bvec[,'min'],mat_position$bvec[,'max']*-1,
                    mat_box$bvec[,'min'],mat_box$bvec[,'max']*-1,
                    mat_group$bvec[,'min'],mat_group$bvec[,'max']*-1,
                    mat_fctExp_sector$bvec[,'min'],mat_fctExp_sector$bvec[,'max']*-1,
                    mat_fctExp_style$bvec[,'min'],mat_fctExp_style$bvec[,'max']*-1)
          try(res <- quadprog::solve.QP(Dmat * obj$risk[,'risk_aversion'],dvec,Amat,bvec))
          port_ <- data.frame(date=i,stockID=univ,wgt=res$solution, stringsAsFactors = FALSE)
          port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
          
          
        }else if(obj$risk[,'method'] == "ipop"){
          c.ipop <- matrix(-dvec, ncol = 1)
          A.ipop <- t(cbind(mat_position$Amat,mat_group$Amat,mat_fctExp_sector$Amat,mat_fctExp_style$Amat))
          bvec <- rbind(mat_position$bvec,mat_group$bvec,mat_fctExp_sector$bvec,mat_fctExp_style$bvec)
          b.ipop <- matrix(bvec[,'min'], ncol = 1)
          r.ipop <- matrix(bvec[,'max']-bvec[,'min'], ncol = 1)
          
          lb.ipop <- matrix(data = mat_box$bvec[,'min'], ncol = 1)
          ub.ipop <- matrix(data = mat_box$bvec[,'max'], ncol = 1)
          try(res.ipop <- kernlab::ipop(c = c.ipop, H = Dmat * obj$risk[,'risk_aversion'],
                                        A = A.ipop, b = b.ipop, r = r.ipop,
                                        l = lb.ipop, u = ub.ipop,
                                        maxiter = 3000))
          port_ <- data.frame(date=i,stockID=univ,wgt=res.ipop@primal, stringsAsFactors = FALSE)
          port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
        }else if(obj$risk[,'method'] == "matlab"){
          H.matlab <- Dmat * obj$risk[,'risk_aversion']
          f.matlab <- matrix(-dvec, ncol = 1)
          A.matlab <- t(cbind(mat_position$Amat*-1,mat_position$Amat,
                              mat_group$Amat*-1,mat_group$Amat,
                              mat_fctExp_sector$Amat*-1,mat_fctExp_sector$Amat,
                              mat_fctExp_style$Amat*-1,mat_fctExp_style$Amat))
          b.matlab <- matrix(c(mat_position$bvec[,'min']*-1,mat_position$bvec[,'max'],
                               mat_group$bvec[,'min']*-1,mat_group$bvec[,'max'],
                               mat_fctExp_sector$bvec[,'min']*-1,mat_fctExp_sector$bvec[,'max'],
                               mat_fctExp_style$bvec[,'min']*-1,mat_fctExp_style$bvec[,'max']),ncol=1)
          lb.matlab <- matrix(data = mat_box$bvec[,'min'], ncol = 1)
          ub.matlab <- matrix(data = mat_box$bvec[,'max'], ncol = 1)
          
          system.time({
            R.matlab::setVariable(matlab, H = H.matlab, f = f.matlab, A = A.matlab, b = b.matlab,
                                  lb = lb.matlab, ub = ub.matlab)
            R.matlab::evaluate(matlab, "optionn = optimoptions(@quadprog,'Algorithm','interior-point-convex','MaxIter',5000);")
            R.matlab::evaluate(matlab, "res = quadprog(H,f,A,b,[],[],lb,ub,[],optionn);")
            res.tmp <- R.matlab::getVariable(matlab, "res")
            res.matlab <- res.tmp$res
          })
          
          port_ <- data.frame(date=i,stockID=univ,wgt=res.matlab, stringsAsFactors = FALSE)
          port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
        }else if(obj$risk[,'method'] == "rmosek"){
          require(Rmosek)
          F.mosek <-  Matrix::chol(Dmat * obj$risk[,'risk_aversion'])
          f.mosek <- -dvec
          A.mosek <- t(cbind(mat_position$Amat*-1,mat_position$Amat,
                             mat_group$Amat*-1,mat_group$Amat,
                             mat_fctExp_sector$Amat*-1,mat_fctExp_sector$Amat,
                             mat_fctExp_style$Amat*-1,mat_fctExp_style$Amat))
          b.mosek <- matrix(c(mat_position$bvec[,'min']*-1,mat_position$bvec[,'max'],
                              mat_group$bvec[,'min']*-1,mat_group$bvec[,'max'],
                              mat_fctExp_sector$bvec[,'min']*-1,mat_fctExp_sector$bvec[,'max'],
                              mat_fctExp_style$bvec[,'min']*-1,mat_fctExp_style$bvec[,'max']),ncol=1)
          
          lb.mosek <- mat_box$bvec[,'min']
          ub.mosek <- mat_box$bvec[,'max']
          prob <- mosek_qptoprob(F =F.mosek , f=f.mosek, A=A.mosek, b=b.mosek, lb=lb.mosek, ub=ub.mosek)
          try(r <- mosek(prob))
          port_ <- data.frame(date=i,stockID=univ,wgt=r$sol$itr$xx[1:length(univ)], stringsAsFactors = FALSE)
          port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
        }
        
        port_ <- port_[abs(port_$wgt)>0.0005,]
        colnames(port_) <-c("date","stockID","wgt")
        if(constr$position[,'min']==1 &  constr$position[,'max']==1){
          port_$wgt <- port_$wgt/sum(port_$wgt)  
        }
        
        
      }else{
        require(PortfolioAnalytics)
        pspec <- portfolio.spec(assets=univ)
        pspec <- add.constraint(portfolio=pspec, type="weight_sum", min_sum=mat_position$bvec[,"min"], max_sum=mat_position$bvec[,"max"])
        pspec <- add.constraint(portfolio=pspec,type="box",min=mat_box$bvec[,"min"],max=mat_box$bvec[,"max"])
        Amat <- cbind(mat_group$Amat,mat_fctExp_sector$Amat,mat_fctExp_style$Amat)
        bvec <- rbind(mat_group$bvec,mat_fctExp_sector$bvec,mat_fctExp_style$bvec)
        pspec <- add.constraint(portfolio=pspec, type="factor_exposure",
                                B=Amat,lower=bvec[,"min"],upper=bvec[,"max"])
        pspec <- add.objective(portfolio=pspec,type='return',name='mean')
        dvec <- as.xts(matrix(dvec,nrow=1),order.by = as.Date(i,origin = '1970-01-01'))
        opt_maxret <- optimize.portfolio(R=dvec, portfolio=pspec,
                                         optimize_method="ROI",
                                         trace=TRUE)
        
        port_ <- data.frame(date=i,stockID=univ,wgt=opt_maxret$weights, stringsAsFactors = FALSE)
        port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
        port_ <- port_[abs(port_$wgt)>0.0005,]
        if(constr$position[,'min']==1 &  constr$position[,'max']==1){
          port_$wgt <- port_$wgt/sum(port_$wgt)  
        }
      }
      
    }
    

    port <- rbind(port,port_)
  }# for dates end
  
  if(dim(constr$trackingerror)[1]>0 | obj$risk[,'method']=='matlab'){
    close(matlab)
  }

  port <- transform(port,date=as.Date(date,origin = '1970-01-01'),
                    stockID=as.character(stockID))
  return(port)
}

