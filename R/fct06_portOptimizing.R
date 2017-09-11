
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
#' factorlists <- buildFactorLists_lcfs(factorIDs = c("F000001","F000002"),factorRefine=refinePar_default("robust"))
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
#' @param alphaf
#' @param fRtn
#' @param fCov
#' @param exp_rtn
#' @param bmk
#' @param constr
#' @param target
#' @param optWay
#' @return a port
#' @export
#' @examples 
#' TS <- getTS(RebDates = as.Date("2017-03-31"),indexID = "EI000300")
#' TSF <- getTSF(TS,FactorList = buildFactorList_lcfs("F000006",factorRefine=refinePar_default("robust")))
#' TSF <- renameCol(TSF,"factorscore","combfct")
#' # constrain setting
#' constr <- constr_default()
#' constr <- addConstr_box(constr,each = c(0,0.02))
#' constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#' conslist <- buildFactorLists_lcfs("F000002_1",factorRefine=refinePar_default("robust",NULL))
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
getPort_opt <- function(TSF,alphaf,
                        fRtn=data.frame(fname=alphaf, frtn=rep(1/length(alphaf),length(alphaf)),stringsAsFactors = FALSE),
                        fCov,
                        exp_rtn="exp_rtn",
                        bmk=NULL,
                        constr=constr_default(),
                        addEvent=FALSE,
                        target=c('return','balance'),
                        optWay=c('ipop','solve.QP','Matlab')){
  target <- match.arg(target)
  optWay <- match.arg(optWay)
  
  if(optWay == "Matlab"){
    R.matlab::Matlab$startServer()
    matlab <- R.matlab::Matlab()
    open(matlab)
  }
  
  fnames <- guess_factorNames(TSF)
  if(missing(alphaf)){
    alphaf <- fnames
  }
  
  
  ### constrain data preparing
  # 1.add bmk_wgt (add 'bmk_wgt' column and some records that is in bmk but not in TSF)
  TSF2 <- get_bmk_wgt(TSF,bmk=bmk,byTS = FALSE)
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
    TSF2 <- getMultiFactor(TSF2,FactorLists = fctlists$factorlist)
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
    if(exp_rtn %in% colnames(TSF)){
      dvec <- TSF2_[univFilter,exp_rtn]
    } else {
      if('date' %in% colnames(fRtn)){
        fRtn_ <- fRtn[fRtn$date==i,-1]
      }else{
        fRtn_ <- fRtn
      }
      rownames(fRtn_) <- fRtn_$fname
      alphamat <- as.matrix(TSF2_[univFilter,alphaf,drop=FALSE])
      dvec <- as.vector(alphamat %*% as.matrix(fRtn_[alphaf,'frtn']))
    }
    if(addEvent){
      # add event return
    }
    
    
    
    # get 'Amat' & 'bvec'
    mat_group <- get_constrMat_group(TSF2_, univFilter, cons = constr$group)
    mat_box <- get_constrMat_box(TSF2_, univFilter, cons = constr$box)
    mat_position <- get_constrMat_position(TSF2_, univFilter, cons = constr$position)
    mat_fctExp_sector <- get_constrMat_fctExp_sector(TSF2_, univFilter, cons = constr$fctExp_sector)
    mat_fctExp_style <- get_constrMat_fctExp_style(TSF2_, univFilter, cons = constr$fctExp_style)
    
    
    
    if(target=='balance'){
      #get 'Dmat'
      if('date' %in% colnames(fCov)){
        fCov_ <- fCov[fCov$date==i,-1]
      }else{
        fCov_ <- fCov
      }
      rownames(fCov_) <- colnames(fCov_)
      
      Fcovmat <- as.matrix(fCov_[alphaf,alphaf])
      Dmat <- alphamat %*% Fcovmat %*% t(alphamat)
      Dmat <- (Dmat+t(Dmat))/2
      # tmp <- Matrix::nearPD(Dmat)
      # Dmat <- tmp$mat
      # Dmat <- matrix(Dmat,nrow = nrow(Dmat))
      nstock <- dim(Dmat)[1]
      
      if(optWay == "solve.QP"){
        Amat <- cbind(riskmat,-1*riskmat)
        Amat <- cbind(1,Amat,diag(x=1,nstock),diag(x=-1,nstock))#control weight
        bvec <- c(1,totwgt$min,-1*totwgt$max,wgtLimit$min,-1*wgtLimit$max)
        system.time(res <- quadprog::solve.QP(Dmat,dvec,Amat,bvec,meq = 1))
        tmp <- data.frame(date=i,stockID=univ,wgt=res$solution)
        
      }else if(optWay == "ipop"){
        f.ipop <- matrix(-dvec, ncol = 1)
        A.ipop <- t(cbind(1,riskmat))
        b.ipop <- c(1,totwgt$min)
        dif.ipop <- totwgt$max - totwgt$min
        r.ipop <- c(0, dif.ipop)
        lb.ipop <- matrix(data = wgtLimit$min, nrow = nstock, ncol = 1)
        ub.ipop <- matrix(data = wgtLimit$max, nrow = nstock, ncol = 1)
        system.time(res.ipop <- kernlab::ipop(c = f.ipop, H = Dmat,
                                              A = A.ipop, b = b.ipop, r = r.ipop,
                                              l = lb.ipop, u = ub.ipop,
                                              maxiter = 3000))
        tmp <- data.frame(date=i,stockID=univ,wgt=res.ipop@primal)
        
      }else if(optWay == "Matlab"){
        H.matlab <- Dmat
        f.matlab <- matrix(-dvec, ncol = 1)
        A.matlab <- t(cbind(-1*riskmat, riskmat))
        b.matlab <- as.matrix(c(-1*totwgt$min, totwgt$max), ncol=1)
        Aeq.matlab <- matrix(data = 1, nrow = 1, ncol = nstock)
        beq.matlab <- 1
        lb.matlab <- matrix(data = wgtLimit$min, nrow = nstock, ncol = 1)
        ub.matlab <- matrix(data = wgtLimit$max, nrow = nstock, ncol = 1)
        system.time({
          R.matlab::setVariable(matlab, H = H.matlab, f = f.matlab, A = A.matlab, b = b.matlab,
                                Aeq = Aeq.matlab, beq = beq.matlab, lb = lb.matlab, ub = ub.matlab)
          R.matlab::evaluate(matlab, "optionn = optimoptions(@quadprog,'Algorithm','interior-point-convex','MaxIter',5000);")
          R.matlab::evaluate(matlab, "res = quadprog(H,f,A,b,Aeq,beq,lb,ub,[],optionn);")
          res.tmp <- R.matlab::getVariable(matlab, "res")
          res.matlab <- res.tmp$res
        })
        tmp <- data.frame(date=i,stockID=univ,wgt=res.matlab)
      }
      
      tmp <- tmp[tmp$wgt>0.0005,]
      colnames(tmp) <-c( "date","stockID","wgt")
      tmp <- transform(tmp,wgt=wgt/sum(wgt))
      
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
      port_$wgt <- port_$wgt/sum(port_$wgt)  # what if port is a longshort port? todo...
    }
    port <- rbind(port,port_)
  }# for dates end
  
  if(optWay == "Matlab"){
    close(matlab)
  }
  port$date <- as.Date(port$date,origin = '1970-01-01')
  port$stockID <- as.character(port$stockID)
  return(port)
}

