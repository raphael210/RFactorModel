
# ---  build, set, add, clear constrains  ------

#' opt_constrain
#' 
#' build, set, add, clear constrains
#' 
#' @name opt_constrain
#' @param constr a list contained the constraining setting.
#' @param relative integer of 0,1,2
#' @return a list contain the constrains 
#' @export
#' @rdname opt_constrain
#' @examples 
#' constr <- constr_default()
constr_default <- function(position=c(1, 1), box_each=c(0,0.1)){
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
  constr$leverage <- data.frame(ID = character(0),
                                lev = numeric(0),
                                stringsAsFactors = FALSE)
  constr$sus_lim <- data.frame(ID = c("sus","lim_up","lim_low"), 
                               stringsAsFactors = FALSE)
  
  constr <- addConstr_position(constr, position = position, relative = 0)
  constr <- addConstr_box(constr, each = box_each, relative = 0, priority = NA)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' clearConstr(constr,"box")
clearConstr <- function(constr,item){
  constr[[item]] <- constr[[item]][0,,drop=FALSE]
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


#' @export
#' @rdname opt_constrain
#' @param FactorLists a factorlists
#' @param min a vector with the same length of FactorLists, or a scalar
#' @param max a vector with the same length of FactorLists, or a scalar
#' @examples
#' factorlists <- buildFactorLists_lcfs(factorIDs = c("F000001","F000002"),factorRefine=refinePar_default("scale",NULL))
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
#' @param  priority integer(only the max priority will make effect on the same stock), NA(not involved in the integeral priority, enforce to get the intersetion in the final result.) 
#' @examples
#' constr <- constr_default()
#' constr <- addConstr_box(constr,each=c(0,0.035),relative = 0,priority = NA)
#' constr <- addConstr_box(constr,EQ002011=c(0,0.05),EQ000028=c(0,0.05),priority = 2)
#' constr <- addConstr_box(constr,EI399005=c(-0.2,0.2),relative = 1,priority = 1)
#' constr <- addConstr_box(constr,ES33370000=c(0,0.03),relative = 0,priority = 1)
#' constr <- addConstr_box(constr,ES33330000=c(-0.1,0.1),relative = 2,priority = 1)
addConstr_box <- function(constr,each, ..., relative=0, priority=0){
  if(!missing(each)){
    box_each <- data.frame(ID="each",min=each[1],max=each[2],stringsAsFactors = FALSE)
  } else {
    box_each <- data.frame(stringsAsFactors = FALSE)
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
  cons$priority <- as.integer(priority)
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
#' addConstr_leverage(constr,leverage=1.6) #for 130/30
addConstr_leverage <- function(constr,leverage){
  if(missing(leverage)){
    cons <- data.frame(stringsAsFactors = FALSE)
  } else {
    cons <- data.frame(ID="leverage",lev=leverage,stringsAsFactors = FALSE)
  }
  check_constr(cons)
  constr$leverage <- cons
  return(constr)
}


#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_turnover(constr,turnover_target=0.5)
addConstr_turnover <- function(constr,turnover_target=0.25){
  if(nrow(constr$turnover)>0){
    warning("Turnover constrain could only have one. The existing one will be replaced!")
    constr <- clearConstr(constr,"turnover")
  }
  cons <- data.frame(ID="turnover",target=turnover_target,stringsAsFactors = FALSE)
  constr$turnover <- rbind(constr$turnover,cons)
  return(constr)
}
#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_trackingerror(constr,trackingerror_ann=0.08)
addConstr_trackingerror <- function(constr,trackingerror_ann=0.05){
  if(nrow(constr$trackingerror)>0){
    warning("Trackingerror constrain could only have one. The existing one will be replaced!")
    constr <- clearConstr(constr,"trackingerror")
  }
  cons <- data.frame(ID="trackingerror",target=trackingerror_ann,stringsAsFactors = FALSE)
  constr$trackingerror <- rbind(constr$trackingerror,cons)
  return(constr)
}



#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_sus_lim(constr,ID=c("sus","lim_up"))
addConstr_sus_lim <- function(constr,ID=c("sus","lim_up","lim_low")){
  cons <- data.frame(ID=ID,stringsAsFactors = FALSE)
  constr$sus_lim <- rbind(constr$sus_lim,cons)
  return(constr)
}

#' @export
#' @rdname opt_constrain
#' @examples
#' addConstr_FFV(constr,TW_ratio = 3)
addConstr_FFV <- function(constr,TW_ratio = 3){
  if("FFV" %in% names(constr)){
    warning("FFV constrain could only have one. The existing one will be replaced!")
  }
  cons <- data.frame(ID="FFV",TW_ratio = TW_ratio, stringsAsFactors = FALSE)
  constr$FFV <- cons
  return(constr)
}

#' @export
#' @rdname opt_constrain
setConstr_group <- function(constr, ..., relative){
  constr <- clearConstr(constr,"group")
  constr <- addConstr_group(constr = constr, ..., relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_box <- function(constr, each,  ..., relative, priority){
  constr <- clearConstr(constr,"box")
  constr <- addConstr_box(constr = constr, each = each, ..., relative = relative, priority=priority)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_fctExp_sector <- function(constr, each, ..., relative, sectorAttr=defaultSectorAttr()){
  constr <- clearConstr(constr,"fctExp_sector")
  constr <- addConstr_fctExp_sector(constr = constr, each=each, ..., relative = relative, sectorAttr = sectorAttr)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_position <- function(constr, position, relative){
  constr <- clearConstr(constr,"position")
  constr <- addConstr_position(constr = constr, position = position, relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_leverage <- function(constr, leverage){
  constr <- clearConstr(constr,"leverage")
  constr <- addConstr_leverage(constr = constr, leverage = leverage)
  return(constr)
}

#' @export
#' @rdname opt_constrain
setConstr_fctExp_style <- function(constr,FactorLists,min,max,relative){
  constr <- clearConstr(constr,"fctExp_style")
  constr <- addConstr_fctExp_style(constr = constr, FactorLists = FactorLists, min=min, max=max, relative = relative)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_turnover <- function(constr,turnover_target,...){
  constr <- clearConstr(constr,"turnover")
  constr <- addConstr_turnover(constr,turnover_target,...)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_trackingerror <- function(constr,trackingerror_ann,...){
  constr <- clearConstr(constr,"trackingerror")
  constr <- addConstr_trackingerror(constr,trackingerror_ann,...)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_sus_lim <- function(constr,ID){
  constr <- clearConstr(constr,"sus_lim")
  constr <- addConstr_sus_lim(constr,ID)
  return(constr)
}
#' @export
#' @rdname opt_constrain
setConstr_FFV <- function(constr,TW_ratio){
  cons <- data.frame(ID="FFV",TW_ratio = TW_ratio, stringsAsFactors = FALSE)
  constr$FFV <- cons
  return(constr)
}






# ---  get 'linear' constrain result of matrixs and vectors in a single-period ------
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

get_constrMat_leverage <- function(TSF2, univFilter, cons){
  if(nrow(cons)==0){
    return(NULL)
  }
  Amat <- as.matrix(TSF2[univFilter,'wgt_bmk'])
  bvec <- cons[,'lev']
  
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
        if(is.list(sectorAttr_j[[1]])){
          ID_each <- as.character(1:(sectorAttr_j$level))
        }else{
          ID_each <- setdiff(CT_industryList(std=sectorAttr_j$std,level=sectorAttr_j$level)$IndustryID, specified_sec)
          ID_each <- intersect(ID_each,names(TSF2)) # sometimes not all the sectors is in the TSF2 univ.
        }
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



get_constrMat_box <- function(TSF2, univFilter, cons, return_Amat=FALSE){
  if(nrow(cons)==0){
    return(list(Amat=NULL,bvec=NULL))
  }
  univ <- TSF2[univFilter,"stockID"]
  
  # 1.spec_EQ
  cons_spec_EQ <- cons[substr(cons$ID,1,2) %in% c("EQ"), ]
  # 2.spec_sector
  table_sect <- cons[substr(cons$ID,1,2) %in% c("ES","EI"), ]
  if(nrow(table_sect)>0){
    for(j in 1:nrow(table_sect)){
      secID_ <- table_sect[j,"ID"]
      stockIDs_ <- TSF2[TSF2[[secID_]] & univFilter, "stockID"]
      cons_spec_sect_ <- data.frame(ID=stockIDs_, stringsAsFactors = FALSE)
      cons_spec_sect_ <- mutate(cons_spec_sect_,min=table_sect[j,"min"],max=table_sect[j,"max"],relative=table_sect[j,"relative"],priority=table_sect[j,"priority"])
      if(j==1L){
        cons_spec_sect <- cons_spec_sect_
      } else {
        cons_spec_sect <- rbind(cons_spec_sect,cons_spec_sect_)
      }
    }
  } else {
    cons_spec_sect <- data.frame(stringsAsFactors = FALSE)
  }
  # 3.each
  table_each <- cons[cons$ID == "each", ]
  if(nrow(table_each)>0){
    for(j in 1:nrow(table_each)){
      cons_each_ <- data.frame(ID=univ, stringsAsFactors = FALSE)
      cons_each_ <- mutate(cons_each_,min=table_each[j,"min"],max=table_each[j,"max"],relative=table_each[j,"relative"],priority=table_each[j,"priority"])
      if(j==1L){
        cons_each <- cons_each_
      } else {
        cons_each <- rbind(cons_each,cons_each_)
      }
    }
  } else {
    cons_each <- data.frame(stringsAsFactors = FALSE)
  }
  # 4.all
  cons_all <- rbind(cons_spec_EQ,cons_spec_sect,cons_each)
  
  # 5.prioritys: filter the highest priority(except NA)
  cons_num <- dplyr::filter(cons_all,!is.na(priority))
  cons_num <- dplyr::group_by(cons_num,ID)
  cons_num <- dplyr::mutate(cons_num, max_prio=max(priority))
  cons_num <- dplyr::filter(cons_num, priority == max_prio) %>% dplyr::select(-max_prio)
  cons_na <- dplyr::filter(cons_all,is.na(priority))
  cons_prio <- dplyr::bind_rows(cons_na,cons_num)
  
  # 6.relas
  relas <- unique(cons$relative)
  for(i in 1:length(relas)){
    rela <- relas[i]
    cons_ <- cons_prio[cons_prio$relative==rela,]
    if(rela==1L){
      cons_ <- cons_
    } else if(rela==2L){
      cons_ <- dplyr::left_join(cons_,TSF2[,c("stockID","wgt_bmk")],by=c("ID"="stockID"))
      cons_ <- dplyr::mutate(cons_,min=min*wgt_bmk,max=max*wgt_bmk)
    } else {
      cons_ <- dplyr::left_join(cons_,TSF2[,c("stockID","wgt_bmk")],by=c("ID"="stockID"))
      cons_ <- dplyr::mutate(cons_,min=min-wgt_bmk,max=max-wgt_bmk)
    }
    if(i==1L){
      cons_re <- cons_[,c("ID","min","max")]
    } else {
      cons_re <- rbind(cons_re,cons_[,c("ID","min","max")])
    }
  }
  
  # 7.regroup by stockIDs
  cons_re <- data.table::data.table(cons_re,key = "ID")[,list(min=max(min),max=min(max)),by="ID"]
  cons_re <- dplyr::left_join(data.frame(ID=univ,stringsAsFactors = FALSE),cons_re,by="ID")
  cons_re[is.na(cons_re$min),"min"] <- -Inf
  cons_re[is.na(cons_re$max),"max"] <- Inf
  
  # 8.result
  bvec <- as.matrix(cons_re[,c("min","max")])
  if(any(bvec[,2] < bvec[,1])){
    # stop('max less than min in the box constrain!')
    dates <- TSF2$date[1]
    stocks_ <- cons_re %>% filter(min>max)
    warning(paste('date:',dates,',stock:',stocks_$ID,'box constraint max:',stocks_$max,'less than min:',stocks_$min))
    cons_re <- cons_re %>% mutate(max=ifelse(min>max,min,max))
    bvec <- as.matrix(cons_re[,c("min","max")])
  }
  rownames(bvec) <- cons_re$ID
  
  if(return_Amat){
    Amat <- diag(1,nrow = length(univ))
  } else {
    Amat <- NULL
  }
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
      benchdata <- rm_suspend(benchdata,nearby = 0)
    }
    benchdata <- port.wgt_align(benchdata)
    
    benchdata <- dplyr::rename(benchdata,wgt_bmk=wgt)
    if(byTS){
      TS <- merge.x(TS,benchdata,by=c('date','stockID'))
    }else{
      TS <- dplyr::full_join(TS,benchdata,by=c('date','stockID'))
    }
    TS[is.na(TS$wgt_bmk),'wgt_bmk'] <- 0
  }
  return(TS)
}

#' get_exp_rtn
#' @param frtn a vector, or a dataframe
#' @export
#' @examples 
#' # -- frtn as a vector
#' mtsf <- getMultiFactor_lcfs(ts,c("F000008","F000006"),factorRefine = refinePar_default("scale"))
#' re <- get_exp_rtn(mtsf,c(0.1,0.3))
#' # -- frtn as a dataframe
#' mtsfr <- getTSR(mtsf)
#' reg_res <- reg.TSFR(mtsfr)
#' frtn <- getfRtn(reg_results = reg_res)
#' re2 <- get_exp_rtn(mtsf,frtn)
get_exp_rtn <- function(TSF,frtn){  # to be modified......
  alpha_fnames <- guess_factorNames(TSF)
  if(is.data.frame(frtn)){
    frtn <- as.matrix(frtn['frtn'])
  } else {
    frtn <- as.matrix(frtn)
  }
  alphamat <- as.matrix(TSF[,alpha_fnames,drop=FALSE])
  
  # if(TRUE){# alphamat adjustment
  #   # neg, remove positive part
  #   NEG_ind <- substr(alpha_fnames,1,3) == "NEG"
  #   if(sum(NEG_ind) > 0){
  #     NEG_vec <- alpha_fnames[NEG_ind]
  #     for(ii in 1:length(NEG_vec)){
  #       NEG_fname <- NEG_vec[ii]
  #       epsilon <- quantile(alphamat[,NEG_fname],0.3)[[1]]
  #       alphamat[alphamat[,NEG_fname] > epsilon, NEG_fname] <- 0
  #     }
  #   }
  #   # pos, remove neg part
  #   POS_ind <- substr(alpha_fnames,1,3) == "POS"
  #   if(sum(POS_ind) > 0){
  #     POS_vec <- alpha_fnames[POS_ind]
  #     for(ii in 1:length(POS_vec)){
  #       POS_fname <- POS_vec[ii]
  #       epsilon <- quantile(alphamat[,POS_fname],0.7)[[1]]
  #       alphamat[alphamat[,POS_fname] < epsilon, POS_fname] <- 0
  #     }
  #   }
  # }
  
  dvec <- as.vector(alphamat %*% frtn)
  re <- data.frame(TSF,exp_rtn=dvec)
  return(re)
}




# ---  port optimizing  ------
#' getPort_opt
#' 
#' @param TSF a \bold{TSF} object,may contains multiple factors.
#' @param exp_rtn a string, indicating the expected rtn column name. Or NULL, which means return part doesn't involved in the optimization.
#' @param bmk benchmark indexID code.
#' @param constr constrain lists,see \code{\link{opt_constrain}}.
#' @param lamda numeric scalar, risk_aversion
#' @param init_port if turnover constrain involved ,param \code{init_port} is required.
#' @param min_wgt Numeric, or NULL. stock's minimum weight in optimized portfolio.
#' @return a optimized port
#' @export
#' @examples 
#' TS <- getTS(RebDates = as.Date("2017-03-31"),indexID = "EI000300")
#' TSF <- getTSF(TS,FactorList = buildFactorList_lcfs("F000006",factorRefine = refinePar_default("scale")))
#' # constrain setting
#' constr <- constr_default(box_each = c(0,0.02))
#' constr <- addConstr_box(constr,each = c(0,0.02))
#' constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
#' conslist <- buildFactorLists_lcfs("F000002",factorRefine = refinePar_default("scale",NULL))
#' # with bmk
#' constr <- addConstr_fctExp_style(constr,conslist,-0.1,0.1)
#' port_opt <- getPort_opt(TSF,bmk = "EI399330",constr = constr,exp_rtn = 'factorscore')
#' # without bmk
#' constr2 <- setConstr_fctExp_sector(constr,each = c(0,0.1),relative = 0)
#' port_opt2 <- getPort_opt(TSF,bmk=NULL,constr = constr2,exp_rtn = 'factorscore')
#' # long-short port
#' constr3 <- setConstr_position(constr_default(),position = c(0,0),relative = 0)
#' constr3 <- setConstr_box(constr3,each=c(-0.05,0.05),relative=0)
#' port_opt3 <- getPort_opt(TSF,bmk="EI000300",constr = constr3,exp_rtn = 'factorscore')
getPort_opt <- function(TSF,
                        bmk=NULL,
                        exp_rtn="factorscore",
                        risk_list=NULL,
                        lamda = NULL,
                        constr=constr_default(),
                        init_port=NULL,
                        min_wgt=0.0001,
                        optsolver=c('CVXR','RMOSEK')){
  
  optsolver <- match.arg(optsolver)
  
  ### add bmk_wgt (add 'bmk_wgt' column and some records that is in bmk but not in TSF)
  TSF2 <- get_bmk_wgt(TSF,bmk=bmk,byTS = FALSE,rmbmkSus = FALSE)
  
  ### constrain data preparing
  TSF2 <- add_constr_data(TSF = TSF2, constr = constr)
  
  ### init_port
  if(is.null(init_port)){
    init_port <- data.frame(stringsAsFactors = FALSE)
  }else{
    init_port <- init_port[,c('stockID','wgt')]
  }
  
  ### looping.....
  dates <- unique(TSF$date)
  port <- data.frame()
  for(i in 1:length(dates)){
    message(rdate2int(dates[i]), ",", appendLF = FALSE)
    TSF_ <- TSF[TSF$date==dates[i],]
    TSF2_ <- TSF2[TSF2$date==dates[i],]
    univ <- TSF_$stockID
    univFilter <- TSF2_$stockID %in% univ
    
    ## turnover constrain
    if(dim(constr$turnover)[1]>0){
      turnover_target <- constr$turnover[,'target']*2
    }
    
    ## init_wgt 
    if(nrow(init_port)>0){
      init_notInUniv <- !(init_port$stockID %in% univ)
      init_port_notInUniv <- init_port[init_notInUniv,]
      init_port <- init_port[!init_notInUniv,]
      # change to relative wgt
      init_wgt <- TSF2_ %>% dplyr::select(stockID,wgt_bmk) %>% 
        dplyr::left_join(init_port[,c('stockID','wgt')],by='stockID') %>% 
        mutate(wgt=ifelse(is.na(wgt),0,wgt)) %>% 
        mutate(wgt=wgt-wgt_bmk)
      init_wgt <- matrix(init_wgt[univFilter,'wgt'],ncol = 1)
      # init port component not in univ,then have to sell. Change the turn over target
      if(nrow(init_port_notInUniv)>0 && dim(constr$turnover)[1]>0){
        turnover_target <- turnover_target + sum(init_port_notInUniv[,'wgt'])
      } 
    }
    
    
    ## get 'dvec'
    if(is.null(exp_rtn)){
      dvec <- rep(0,length(univ)) #objects don't have return part
    } else if(! exp_rtn %in% names(TSF2_)){
      stop("'exp_rtn' not exist!")
    } else {
      dvec <- TSF2_[univFilter,exp_rtn]
      if(sd(dvec)>0){
        dvec <- dvec/sd(dvec) # scale
      }
    }
    
    
    
    ## get 'Dmat'
    if(!is.null(risk_list)){
      if(is.list(risk_list) & identical(names(risk_list),c('mTSF','fCov','sigma'))){
        # univ
        univ_df <-  data.frame(stockID=univ, stringsAsFactors = FALSE)
        # risk exposure
        risk_mat <- risk_list$mTSF
        risk_mat <- risk_mat %>% dplyr::filter(date==dates[i]) %>% dplyr::select(-date)
        risk_mat <- dplyr::left_join(univ_df,risk_mat,by='stockID') %>% dplyr::select(-stockID)
        risk_mat <- as.matrix(risk_mat)
        # covariance matrix
        fCov_mat <- risk_list$fCov
        if('date' %in% colnames(fCov_mat)){
          fCov_mat <- fCov_mat %>% dplyr::filter(date==dates[i]) %>% dplyr::select(-date)
        }
        fCov_mat <- as.matrix(fCov_mat)
        # sigma matrix
        sigma_mat <- risk_list$sigma
        if('date' %in% colnames(sigma_mat)){
          sigma_mat <- sigma_mat %>% dplyr::filter(date==dates[i]) %>% dplyr::select(-date)
        }
        sigma_mat <- dplyr::left_join(univ_df,sigma_mat,by='stockID')
        # sigma_mat <- sigma_mat %>% dplyr::mutate(sigma= ifelse(is.na(sigma), median(sigma, na.rm=TRUE), sigma))
        sigma_mat <- as.vector(sigma_mat$sigma)
        # double check
        if(!identical(colnames(risk_mat),colnames(fCov_mat))) stop('Factor Exposure and Covariance Matrix do not match.')
        # output
        Dmat <- list("fexpo" = risk_mat, "fCov" = fCov_mat, "sigma" = sqrt(sigma_mat))
        # 
        if(optsolver == "RMOSEK"){
          sigma_mat <- diag(sigma_mat)
          Dmat <- risk_mat %*% fCov_mat %*% t(risk_mat) + sigma_mat
        }
      }else{
        # this part is processed when each element in the risk_list is the NxN large complete covariance matrix for each specific date
        sigma <- risk_list[[as.character(dates[i])]]
        sigma <- as.data.frame(sigma)
        sigma$stockID <- colnames(sigma)
        
        sigma_mat <- data.frame(stockID=univ,stringsAsFactors = FALSE)
        sigma_mat <- dplyr::left_join(sigma_mat, sigma, by = "stockID")
        # to do ...
        # sigma_mat[is.na(sigma_mat)] <- ...
        sigma_mat$stockID <- NULL
        
        Dmat <- as.matrix(sigma_mat)
      }
    }else{
      Dmat <- NULL
    }
    
    
    
    ## get 'Amat' & 'bvec'
    mat_group <- get_constrMat_group(TSF2_, univFilter, cons = constr$group)
    mat_position <- get_constrMat_position(TSF2_, univFilter, cons = constr$position)
    leverage <- get_constrMat_leverage(TSF2_, univFilter, cons = constr$leverage)
    mat_fctExp_sector <- get_constrMat_fctExp_sector(TSF2_, univFilter, cons = constr$fctExp_sector)
    mat_fctExp_style <- get_constrMat_fctExp_style(TSF2_, univFilter, cons = constr$fctExp_style)
    # mat_box
    constr_box <- constr$box
    if(exists('FFV', where=constr) && dim(constr$FFV)[1]>0){# add box constrain due to FFV
      constr_box_FFV <- con_box_FFV(TS = TSF_[,c('date','stockID')],TW_ratio = constr$FFV[,'TW_ratio'],bmk = bmk)
      constr_box <- rbind(constr_box,constr_box_FFV)
    }
    mat_box <- get_constrMat_box(TSF2_, univFilter, cons = constr_box)
    # reshape box_bvec due to suspend & price limit
    if(dim(constr$sus_lim)[1]>0){ 
      sus_lim <- wgt_sus_lim(TS = TSF_[,c('date','stockID')], init_port = init_port, TSF2=TSF2_, 
                             constr_sus_lim = constr$sus_lim)
      mat_box$bvec <- box_bvec_reshape_sus_lim(box_bvec = mat_box$bvec, wgt_sus_lim = sus_lim)
    }
    # wrapping
    mat_vec_list <- list(mat_group=mat_group,
                         mat_box=mat_box,
                         mat_position=mat_position,
                         mat_fctExp_sector=mat_fctExp_sector,
                         mat_fctExp_style=mat_fctExp_style)
    
    
    
    ## check Amat and bvec
    jumptag <- mat_constr_check(mat_vec_list,date = dates[i],type='auto')
    mat_vec_list <- jumptag[['mat_vec_list']]
    jumptag <- jumptag[['conflicttag']]
    if(jumptag){
      next
    } 
    
    ## Solving
    if(dim(constr$trackingerror)[1]>0){
      trackingerror_target <- constr$trackingerror[,'target']
      lamda <- NULL
    }else{
      trackingerror_target <- NULL
    }
    
    if(dim(constr$turnover)[1]==0 || nrow(init_port)==0){
      turnover_target <- NULL
      init_wgt <- NULL
    }
    
    if(optsolver=='CVXR'){
      wgt_ <- try(solver_CVXR(dvec,Dmat,mat_vec_list,trackingerror_target,turnover_target,init_wgt,lamda,leverage),silent = FALSE)
    }else if(optsolver=='RMOSEK'){
      wgt_ <- try(solver_Rmosek(dvec,Dmat,mat_vec_list,trackingerror_target,turnover_target,init_wgt,lamda,leverage),silent = FALSE)
    }
    
    
    ## opt_port_result
    if(!inherits(wgt_, "try-error") & !all(is.na(wgt_))){ 
      port_ <- data.frame(date=dates[i],stockID=univ,wgt=wgt_, stringsAsFactors = FALSE)
      port_$wgt <- port_$wgt + TSF2_[univFilter,"wgt_bmk"]
      
      if(!is.null(min_wgt)){ # remove the mini-weighted stocks
        target_long <- sum(port_$wgt[port_$wgt>0])
        target_short <- sum(port_$wgt[port_$wgt<0])
        port_ <- port_[abs(port_$wgt)>=min_wgt,]
        port_ <- port.wgt_align(port_, target_long = target_long, target_short = target_short)
      }
      
      port <- rbind(port,port_)
      init_port <- port_[,c('stockID','wgt')]
    }else{
      warning("No optimizing result in ",rdate2int(dates[i]),"!\n")
      init_port <- data.frame(stringsAsFactors = FALSE)
      next
    } 
    
  }# for dates end
  
  port <- transform(port,stockID=as.character(stockID))
  return(port)
}



solver_Rmosek <- function(dvec,Dmat,mat_vec_list,
                          trackingerror_target=NULL,turnover_target=NULL,init_wgt=NULL,lamda=NULL,leverage=NULL){
  
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  if(is.null(trackingerror_target)){
    stocknum <- length(dvec)
    
    if(is.null(turnover_target)){
      # part1:deal with the simplest linear and quadprog optimization
      f.mosek <- -dvec
      
      Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
      A.mosek <- t(Amat_bvec$amat)
      b.mosek <- matrix(c(Amat_bvec$bvec),ncol=1)
      
      lb.mosek <- mat_box$bvec[,'min']
      ub.mosek <- mat_box$bvec[,'max']
      
      if(is.null(Amat_bvec$amat_eq)){
        Aeq.mosek <- NA
        beq.mosek <- NA
      }else{
        Aeq.mosek <- t(Amat_bvec$amat_eq)
        beq.mosek <- matrix(c(Amat_bvec$bvec_eq),ncol=1)
      }
      
      if(is.null(lamda)){
        #simple linear optimization
        prob <- Rmosek::mosek_lptoprob(f=f.mosek, A=A.mosek, b=b.mosek,Aeq=Aeq.mosek,beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
      }else{
        #simple quadprog optimization
        F.mosek <-  Matrix::chol(Dmat * lamda)
        prob <- Rmosek::mosek_qptoprob(F=F.mosek , f=f.mosek, A=A.mosek, b=b.mosek,Aeq=Aeq.mosek,beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
      }
      res <- Rmosek::mosek(prob)
      return(res$sol$itr$xx[1:stocknum])
      #part1 done!
    }else{
      # part2: deal with the linear and quadprog optimization with turnover constraint
      # have to separate the weight vector to buy and sell part, both parts are positive
      Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
      A1 <- t(Amat_bvec$amat)
      b1 <- matrix(c(Amat_bvec$bvec),ncol=1)
      
      A.mosek <- rbind(cbind(A1,-1*A1),1)
      b.mosek <- c(b1-A1%*%init_wgt,turnover_target)
      
      if(is.null(Amat_bvec$amat_eq)){
        Aeq.mosek <- NA
        beq.mosek <- NA
      }else{
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
      if(is.null(lamda)){
        #linear optimization with turnover constraint
        f.mosek <- c(-dvec,dvec)
        prob <- Rmosek::mosek_lptoprob(f=f.mosek, A=A.mosek, b=b.mosek, Aeq=Aeq.mosek, beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
        
      }else{
        #quadprog optimization with turnover constraint
        COV <- rbind(cbind(Dmat,-1*Dmat),
                     cbind(-1*Dmat,Dmat))
        
        
        F.mosek <-  Matrix::chol(COV)
        f.mosek <- c(-dvec+2*lamda*Dmat %*% init_wgt,
                     dvec-2*lamda*Dmat %*% init_wgt)
        prob <- Rmosek::mosek_qptoprob(F =F.mosek , f=f.mosek, A=A.mosek, b=b.mosek, Aeq=Aeq.mosek, beq=beq.mosek, lb=lb.mosek, ub=ub.mosek)
        
      }
      res <- Rmosek::mosek(prob)
      return(res$sol$itr$xx[1:stocknum]-res$sol$itr$xx[(stocknum+1):(2*stocknum)]+init_wgt)
      #part2 done!
    }
    
  }else{
    
    if(is.null(turnover_target)){
      # part3: deal with the linear optimization with tracking error constraint
      Amat_bvec <- mat_vec_bind(dir='gl',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style,eqsep = FALSE)
      nstock <- length(dvec)
      nconstr <- ncol(Amat_bvec$amat)
      Gmat <-  Matrix::chol(Dmat)
      gamma_ <- trackingerror_target
      lo1 <- list()
      lo1$sense <- "max"
      lo1$c <- c(dvec,0,rep(0,nstock))
      Amat_ <- cbind(rbind(t(Amat_bvec$amat),Gmat),
                     matrix(0,ncol = 1,nrow = nstock+nconstr),
                     rbind(matrix(0,ncol = nstock,nrow = nconstr),diag(-1,nrow = nstock)))
      lo1$A <- Matrix::Matrix(Amat_,sparse=TRUE)
      lo1$bc <- rbind(blc = c(Amat_bvec$bvec[,'min'],rep(0,nstock)),
                      buc = c(Amat_bvec$bvec[,'max'],rep(0,nstock)))
      lo1$bx <- rbind(blx = c(mat_box$bvec[,'min'],gamma_,rep(-Inf,nstock)),
                      bux = c(mat_box$bvec[,'max'],gamma_,rep(Inf,nstock)))
      
      lo1$cones <- matrix(list(), nrow=2, ncol=1)
      rownames(lo1$cones) <- c("type","sub")
      lo1$cones[,1] <- list("QUAD", c(nstock+1,(nstock+2):(2*nstock+1)))
      
      r <- Rmosek::mosek(lo1)
      return(r$sol$itr$xx[1:nstock]) 
      #part3 done!
    }else{
      # part4: deal with the linear optimization with tracking error and turnover constraint
      Amat_bvec <- mat_vec_bind(dir='gl',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style,eqsep = FALSE)
      nstock <- length(dvec)
      nconstr <- ncol(Amat_bvec$amat)
      Gmat <-  Matrix::chol(Dmat)
      gamma_ <- trackingerror_target
      lo1 <- list()
      lo1$sense <- "max"
      lo1$c <- c(dvec,0,rep(0,nstock),rep(0,nstock))
      Amat_ <- rbind(cbind(t(Amat_bvec$amat),matrix(0,ncol = 1,nrow = nconstr),matrix(0,ncol = nstock,nrow = nconstr),matrix(0,ncol = nstock,nrow = nconstr)),
                     cbind(Gmat,matrix(0,ncol = 1,nrow = nstock),diag(-1,nrow = nstock),matrix(0,ncol = nstock,nrow = nstock)),
                     cbind(diag(1,nrow = nstock),matrix(0,ncol = 1,nrow = nstock),matrix(0,ncol = nstock,nrow = nstock),diag(-1,nrow = nstock)),
                     cbind(diag(1,nrow = nstock),matrix(0,ncol = 1,nrow = nstock),matrix(0,ncol = nstock,nrow = nstock),diag(1,nrow = nstock)),
                     cbind(matrix(0,ncol=nstock,nrow=1),matrix(0,ncol = nstock+1,nrow = 1),matrix(1,ncol = nstock,nrow=1)))
      lo1$A <- Matrix::Matrix(Amat_,sparse=TRUE)
      lo1$bc <- rbind(blc = c(Amat_bvec$bvec[,'min'],rep(0,nstock),rep(-Inf,nstock),init_wgt,0),
                      buc = c(Amat_bvec$bvec[,'max'],rep(0,nstock),init_wgt,rep(Inf,nstock),turnover_target))
      lo1$bx <- rbind(blx = c(mat_box$bvec[,'min'],gamma_,rep(-Inf,nstock),rep(0,nstock)),
                      bux = c(mat_box$bvec[,'max'],gamma_,rep(Inf,nstock),rep(turnover_target,nstock)))
      
      lo1$cones <- matrix(list(), nrow=2, ncol=1)
      rownames(lo1$cones) <- c("type","sub")
      lo1$cones[,1] <- list("QUAD", c(nstock+1,(nstock+2):(2*nstock+1)))
      
      r <- Rmosek::mosek(lo1)
      return(r$sol$itr$xx[1:nstock]) 
      #part4 done!
    }
  }
}

solver_CVXR <- function(dvec,Dmat,mat_vec_list,
                        trackingerror_target=NULL,turnover_target=NULL,init_wgt=NULL,lamda=NULL,leverage=NULL,solver="ECOS"){
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  require(CVXR)
  stocknum <- length(dvec)
  wgt <- Variable(stocknum)
  
  # OBJECT 
  dvec <- matrix(dvec,nrow = 1,ncol = stocknum)
  rtn <- dvec %*% wgt
  if(is.null(lamda)){
    obj_cvxr <- Maximize(rtn)
  } else {
    if(is.null(Dmat)){
      stop("lamda is not NULL, while Dmat is NULL!")
    } else if(is.list(Dmat)){
      # If Dmat is a list, then it contains two element, covariance matrix of factors and stock residual sigma
      # the decomposition lowers the dimensions and speed up the calculation
      fexpwgt <- t(Dmat$fexpo) %*% wgt
      risk <- quad_form(fexpwgt, Dmat$fCov)
      risk2 <- sum_squares(Dmat$sigma * wgt)
      obj_cvxr <- Maximize(rtn - 0.5*lamda*risk-0.5*lamda*risk2)
    } else {
      # else, Dmat is a large covariance of stocks
      risk <- quad_form(wgt, Dmat)
      obj_cvxr <- Maximize(rtn - 0.5*lamda*risk)
    }
  }
  
  # ADD LINEAR CONSTRAINT
  Amat_bvec <- mat_vec_bind(dir='le',mat_position,mat_group,mat_fctExp_sector,mat_fctExp_style)
  cons_mat <- t(Amat_bvec$amat) %*% wgt <= Amat_bvec$bvec
  constraints_cvxr <- list(cons_mat)
  if(!is.null(Amat_bvec$amat_eq)){
    cons_eq <- t(Amat_bvec$amat_eq) %*% wgt == Amat_bvec$bvec_eq 
    constraints_cvxr <- c(constraints_cvxr,list(cons_eq))
  }
  cons_box1 <- wgt >= mat_box$bvec[,'min']
  cons_box2 <- wgt <= mat_box$bvec[,'max']
  constraints_cvxr <- c(constraints_cvxr,list(cons_box1,cons_box2))
  
  #add trackingerror constraint
  if(!is.null(trackingerror_target)){
    fexpwgt <- t(Dmat$fexpo) %*% wgt
    risk <- quad_form(fexpwgt, Dmat$fCov)
    risk2 <- sum_squares(Dmat$sigma * wgt)
    cons_te <- risk+risk2 <= trackingerror_target*trackingerror_target
    constraints_cvxr <- c(constraints_cvxr,list(cons_te))
  }
  
  #add turnover constraint
  if(!is.null(turnover_target)){
    cons_turnover <- p_norm(wgt-init_wgt,1) <= turnover_target
    constraints_cvxr <- c(constraints_cvxr,list(cons_turnover))
  }
  
  #add leverage constraint
  if(!is.null(leverage)){
    Amat <- leverage$Amat
    bvec <- leverage$bvec
    if(all(Amat==0)){
      cons_lev <- p_norm(wgt,1) <= bvec
    }else{
      cons_lev <- p_norm(wgt+Amat,1) <= bvec
    }
    
    constraints_cvxr <- c(constraints_cvxr,list(cons_lev))
  }
  
  # solve
  prob_cvxr <- Problem(obj_cvxr, constraints = constraints_cvxr)
  output_cvxr <- solve(prob_cvxr, solver=solver)
  
  # get output
  result_cvxr <- output_cvxr$getValue(wgt)
  return(result_cvxr)
}









# param dir  ge means greater than, le means less than,gl means both direction.
# param eqsep means whether seprate amat_eq and bvec_eq.  
mat_vec_bind <- function(dir=c('ge','le','gl'),...,eqsep=TRUE){
  dir <- match.arg(dir)
  
  rawdata <- list(...)
  amat <- rawdata[[1]]$Amat
  bvec <- rawdata[[1]]$bvec
  for(i in 2:length(rawdata)){
    amat <- cbind(amat,rawdata[[i]]$Amat)
    bvec <- rbind(bvec,rawdata[[i]]$bvec)
  }
  
  amat_eq <- NULL
  bvec_eq <- NULL
  if(eqsep){
    eqindex <- unname(which(bvec[,'min']==bvec[,'max']))
    if(length(eqindex)>0){
      amat_eq <- amat[,eqindex,drop=FALSE]
      bvec_eq <- bvec[eqindex,'min',drop=FALSE]
      
      amat <- amat[,-eqindex,drop=FALSE]
      bvec <- bvec[-eqindex,,drop=FALSE]
    }
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


mat_constr_check <- function(mat_vec_list,date,type=c('auto','skip')){
  type <- match.arg(type)
  
  mat_position <- mat_vec_list$mat_position
  mat_box <- mat_vec_list$mat_box
  mat_group <- mat_vec_list$mat_group
  mat_fctExp_sector <- mat_vec_list$mat_fctExp_sector
  mat_fctExp_style <- mat_vec_list$mat_fctExp_style
  
  conflicttag <- 0
  warnmessage <- ''
  
  if(!is.null(mat_box$bvec)){
    
    #check position constrain
    if(!is.null(mat_position$Amat)){
      pos_range <- t(mat_position$Amat) %*% mat_box$bvec
      if(max(pos_range[,'min'],mat_position$bvec[,'min'])>min(pos_range[,'max'],mat_position$bvec[,'max'])){
        warnmessage <- paste(warnmessage,'position bvec_range',brkQT(round(mat_position$bvec[j,],2)),
                             ',pos_range',brkQT(round(pos_range[j,],2)),
                             'constrain unqualified!\n')
        conflicttag <- conflicttag+1
        
        if(type=='auto'){
          if(mat_position$bvec[j,'max']<pos_range[j,'min']){
            mat_position$bvec[j,'min'] <- pos_range[j,'min']
            mat_position$bvec[j,'max'] <- pos_range[j,'min']
          }else{
            mat_position$bvec[j,'min'] <- pos_range[j,'max']
            mat_position$bvec[j,'max'] <- pos_range[j,'max']
          }
          
          warnmessage <- paste(warnmessage,'position constrain relaxed!\n')
          conflicttag <- 0
        }
        
      }
    }
    
    #check group constrain
    if(!is.null(mat_group$Amat)){
      group_range <- t(mat_group$Amat) %*% mat_box$bvec
      for(j in 1:nrow(group_range)){
        if(max(group_range[j,'min'],mat_group$bvec[j,'min'])>min(group_range[j,'max'],mat_group$bvec[j,'max'])){
          warnmessage <- paste(warnmessage,'group:',rownames(group_range)[j],
                               ',bvec_range',brkQT(round(mat_group$bvec[j,],4)),
                               ',group_range',brkQT(round(group_range[j,],4)),
                               'constrain unqualified!\n')
          conflicttag <- conflicttag+1
          
          if(type=='auto'){
            if(mat_group$bvec[j,'max']<group_range[j,'min']){
              mat_group$bvec[j,'min'] <- group_range[j,'min']
              mat_group$bvec[j,'max'] <- group_range[j,'min']
            }else{
              mat_group$bvec[j,'min'] <- group_range[j,'max']
              mat_group$bvec[j,'max'] <- group_range[j,'max']
            }
            
            warnmessage <- paste(warnmessage,'group:',rownames(group_range)[j],'constrain relaxed!\n')
            conflicttag <- 0
          }
        }
      }
    }
    
    #check fctExp_sector constrain
    if(!is.null(mat_fctExp_sector$Amat)){
      sector_range <- t(mat_fctExp_sector$Amat) %*% mat_box$bvec
      for(j in 1:nrow(sector_range)){
        if(max(sector_range[j,'min'],mat_fctExp_sector$bvec[j,'min'])>min(sector_range[j,'max'],mat_fctExp_sector$bvec[j,'max'])){
          warnmessage <- paste(warnmessage,'sector:',rownames(sector_range)[j],
                               ",bvec_range",brkQT(round(mat_fctExp_sector$bvec[j,],6)),
                               ',sector_range',brkQT(round(sector_range[j,],6)),
                               'constrain unqualified!\n')
          conflicttag <- conflicttag+1
          if(type=='auto'){
            if(mat_fctExp_sector$bvec[j,'max']<sector_range[j,'min']){
              mat_fctExp_sector$bvec[j,'min'] <- sector_range[j,'min']
              mat_fctExp_sector$bvec[j,'max'] <- sector_range[j,'min']
            }else{
              mat_fctExp_sector$bvec[j,'min'] <- sector_range[j,'max']
              mat_fctExp_sector$bvec[j,'max'] <- sector_range[j,'max']
            }

            warnmessage <- paste(warnmessage,'sector:',rownames(sector_range)[j],'constrain relaxed!\n')
            conflicttag <- 0
          }
          
        }
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
    warning(paste(date,' conflict number:',conflicttag,'.\n',warnmessage))
    conflicttag <- TRUE
  }else{
    if(nchar(warnmessage)>0){
      warning(paste(date,'\n',warnmessage))
    }
    conflicttag <- FALSE
  }
  mat_vec_list <- list(mat_group=mat_group,
                       mat_box=mat_box,
                       mat_position=mat_position,
                       mat_fctExp_sector=mat_fctExp_sector,
                       mat_fctExp_style=mat_fctExp_style)
  return(list(conflicttag=conflicttag,mat_vec_list=mat_vec_list))
}


add_constr_data <- function(TSF,constr){
  # 1.add sector constrain factors
  sectorAttr <- unique(constr$fctExp_sector[,"sectorAttr"])
  if(dim(sectorAttr)[1]>0){
    for(i in 1:dim(sectorAttr)[1]){
      sectorAttr_ <- sectorAttr[[i,1]]
      TSF <- gf_sector(TSF,sectorAttr_)
      TSF <- dplyr::select(TSF,-sector)
    }
  }
  # 2.add style-constrain factors
  fnames <- guess_factorNames(TSF,silence = TRUE)
  fctlists <- unique(constr$fctExp_style[,"factorlist"])
  if(dim(fctlists)[1]>0){
    FactorLists <- fctlists$factorlist
    diff.fnames <- setdiff(sapply(FactorLists,'[[','factorName'),fnames)
    common.fnames <- intersect(sapply(FactorLists,'[[','factorName'),fnames)
    if(nrow(TSF)>nrow(TSF) || length(diff.fnames)>0){
      if(nrow(TSF)>nrow(TSF) && length(common.fnames)>0){
        TSF[,common.fnames] <- NULL
        diff.fnames <- c(diff.fnames,common.fnames)
      }
      if(length(diff.fnames)>0) FactorLists <- FactorLists[sapply(FactorLists,function(x) x$factorName %in% diff.fnames)]
      
      TSFconstr <- getMultiFactor(TSF[,c('date','stockID')],FactorLists,silence = TRUE)
      TSF <- dplyr::left_join(TSF,TSFconstr,by=c('date','stockID'))
    }
  }
  # 3.add group-constrain sector factors
  groupIDs <- unique(constr$group$ID)
  groupIDs <- setdiff(groupIDs,colnames(TSF))
  if(length(groupIDs)>0){
    for(i in 1:length(groupIDs)){
      TSF <- is_component(TS=TSF,sectorID = groupIDs[i])
      TSF <- renameCol(TSF,"is_comp",groupIDs[i])
    }
  }
  # 4.add sector-specified box-constrain sector factors
  sectorIDs <- unique(constr$box$ID)
  sectorIDs <- sectorIDs[substr(sectorIDs,1,2) %in% c("EI","ES")]
  sectorIDs <- setdiff(sectorIDs,colnames(TSF))
  if(length(sectorIDs)>0){
    for(i in 1:length(sectorIDs)){
      TSF <- is_component(TS=TSF,sectorID = sectorIDs[i])
      TSF <- renameCol(TSF,"is_comp",sectorIDs[i])
    }
  }
  return(TSF)
}


## FFV 
con_box_FFV <- function(TS,TW_ratio,bmk){
  bmk_data <- getIndexCompWgt(indexID = bmk, endT = TS$date[1])
  bmk_data <- gf_cap(bmk_data, var = "free_cap", varname = "free_cap")
  TW <- sum(bmk_data$free_cap, na.rm = TRUE)
  TS_ffv <- gf_cap(TS[,c('date','stockID')], var = "free_cap", varname = "ffv")
  TS_ffv$max <- TS_ffv$ffv/(TW/TW_ratio)
  TS_ffv$min <- 0
  TS_ffv$relative <- 0
  TS_ffv$priority <- NA
  TS_ffv <- dplyr::rename(TS_ffv, ID = stockID)
  constr_box_FFV <- TS_ffv[,c("ID","min","max","relative","priority")]
  return(constr_box_FFV)
}

## suspend & price_limit
wgt_sus_lim <- function(TS,init_port,TSF2,constr_sus_lim){
  flag_sus <- "sus" %in% constr_sus_lim$ID
  flag_lim_up <- "lim_up" %in% constr_sus_lim$ID
  flag_lim_low <- "lim_low" %in% constr_sus_lim$ID
  
  univ_lim_low <- is_priceLimit(TS, nearby = 1,priceType = "close",lim = c(-10,Inf),drop = TRUE)
  univ_lim_up <- is_priceLimit(TS, nearby = 1,priceType = "close",lim = c(-Inf,10),drop = TRUE)
  univ_sus <- is_suspend(TS,nearby = 1,drop = TRUE)
  
  wgt_sus_lim <- data.frame(stringsAsFactors = FALSE)
  if(any(univ_sus) && flag_sus){ # -(1)sus: could not sell nor buy
    box_sus <- data.frame(stockID=TS[univ_sus,"stockID"], type="sus",stringsAsFactors = FALSE)
    wgt_sus_lim <- rbind(wgt_sus_lim, box_sus)
  }
  if(any(univ_lim_up) && flag_lim_up){ # -(2)lim_up: could only sell
    box_lim_up <- data.frame(stockID=TS[univ_lim_up,"stockID"], type="lim_up",stringsAsFactors = FALSE)
    wgt_sus_lim <- rbind(wgt_sus_lim, box_lim_up)
  }
  if(any(univ_lim_low) && flag_lim_low){ # -(3)lim_low: could only buy
    box_lim_low <- data.frame(stockID=TS[univ_lim_low,"stockID"],type="lim_low",stringsAsFactors = FALSE)
    wgt_sus_lim <- rbind(wgt_sus_lim, box_lim_low)
  }
  if(nrow(wgt_sus_lim)==0){
    return(wgt_sus_lim)
  } else {
    # wgt
    if(nrow(init_port)>0){
      wgt_sus_lim <- dplyr::left_join(wgt_sus_lim,init_port[,c('stockID','wgt')],by="stockID")
      wgt_sus_lim <- dplyr::mutate(wgt_sus_lim,wgt=ifelse(is.na(wgt),0,wgt))
    } else {
      wgt_sus_lim$wgt <- 0
    }
    # relative wgt
    wgt_sus_lim <- dplyr::left_join(wgt_sus_lim,TSF2[,c("stockID","wgt_bmk")],by="stockID")
    wgt_sus_lim <- dplyr::mutate(wgt_sus_lim,wgt_rela=wgt-wgt_bmk)
    
    #deal with suspend and pricelimit conflicts,if pricelimit then remove suspend
    wgt_sus_lim <- wgt_sus_lim %>% arrange(stockID,type) %>% 
      group_by(stockID) %>% mutate(n=n()) %>% ungroup() %>% 
      filter(n==1 | (n>1 & type!='sus')) %>% select(-n)
    wgt_sus_lim <- as.data.frame(wgt_sus_lim)
    
    return(wgt_sus_lim)
  }
}

box_bvec_reshape_sus_lim <- function(box_bvec,wgt_sus_lim){
  if(nrow(wgt_sus_lim)==0){
    return(box_bvec)
  }
  bvec <- as.data.frame(box_bvec,stringsAsFactors=FALSE)
  bvec$stockID <- rownames(bvec)
  bvec <- dplyr::left_join(bvec,wgt_sus_lim,by="stockID")
  type_flag <- bvec$type
  type_flag[is.na(type_flag)] <- "normal"
  bvec <- dplyr::mutate(bvec,
                        min=ifelse(type_flag=="sus",wgt_rela,
                                   ifelse(type_flag=="lim_up",pmin(min,wgt_rela),
                                          ifelse(type_flag=="lim_low",pmax(min,wgt_rela),min))),
                        max=ifelse(type_flag=="sus",wgt_rela,
                                   ifelse(type_flag=="lim_up",pmin(max,wgt_rela),
                                          ifelse(type_flag=="lim_low",pmax(max,wgt_rela),max))))
  bvec <- as.matrix(bvec[,c("min","max")])
  rownames(bvec) <- rownames(box_bvec)
  return(bvec)
}


