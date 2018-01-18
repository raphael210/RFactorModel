# ------------- 5 combiFactors_1 ---------
#-GROWTH
#' @export
fls_GROWTH <- function(factorRefine,lcfs=TRUE){
  if(lcfs){
    buildFactorLists_lcfs(factorIDs = c("F000018","F000012","F000019"), 
                          factorRefine=factorRefine)
  } else {
    buildFactorLists(buildFactorList(factorFun = "gf.NP_YOY",factorDir = 1,factorPar=list(src="fin")),
                     buildFactorList(factorFun = "gf.GG_OR_Q", factorDir = 1),
                     buildFactorList(factorFun = "gf.G_OCF", factorDir = 1),
                     factorRefine=factorRefine)
  }
}
#' @export
fl_GROWTH <- function(factorRefine,wgts="eq",
                      comb_factorRefine=refinePar_default("none"),lcfs=TRUE){
  re <- buildFactorList_combi(fls_GROWTH(factorRefine,lcfs=lcfs),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "GROWTH")
  return(re)
}


#-VALUE
#' @export
fls_VALUE <- function(factorRefine,lcfs=TRUE){
  if(lcfs){
    buildFactorLists_lcfs(factorIDs = c("F000006_1","F000007_1","F000010_1"),
                          factorRefine=factorRefine)
  } else {
    buildFactorLists(buildFactorList(factorFun = "gf.ln_PB_mrq", factorDir = -1),
                     buildFactorList(factorFun = "gf.ln_PE_ttm", factorDir = -1),
                     buildFactorList(factorFun = "gf.ln_F_PE", factorDir = -1),
                     factorRefine=factorRefine)
  }
}
#' @export
fl_VALUE <- function(factorRefine,wgts="eq",
                     comb_factorRefine=refinePar_default("none"),lcfs=TRUE){
  re <- buildFactorList_combi(fls_VALUE(factorRefine,lcfs=lcfs),wgts = wgts,
                                        factorRefine=comb_factorRefine,
                                        factorDir = 1,
                                        factorName = "VALUE")
  return(re)
}

#-FORECAST
#' @export
fls_FORECAST <- function(factorRefine,lcfs=TRUE){
  if(lcfs){
    buildFactorLists_lcfs(factorIDs = c("F000003","F000004","F000009"),
                          factorRefine=factorRefine)
  } else {
    buildFactorLists(buildFactorList(factorFun = "gf.F_NP_chg", factorDir = 1,factorPar = 'span="w13"'),
                     buildFactorList(factorFun = "gf.F_target_rtn", factorDir = 1,factorPar = 'con_type="1,2"'),
                     buildFactorList(factorFun = "gf.F_rank_chg", factorDir = 1,factorPar = 'lag=60,con_type="1,2"'),
                     factorRefine=factorRefine)
  }
}
#' @export
fl_FORECAST <- function(factorRefine,wgts="eq",
                        comb_factorRefine=refinePar_default("none"),lcfs=TRUE){
  re <- buildFactorList_combi(fls_FORECAST(factorRefine,lcfs=lcfs),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "FORECAST")
  return(re)
}

#-MARKET
#' @export
fls_MARKET <- function(factorRefine,lcfs=TRUE){
  if(lcfs){
    buildFactorLists_lcfs(factorIDs = c("F000008","F000017","F000024","F000022"),
                          factorRefine=factorRefine)
  } else {
    buildFactorLists(buildFactorList(factorFun = "gf.pct_chg_per", factorDir = -1),
                     buildFactorList(factorFun = "gf.IVR", factorDir = -1),
                     buildFactorList(factorFun = "gf.ILLIQ", factorDir = 1),
                     buildFactorList(factorFun = "gf.volatility", factorDir = -1),
                     factorRefine=factorRefine)
  }
}
#' @export
fl_MARKET <- function(factorRefine,wgts="eq",
                      comb_factorRefine=refinePar_default("none"),lcfs=TRUE){
  re <- buildFactorList_combi(fls_MARKET(factorRefine,lcfs=lcfs),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "MARKET")
  return(re)
}

#-PROFIT
#' @export
fls_PROFIT <- function(factorRefine,lcfs=FALSE){
  if(lcfs){
    # buildFactorLists_lcfs(factorIDs = c("F000008","F000017","F000024"),
    #                       factorRefine=factorRefine)
  } else {
    buildFactorLists(buildFactorList(factorFun = "gf.ROE_Q", factorDir = 1),
                     buildFactorList(factorFun = "gf.pio_f_score", factorDir = 1),
                     buildFactorList(factorFun = "gf.High3Managers", factorDir = 1),
                     factorRefine=factorRefine)
  }
}
#' @export
fl_PROFIT <- function(factorRefine,wgts="eq",
                      comb_factorRefine=refinePar_default("none"),lcfs=TRUE){
  re <- buildFactorList_combi(fls_PROFIT(factorRefine,lcfs=lcfs),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "PROFIT")
  return(re)
}

#-COMBINED
#' @export
fls_COMB <- function(factorRefine, wgts=list("eq","eq","eq","eq","eq"),lcfs=TRUE){
  re <- buildFactorLists(fl_GROWTH(factorRefine, wgts = wgts[[1]],lcfs = lcfs),
                         fl_VALUE(factorRefine, wgts = wgts[[2]],lcfs = lcfs),
                         fl_FORECAST(factorRefine, wgts = wgts[[3]],lcfs = lcfs),
                         fl_MARKET(factorRefine, wgts = wgts[[4]],lcfs = lcfs),
                         fl_PROFIT(factorRefine, wgts = wgts[[5]],lcfs = FALSE))
  return(re)
}




# ------------- 5 stable Factors ---------
#' @export
fls_5stable <- function(factorRefine,lcfs=TRUE){
  if(lcfs){
    fls_other3 <- buildFactorLists_lcfs(factorIDs = c("F000007_1","F000017","F000018"),factorRefine=factorRefine)
  } else {
    fls_other3 <- buildFactorLists(buildFactorList(factorFun = "gf.ln_PE_ttm", factorDir = -1),
                                   buildFactorList(factorFun = "gf.IVR", factorDir = -1),
                                   buildFactorList(factorFun = "gf.NP_YOY",factorDir = 1,factorPar=list(src="fin")),
                                   factorRefine=factorRefine)
    
  }
  re <- c(list(fl_FORECAST(factorRefine,lcfs = lcfs)),
          list(fl_PROFIT(factorRefine,lcfs = FALSE)),
          fls_other3)
  return(re)
}


# ------------- GroupFactorLists ---------
GroupFactorLists <- function(){
  gfconst <- rbind(tibble::tibble(factorType="SIZE",
                                  groupFun="gf.SIZE",
                                  groupDesc="SIZE",
                                  factorFun=c("gf.ln_mkt_cap","gf.free_float_sharesMV","gf.nl_size"),
                                  factorPar=c("","",""),
                                  factorDir=c(-1,-1,-1),
                                  factorName=c("ln_mkt_cap_","free_float_sharesMV_","nl_size_"),
                                  wgt=c(0.45,0.35,0.2)),
                   tibble::tibble(factorType="GROWTH",
                                  groupFun="gf.GROWTH",
                                  groupDesc="GROWTH",
                                  factorFun=c("gf.NP_YOY","gf.G_EPS_Q","gf.G_MLL_Q","gf.G_OCF","gf.G_scissor_Q"),
                                  factorPar=c("","","","",""),
                                  factorDir=c(1,1,1,1,1),
                                  factorName=c("NP_YOY","G_EPS_Q","G_MLL_Q","G_OCF","G_scissor_Q"),
                                  wgt=c(0.3,0.3,0.15,0.1,0.15)),
                   tibble::tibble(factorType="FORECAST",
                                  groupFun="gf.FORECAST",
                                  groupDesc="FORECAST",
                                  factorFun=c("gf.F_NP_chg","gf.F_target_rtn","gf.F_rank_chg"),
                                  factorPar=c('span="w13",con_type="1,2"','con_type="1,2"','lag=60,con_type="1,2"'),
                                  factorDir=c(1,1,1),
                                  factorName=c("F_NP_chg_w13","F_target_rtn","F_rank_chg_60"),
                                  wgt=c(0.2,0.5,0.3)),
                   tibble::tibble(factorType="TRADING",
                                  groupFun="gf.TRADING",
                                  groupDesc="TRADING",
                                  factorFun=c("gf.liquidity","gf.pct_chg_per","gf.ILLIQ","gf.volatility","gf.beta"),
                                  factorPar=c("","N=60","","",""),
                                  factorDir=c(-1,-1,1,-1,-1),
                                  factorName=c("liquidity_","pct_chg_per_60_","ILLIQ" ,"volatility_" ,"beta_"),
                                  wgt=c(0.2,0.3,0.3,0.1,0.1)),
                   tibble::tibble(factorType="EARNINGYIELD",
                                  groupFun="gf.EARNINGYIELD",
                                  groupDesc="EARNINGYIELD",
                                  factorFun=c("gf.ROE_ttm","gf.ROA_ttm"),
                                  factorPar=c("",""),
                                  factorDir=c(1,1),
                                  factorName=c("ROE_ttm","ROA_ttm"),
                                  wgt=c(0.7,0.3)),
                   tibble::tibble(factorType="VALUE",
                                  groupFun="gf.VALUE",
                                  groupDesc="VALUE",
                                  factorFun=c("gf.PB_mrq","gf.EP_ttm","gf.dividendyield"),
                                  factorPar=c("","",""),
                                  factorDir=c(-1,1,1),
                                  factorName=c("PB_mrq_","EP_ttm","dividendyield"),
                                  wgt=c(0.6,0.2,0.2)),
                   tibble::tibble(factorType="QUALITY",
                                  groupFun="gf.QUALITY",
                                  groupDesc="QUALITY",
                                  factorFun=c("gf.pio_f_score"),
                                  factorPar=c(""),
                                  factorDir=c(1),
                                  factorName=c("pio_f_score"),
                                  wgt=c(1)),
                   tibble::tibble(factorType="OTHER",
                                  groupFun="gf.OTHER",
                                  groupDesc="OTHER",
                                  factorFun=c("gf.momentum","gf.IVR","gf.disposition"),
                                  factorPar=c("","",""),
                                  factorDir=c(1,-1,-1),
                                  factorName=c("momentum","IVR_","disposition_"),
                                  wgt=c(0.2,0.4,0.4)))
  
  return(gfconst)
}



