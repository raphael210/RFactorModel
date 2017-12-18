# ------------- 5 combiFactors_1 ---------
#-GROWTH
#' @export
fls_GROWTH <- function(factorRefine){
  buildFactorLists_lcfs(factorIDs = c("F000018","F000012","F000019"), 
                        factorRefine=factorRefine)
}
#' @export
fl_GROWTH <- function(factorRefine,wgts="eq",
                      comb_factorRefine=refinePar_default("none")){
  re <- buildFactorList_combi(fls_GROWTH(factorRefine),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "GROWTH")
  return(re)
}

#-VALUE
#' @export
fls_VALUE <- function(factorRefine){
  buildFactorLists_lcfs(factorIDs = c("F000006_1","F000007_1","F000010_1"),
                                           factorRefine=factorRefine)
}
#' @export
fl_VALUE <- function(factorRefine,wgts="eq",
                     comb_factorRefine=refinePar_default("none")){
  re <- buildFactorList_combi(fls_VALUE(factorRefine),wgts = wgts,
                                        factorRefine=comb_factorRefine,
                                        factorDir = 1,
                                        factorName = "VALUE")
  return(re)
}

#-FORECAST
#' @export
fls_FORECAST <- function(factorRefine){
  buildFactorLists_lcfs(factorIDs = c("F000003","F000004","F000009"),
                        factorRefine=factorRefine)
}
#' @export
fl_FORECAST <- function(factorRefine,wgts="eq",
                        comb_factorRefine=refinePar_default("none")){
  re <- buildFactorList_combi(fls_FORECAST(factorRefine),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "FORECAST")
  return(re)
}

#-MARKET
#' @export
fls_MARKET <- function(factorRefine){
  buildFactorLists_lcfs(factorIDs = c("F000008","F000017"),
                        factorRefine=factorRefine)
}
#' @export
fl_MARKET <- function(factorRefine,wgts="eq",
                      comb_factorRefine=refinePar_default("none")){
  re <- buildFactorList_combi(fls_MARKET(factorRefine),wgts = wgts,
                              factorRefine=comb_factorRefine,
                              factorDir = 1,
                              factorName = "MARKET")
  return(re)
}

#-PROFIT
#' @export
fl_PROFIT <- function(factorRefine){
  buildFactorList(factorFun = "gf.ROE_Q",
                  factorRefine=factorRefine,
                  factorName = "FROFIT")
}


#-COMBINED
#' @export
fls_COMB <- function(factorRefine, wgts=list("eq","eq","eq","eq","eq")){
  re <- buildFactorLists(fl_GROWTH(factorRefine, wgts = wgts[[1]]),
                         fl_VALUE(factorRefine, wgts = wgts[[2]]),
                         fl_FORECAST(factorRefine, wgts = wgts[[3]]),
                         fl_MARKET(factorRefine, wgts = wgts[[4]]),
                         fl_PROFIT(factorRefine, wgts = wgts[[5]]))
  return(re)
}




# ------------- 5 stable Factors ---------
#' @export
fls_5stable <- function(factorRefine){
  fls_other3 <- buildFactorLists_lcfs(factorIDs = c("F000006_1","F000008","F000018"),factorRefine=factorRefine)
  re <- c(list(fl_FORECAST(factorRefine)),
          list(fl_PROFIT(factorRefine)),
          fls_other3)
  return(re)
}


# ------------- GroupFactorLists ---------
#' @export
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



