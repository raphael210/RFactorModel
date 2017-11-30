# ------------- 5 combiFactors_1 ---------
#-GROWTH
fls_growth <- function(factorRefine){
  buildFactorLists_lcfs(factorIDs = c("F000018","F000012","F000019"), factorRefine=factorRefine)
}
fl_growth <- function(factorRefine,factorRefine_sub,wgts="eq"){
  re <- buildFactorList_combi(fls_growth(factorRefine_sub),wgts = wgts,
                              factorRefine=factorRefine,
                              factorDir = 1,
                              factorName = "GROWTH")
}

# 
# #-VALUE
# factorLists_value <- buildFactorLists_lcfs(factorIDs = c("F000006_1","F000007_1","F000010_1"),
#                                            factorRefine=factorRefine)
# factor_value <- buildFactorList_combi(factorLists_value,c(1,1,1),
#                                       factorRefine=factorRefine,
#                                       factorDir = 1,
#                                       factorName = "VALUE")
# #-FORECAST
# factorLists_forecast <- buildFactorLists_lcfs(factorIDs = c("F000003","F000004","F000009"),
#                                               factorRefine=factorRefine)
# factor_forecast <- buildFactorList_combi(factorLists_forecast,c(1,1,1),
#                                          factorRefine=factorRefine,
#                                          factorDir = 1,
#                                          factorName = "FORECAST")
# #-MARKET
# factorLists_market <- buildFactorLists_lcfs(factorIDs = c("F000008","F000017"),
#                                             factorRefine=factorRefine)
# factor_market <- buildFactorList_combi(factorLists_market,c(1,1),
#                                        factorRefine=factorRefine,
#                                        factorDir = 1,
#                                        factorName = "MARKET")
# #-PROFIT
# factor_profit <- buildFactorList(factorFun = "gf.ROE_ttm",
#                                  factorRefine=factorRefine,
#                                  factorName = "FROFIT")
# 
# #-combinede
# factorLists_com_1 <- buildFactorLists(factor_growth,factor_value,factor_forecast,factor_market,factor_profit)