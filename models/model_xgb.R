#
# LOAD DATA
#
load("./data/01_GS_prepro_tv_imputation.Robj")

#
# XGB MODEL
#
library(mlr)
set.seed(123)

train_dummy <- createDummyFeatures(train, target = "RiskPerformance")
test_dummy <- createDummyFeatures(test, target = "RiskPerformance")

task = makeClassifTask(data = train_dummy, 
                       target = "RiskPerformance",
                       positive  = "Bad") 
lrn <- makeLearner("classif.xgboost", predict.type = "prob")

ps = makeParamSet(
  makeNumericParam("eta", lower = 0, upper = 1),
  makeIntegerParam("max_depth", lower = 3, upper = 10),
  makeIntegerLearnerParam("nrounds", lower = 50, upper = 2000),
  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x)2^x),
  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x)2^x)
)
ctrl = makeTuneControlRandom(maxit = 100L)
rdesc = makeResampleDesc("CV", iters = 3L)

res = tuneParams(lrn, task = task, resampling = rdesc, 
                 control = ctrl, par.set = ps, measures=auc)

lrn_best = setHyperPars(makeLearner("classif.xgboost", predict.type = "prob"), 
                        eta = res$x$eta,
                        max_depth = res$x$max_depth,
                        nrounds = res$x$nrounds,
                        lambda = res$x$lambda,
                        alpha = res$x$alpha)
model_xgb = train(lrn_best, task)


pred_fico_xgb <-  getPredictionProbabilities(predict(model_xgb, newdata= test_dummy))
pred_fico_xgb2 <- getPredictionProbabilities(predict(model_xgb, newdata = train_dummy))

# How good is this model?
mltools::auc_roc(pred_fico_xgb, test$RiskPerformance == "Bad")
# 0.7877087
mltools::auc_roc(pred_fico_xgb2, train$RiskPerformance == "Bad")
# 0.8438657




res$x$eta
# 0.1583868
res$x$max_depth
# 5
res$x$nrounds
# 1515
res$x$lambda
# 185.8683
res$x$alpha
# 14.11926



# 
# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18362)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Polish_Poland.1250  LC_CTYPE=Polish_Poland.1250    LC_MONETARY=Polish_Poland.1250
# [4] LC_NUMERIC=C                   LC_TIME=Polish_Poland.1250    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] mlr_2.15.0        ParamHelpers_1.12
# 
# loaded via a namespace (and not attached):
#   [1] parallelMap_1.3   Rcpp_1.0.2        pillar_1.4.2      compiler_3.6.1    tools_3.6.1       packrat_0.5.0    
# [7] tibble_2.1.3      gtable_0.3.0      checkmate_1.9.3   lattice_0.20-38   pkgconfig_2.0.3   rlang_0.4.1      
# [13] Matrix_1.2-17     fastmatch_1.1-0   rstudioapi_0.10   parallel_3.6.1    ranger_0.11.2     dplyr_0.8.3      
# [19] mltools_0.3.5     xgboost_0.82.1    grid_3.6.1        tidyselect_0.2.5  glue_1.3.1        data.table_1.12.2
# [25] R6_2.4.0          XML_3.98-1.20     survival_2.44-1.1 ggplot2_3.2.1     purrr_0.3.2       magrittr_1.5     
# [31] backports_1.1.5   scales_1.0.0      BBmisc_1.11       splines_3.6.1     assertthat_0.2.1  colorspace_1.4-1 
# [37] stringi_1.4.3     lazyeval_0.2.2    munsell_0.5.0     crayon_1.3.4 

