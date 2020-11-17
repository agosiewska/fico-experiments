#
# LOAD DATA
#
load("./data/01_GS_prepro_tv_imputation.Robj")

#
# SVM MODEL
#
library(mlr)
library(e1071)
set.seed(123)

#
# LOAD DATA
#
load("./data/01_GS_prepro_tv_imputation.Robj")

task = makeClassifTask(data = train, 
                       target = "RiskPerformance",
                       positive  = "Bad") 
lrn_svm <- makeLearner("classif.svm", predict.type = "prob")

ps = makeParamSet(
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x)2^x),
  makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x)2^x)
)
ctrl = makeTuneControlRandom(maxit = 100L)
rdesc = makeResampleDesc("CV", iters = 3L)

res = tuneParams(lrn_svm, task = task, resampling = rdesc, 
                 control = ctrl, par.set = ps, measures=auc)

lrn_best = setHyperPars(makeLearner("classif.svm", predict.type = "prob"), 
                        cost = res$x$cost,
                        gamma = res$x$gamma)
model_svm = train(lrn_best, task)


pred_fico_svm <- getPredictionProbabilities(predict(model_svm, newdata =  test, probability=TRUE))
pred_fico_svm2 <- getPredictionProbabilities(predict(model_svm, newdata = train, probability=TRUE))

# How good is this model?
mltools::auc_roc(pred_fico_svm, test$RiskPerformance == "Bad")
#  0.7892957
mltools::auc_roc(pred_fico_svm2, train$RiskPerformance == "Bad")
#  0.8077285


res$x$cost
# 0.2010253
res$x$gamma
# 0.01930817

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
#   [1] e1071_1.7-2       mlr_2.15.0        ParamHelpers_1.12
# 
# loaded via a namespace (and not attached):
#   [1] parallelMap_1.3   Rcpp_1.0.2        pillar_1.4.2      compiler_3.6.1    class_7.3-15      tools_3.6.1      
# [7] packrat_0.5.0     tibble_2.1.3      gtable_0.3.0      checkmate_1.9.3   lattice_0.20-38   pkgconfig_2.0.3  
# [13] rlang_0.4.1       Matrix_1.2-17     fastmatch_1.1-0   rstudioapi_0.10   parallel_3.6.1    ranger_0.11.2    
# [19] dplyr_0.8.3       mltools_0.3.5     grid_3.6.1        tidyselect_0.2.5  glue_1.3.1        data.table_1.12.2
# [25] R6_2.4.0          XML_3.98-1.20     survival_2.44-1.1 kernlab_0.9-27    ggplot2_3.2.1     purrr_0.3.2      
# [31] magrittr_1.5      backports_1.1.5   scales_1.0.0      BBmisc_1.11       splines_3.6.1     assertthat_0.2.1 
# [37] colorspace_1.4-1  stringi_1.4.3     lazyeval_0.2.2    munsell_0.5.0     crayon_1.3.4   