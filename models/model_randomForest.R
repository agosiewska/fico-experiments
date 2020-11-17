#
# LOAD DATA
#
load("./data/01_GS_prepro_tv_imputation.Robj")

#
# GLMNET MODEL
#
library(mlr)
set.seed(123)

task = makeClassifTask(data = train, 
                       target = "RiskPerformance",
                       positive  = "Bad") 
lrn <- makeLearner("classif.randomForest", predict.type = "prob")

ps = makeParamSet(
  makeIntegerParam("ntree", lower = 100, upper = 5000),
  makeIntegerParam("mtry", lower = 3, upper = 20)
)
ctrl = makeTuneControlRandom(maxit = 100L)
rdesc = makeResampleDesc("CV", iters = 3L)

res = tuneParams(lrn, task = task, resampling = rdesc, 
                 control = ctrl, par.set = ps, measures=auc)

lrn_best = setHyperPars(makeLearner("classif.randomForest", predict.type = "prob"), 
                        ntree = res$x$ntree,
                        mtry = res$x$mtry)
model_randomForest = train(lrn_best, task)


pred_fico_randomForest <-  getPredictionProbabilities(predict(model_randomForest, newdata= test))
pred_fico_randomForest2 <- getPredictionProbabilities(predict(model_randomForest, newdata = train))

# How good is this model?
mltools::auc_roc(pred_fico_randomForest, test$RiskPerformance == "Bad")
# 0.7669052
mltools::auc_roc(pred_fico_randomForest2, train$RiskPerformance == "Bad")
# 0.9488953


res$x$ntree
# 296
res$x$mtry
# 3

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
#   [1] parallelMap_1.3     Rcpp_1.0.2          pillar_1.4.2        compiler_3.6.1      tools_3.6.1        
# [6] packrat_0.5.0       tibble_2.1.3        gtable_0.3.0        checkmate_1.9.3     lattice_0.20-38    
# [11] pkgconfig_2.0.3     rlang_0.4.1         Matrix_1.2-17       fastmatch_1.1-0     rstudioapi_0.10    
# [16] parallel_3.6.1      dplyr_0.8.3         mltools_0.3.5       grid_3.6.1          tidyselect_0.2.5   
# [21] glue_1.3.1          data.table_1.12.2   R6_2.4.0            XML_3.98-1.20       survival_2.44-1.1  
# [26] ggplot2_3.2.1       purrr_0.3.2         magrittr_1.5        backports_1.1.5     scales_1.0.0       
# [31] BBmisc_1.11         splines_3.6.1       assertthat_0.2.1    randomForest_4.6-14 colorspace_1.4-1   
# [36] stringi_1.4.3       lazyeval_0.2.2      munsell_0.5.0       crayon_1.3.4   