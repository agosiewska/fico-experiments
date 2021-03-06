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
lrn <- makeLearner("classif.ranger", predict.type = "prob")

ps = makeParamSet(
  makeIntegerParam("num.trees", lower = 100, upper = 5000),
  makeIntegerParam("mtry", lower = 3, upper = 20)
)
ctrl = makeTuneControlRandom(maxit = 100L)
rdesc = makeResampleDesc("CV", iters = 3L)

res = tuneParams(lrn, task = task, resampling = rdesc, 
                 control = ctrl, par.set = ps, measures=auc)

lrn_best = setHyperPars(makeLearner("classif.ranger", predict.type = "prob"), 
                        num.trees = res$x$num.trees,
                        mtry = res$x$mtry)
model_ranger = train(lrn_best, task)


pred_fico_ranger <-  getPredictionProbabilities(predict(model_ranger, newdata= test))
pred_fico_ranger2 <- getPredictionProbabilities(predict(model_ranger, newdata = train))

# How good is this model?
mltools::auc_roc(pred_fico_ranger, test$RiskPerformance == "Bad")
# 0.7859916
mltools::auc_roc(pred_fico_ranger2, train$RiskPerformance == "Bad")
# 0.9838462

res$x$num.trees
# 1425
res$x$mtry
# 3

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
#   [1] parallelMap_1.3   Rcpp_1.0.2        pillar_1.4.2      compiler_3.6.1    iterators_1.0.10  tools_3.6.1      
# [7] packrat_0.5.0     tibble_2.1.3      gtable_0.3.0      checkmate_1.9.3   lattice_0.20-38   pkgconfig_2.0.3  
# [13] rlang_0.4.1       Matrix_1.2-17     fastmatch_1.1-0   foreach_1.4.4     rstudioapi_0.10   parallel_3.6.1   
# [19] ranger_0.11.2     dplyr_0.8.3       mltools_0.3.5     grid_3.6.1        glmnet_2.0-18     tidyselect_0.2.5 
# [25] glue_1.3.1        data.table_1.12.2 R6_2.4.0          XML_3.98-1.20     survival_2.44-1.1 ggplot2_3.2.1    
# [31] purrr_0.3.2       magrittr_1.5      codetools_0.2-16  backports_1.1.5   scales_1.0.0      BBmisc_1.11      
# [37] splines_3.6.1     assertthat_0.2.1  colorspace_1.4-1  stringi_1.4.3     lazyeval_0.2.2    munsell_0.5.0    
# [43] crayon_1.3.4 


