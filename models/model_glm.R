#
# LOAD DATA
#
load("./data/01_GS_prepro_tv_imputation.Robj")

#
# GLM MODEL
#
model_glm <- glm(RiskPerformance ~ ., train, family = "binomial")

pred_fico_glm <- predict(model_glm, test, type = "response")
pred_fico_glm2 <- predict(model_glm, train, type = "response")

# How good is this model?
mltools::auc_roc(pred_fico_glm, test$RiskPerformance == "Good")
# 0.7875445
mltools::auc_roc(pred_fico_glm2, train$RiskPerformance == "Good")
# 0.804036




####################################################
# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18362)
# 
# Matrix products: default
# 
# Random number generation:
#   RNG:     Mersenne-Twister 
# Normal:  Inversion 
# Sample:  Rounding 
# 
# locale:
#   [1] LC_COLLATE=Polish_Poland.1250  LC_CTYPE=Polish_Poland.1250    LC_MONETARY=Polish_Poland.1250
# [4] LC_NUMERIC=C                   LC_TIME=Polish_Poland.1250    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] gbm_2.1.5         rms_5.1-3.1       SparseM_1.77      Hmisc_4.2-0       Formula_1.2-3    
# [6] survival_2.44-1.1 lattice_0.20-38   ggrepel_0.8.1     ggplot2_3.2.1     openxlsx_4.1.0.1 
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.2          mvtnorm_1.0-10      zoo_1.8-6           assertthat_0.2.1   
# [5] digest_0.6.22       packrat_0.5.0       R6_2.4.0            backports_1.1.5    
# [9] acepack_1.4.1       MatrixModels_0.4-1  pillar_1.4.2        mltools_0.3.5      
# [13] rlang_0.4.1         lazyeval_0.2.2      multcomp_1.4-10     rstudioapi_0.10    
# [17] data.table_1.12.2   rpart_4.1-15        Matrix_1.2-17       checkmate_1.9.3    
# [21] labeling_0.3        splines_3.6.1       stringr_1.4.0       foreign_0.8-71     
# [25] htmlwidgets_1.3     munsell_0.5.0       compiler_3.6.1      xfun_0.6           
# [29] pkgconfig_2.0.3     base64enc_0.1-3     htmltools_0.3.6     nnet_7.3-12        
# [33] tidyselect_0.2.5    tibble_2.1.3        gridExtra_2.3       htmlTable_1.13.1   
# [37] codetools_0.2-16    crayon_1.3.4        dplyr_0.8.3         withr_2.1.2        
# [41] MASS_7.3-51.4       grid_3.6.1          nlme_3.1-140        polspline_1.1.14   
# [45] gtable_0.3.0        magrittr_1.5        scales_1.0.0        zip_2.0.4          
# [49] stringi_1.4.3       latticeExtra_0.6-28 sandwich_2.5-1      TH.data_1.0-10     
# [53] RColorBrewer_1.1-2  tools_3.6.1         glue_1.3.1          purrr_0.3.2        
# [57] colorspace_1.4-1    cluster_2.1.0       DALEX_0.4.9         knitr_1.22         
# [61] quantreg_5.51 
