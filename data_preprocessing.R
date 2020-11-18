x <- read.table("data/heloc_dataset_v1.csv", sep = ",", header = T)
# str(x)
# head(x)
# table(x$RiskPerformance)

# -9 No Bureau Record or No Investigation
# -8 No Usable/Valid Trades or Inquiries (Anfragen)
# -7 Condition not Met (e.g. No Inquiries, No Delinquencies) 


### Create new dummy column(s) for Missings 
# check whether Missings arte the same (588) for all variables
nx0 <- ncol(x)
NAs <- x[,2] == -9
for(i in 2:nx0) NAs <- (NAs & x[,i] == -9)
# sum(NAs)  

x$NoBureau <- as.factor(as.numeric(NAs))
# for(i in 2:nx0) x[x[,i] == -9,i] <- NA # 
# sapply(x, function(z) sum(is.na(z))) # REM: additional 10 obs with -9 in: ExternalRiskEstimate

for(j in 2:nx0){
  if(any(x[,j] == -8, na.rm = T)){
    x[[paste("NoValid",names(x)[j], sep = "_")]] <- as.factor(as.numeric(x[, j] == -8))
  }
  if(any(x[,j] == -7, na.rm = T)){
    x[[paste("No",names(x)[j], sep = "_")]] <- as.factor(as.numeric(x[, j] == -7))
  }
  x[(-9 <= x[,j]) & (x[,j] <= -7), j] <- NA
}

## remove NoValid_PercentTradesWBalance (only 18)
# summary(x)
# str(x)
x <- x[,-which(names(x) == "NoValid_PercentTradesWBalance")]

## MaxDelq2PublicRecLast12M: Values 0-7 are monotonically decreasing 	
# 0	derogatory (negativ/schadend/absch?tzig) comment
# 1	120+ days delinquent
# ...
# 7	current and never delinquent
# 8, 9	all other
# table(x$MaxDelq2PublicRecLast12M) # no 8; only 6 x 9
x$MaxDelq2PublicRecLast12M[x$MaxDelq2PublicRecLast12M == 9] <- NA

## MaxDelqEver: Values 2-8 are monotonically decreasing	
# 1	No such value
# 2	derogatory comment
# ...
# 8	current and never delinquent
# 9	all other
# ...table(x$MaxDelqEver) # no 1 / no 9 in data ~> nothing to do!



##################
## create TV split

# REM: TV is a vector with fixed to labels 'T' (for training) and 'V' (for independent testing but not parameter tuning) 
#   to be used for all models in order to allow for comparison.

# ...in a subsequent step I' create training and validation data thatb do already contain 
#   1) the additional dummy features (cf. above)
#   2) ...+ imputation (imputation model built on training data and applied to test data, cf. be following lines)

set.seed(42)
TV <- rep("T", nrow(x))
TV[1:round(0.25*nrow(x))] = "V"
TV <- sample(TV) 
table(TV)

train <- x[TV == "T", ]
test  <- x[TV == "V", ]


###################
## imputation

library(mlr)
imp = impute(train, classes = list(integer = imputeMean(), numeric = imputeMean(), factor = imputeMode()))
# str(imp)
# imp$desc

train <- imp$data
test  <- reimpute(test, imp$desc)


#########################################################################
## Save data at this stage
# save(TV, train, test, file = "data/01_GS_prepro_tv_imputation.Robj")

