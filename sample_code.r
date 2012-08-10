# sample_code.R
# Sample code for the Practice Fusion Diabetes Classification Competition.
# This codes provides an example of how to flatten the data set features for
# diagnoses, medications, and labs and computes a basic random forest benchmark
# for a transformed dataset with 2 diagnoses, 5 medications and 3 labs.
#
# Requires the provided SQLite database.
# Requires file sample_code_library.R
# 7-July-2012
# ================================================================================= #

library(RSQLite)
library(randomForest)
# Assumes sample_code_library.R is in current working directory
source(paste(getwd(), "/sample_code_library.R", sep=''))


# ================================================================================= #
# open connections
n <- dbDriver("SQLite", max.con=25)
con <- dbConnect(n, dbname="compData.db")


# ================================================================================= #
# Create dataset with (Ndx, Nmeds, Nlabs) = (2,5,3)
train <- create_flattenedDataset(con, "training", 2, 5, 3)
test <- create_flattenedDataset(con, "test", 2, 5, 3)


# ================================================================================= #
# Benchmark vanilla random forest
#rf <- randomForest(train[,3:ncol(train)], train$dmIndicator)
#rf_result <- predict(rf, test[,2:ncol(test)], type="response")

GBM_NTREES = 500
#GBM_SHRINKAGE = 0.05
GBM_SHRINKAGE = 0.01
GBM_DEPTH = 4
GBM_MINOBS = 50

#build the GBM model
library(gbm)
GBM_model <- gbm.fit(
  x = train[,-c(1,2)]
  ,y = train$dmIndicator
  ,distribution = "gaussian"
  ,n.trees = GBM_NTREES
  ,shrinkage = GBM_SHRINKAGE
  ,interaction.depth = GBM_DEPTH
  ,n.minobsinnode = GBM_MINOBS
  ,verbose = TRUE)

summary(GBM_model,GBM_NTREES)
#predict for the leaderboard data
rf_result <- predict.gbm(object = GBM_model
                        ,newdata = test[,-1]
                          ,GBM_NTREES)

myPred <- data.frame(test$PatientGuid, rf_result)
colnames(myPred) <- c("PatientGuid", "dmIndicator")
myPred$dmIndicator[myPred$dmIndicator < 0] = 0
write.table(myPred[order(myPred$PatientGuid),], "sample.csv", sep=',', row.names=FALSE, quote=TRUE, col.names=FALSE)