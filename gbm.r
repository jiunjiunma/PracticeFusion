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
# Assumes sample_code_library.R is in current working directory
source(paste(getwd(), "/sample_code_library.R", sep=''))
source(paste(getwd(), "/my_library.R", sep=''))

# ================================================================================= #
# open connections
db_driver <- dbDriver("SQLite", max.con=25)
con <- dbConnect(db_driver, dbname="compData.db")


# ================================================================================= #
# Create dataset with (Ndx, Nmeds, Nlabs) = (2,5,3)
train <- create_flattenedDataset(con, "training", 20, 3, 3)
test <- create_flattenedDataset(con, "test", 20, 3, 3)


GBM_NTREES = 500
#GBM_SHRINKAGE = 0.05
GBM_SHRINKAGE = 0.01
GBM_DEPTH = 4
GBM_MINOBS = 50
GBM_model <- NA
       
predictor <- function(training_set, test_set) {
  #build the GBM model
  library(gbm)
  GBM_model <<- gbm.fit(
    x = training_set[,-c(1,2)]
    ,y = training_set$dmIndicator
    ,distribution = "gaussian"
    ,n.trees = GBM_NTREES
    ,shrinkage = GBM_SHRINKAGE
    ,interaction.depth = GBM_DEPTH
    ,n.minobsinnode = GBM_MINOBS
    ,verbose = TRUE)
  
  summary(GBM_model,GBM_NTREES)
  #predict for the leaderboard data
  rf_result <- predict.gbm(object = GBM_model
                           ,newdata = test_set[,!(names(test_set) %in% c("PatientGuid","dmIndicator"))]
                           ,GBM_NTREES)
  
  myPred <- data.frame(test_set$PatientGuid, rf_result)
  colnames(myPred) <- c("PatientGuid", "dmIndicator")
  myPred$dmIndicator[myPred$dmIndicator < 0] = 0.0000001
  myPred
}
       
validator <- function(validation, prediction) {
  logloss(validation$dmIndicator, prediction$dmIndicator)  
}  
       
ave_log_loss <- cross_validation(predictor, validator, train)

cat(ave_log_loss)
cat("\n")

predictor(train,test)

dbDisconnect(con)
