logloss <- function(results, predictions) { 
  -mean(results*log(predictions) + (1-results)*log(1-predictions))
}

cross_validation <- function(predictor, validator, training_set, n=10) {
  number_of_fold = n
  eval_indices <- sample(seq(1:nrow(training_set)), nrow(training_set))
  batch_size <- floor(nrow(training_set)/number_of_fold)
  total_eval_error <- 0.0
  
  for (i in 1:number_of_fold) {
    eval_sample_indices <- eval_indices[((i-1)*batch_size+1):(i*batch_size)]
    eval_sample_set <- training_set[-eval_sample_indices,]
    eval_validation_set <- training_set[eval_sample_indices,]
    eval_predictions <- predictor(eval_sample_set, eval_validation_set)
    eval_error <- validator(eval_validation_set, eval_predictions)
    total_eval_error <- total_eval_error + eval_error
  } 
  
  total_eval_error/number_of_fold
}