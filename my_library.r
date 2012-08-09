logloss <- function(results, predictions) { 
  -mean(results*log(predictions) + (1-results)*log(1-predictions))
}

get_first_value <- function(items, default=NA) {

  for (i in 1:length(items)) {
    if (!is.na(items[i])) {
      return (items[i])
    } 
  }
  return (default)
}

cross_validation <- function(predictor, validator, training_set, results, n=10) {
  number_of_fold = n
  eval_indices <- sample(seq(1:length(training_set)), length(training_set))
  batch_size <- floor(training_set/number_of_fold)
  total_eval_error <- 0.0
  
  for (i in 1:number_of_fold) {
    eval_sample_indices <- eval_indices[(i-1)*batch_size+1:(i*batch_size)]
    eval_test_set <- training_set[-eval_sample_indices,]
    eval_results <- result_set[-eval_sample_indices]
    eval_predictions <- predictor(eval_test_set, ...)
    eval_error <- validator(eval_results, eval_predictions)
    total_eval_error <- total_eval_error + eval_error
  } 
  
  total_eval_error/number_of_fold
}