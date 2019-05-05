#main function



check_prob=function(prob){
  if (prob<0 | prob>1){
    stop('invalid prob value')
  }
  TRUE
}

#To check the validility of trials
check_trials=function(trials){
  if (trials<0){
    stop('invalid trials value')
  }
  if (all((trials%%1)!=0)){
    stop('invalid trials value')
  }
  TRUE
}

#test if an input success is a valid value for number of successes (i.e. 0 ¡Ü k ¡Ü n)
check_success=function(success,trials){
  if (all(trials<success)){
    stop('success cannot be greater than trials')
  }
  if (all((success%%1)!=0 & success<0)){
    stop('invalid success value' )
  }
  TRUE
}

aux_mean = function(trials,prob){
  return(trials*prob)
}

aux_variance = function(trials,prob){
  return(trials*prob*(1-prob))
}


aux_mode = function(trials,prob){
  md = trials*prob + prob
  if (md%/%1 == md){
    return(c(md,md-1))
  } else {
    return(md%/%1)
  }
}
aux_skewness = function(trials,prob){
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}


aux_kurtosis = function(trials,prob){
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}

#' @title : bin_choose
#' @description :calculates the number of combinations in which k successes can occur in n trials
#' @param :k&n
#' @return  : the number of combinations


bin_choose=function(n,k){
  if (all(k>rep(n,times=length(k)))){
    stop("k cannot be greater than n")
  }
  
  factorial(n)/(factorial(k)*factorial(n-k))
}


#' @title : bin_probability
#' @description :calculates the probability of getting the given number of successes in given number of trials
#' @param :success, trials, and prob
#' @return  : the probability


bin_probability=function(success, trials, prob){
  check_prob(prob)
  check_trials(trials)
  check_success(success,trials)
  pro=bin_choose(trials,success)*prob^success*(1-prob)^(trials-success)
  pro
}



#' @title : bin_distribution
#' @description :produce the table of the distribution of probability as getting the given number of trials
#' @param :trials,prob
#' @return  : the data frame 


bin_distribution=function(trials,prob){
  success_col=0:trials
  pro_col=bin_probability(success = 0:trials, trials, prob)
  dat=data.frame("success"=success_col,
             "probability"=pro_col)
  class(dat)=c("bindis","data.frame")
  return(dat)
}


#' @export:a barplot to display the probability histogram of a binomial distribution object "bindis"
plot.bindis=function(x){
  barplot(x$probability,xlab="success",ylab="probability",border=NA)
}

# plotting binomial probability distribution
dis1 <- bin_distribution(trials = 5, prob = 0.5)
plot(dis1)

#' @title :bin_cumulative
#' @description :produce the table of the distribution of probability and culmulative probability as getting the given number of trials
#' @param :trials,prob
#' @return  : the data frame 

bin_cumulative=function(trials,prob){
  success_col=0:trials
  pro_col=bin_probability(success = 0:trials, trials, prob)
  cul_col=c()
  cul_col[1]=pro_col[1]
  for (i in 2:(trials+1)){
  cul_col[i]=pro_col[i]+cul_col[i-1]
  }
  dat=data.frame("success"=success_col,
                 "probability"=pro_col,
                 "cumulative"=cul_col)
  class(dat)=c("bincum","data.frame")
  return(dat)
  
} 

# binomial cumulative distribution
bin_cumulative(trials = 5, prob = 0.5)

#' @export
# plotting cumulative binomial probability 
plot.bincum=function(x){
  plot(x$cumulative,xlab="success",ylab="probability",type="o")
} 

# plotting binomial cumulative distribution
dis2 <- bin_cumulative(trials = 5, prob = 0.5)
plot(dis2)

#' @title bin_variable
#' @description a main function that return a binomial random variable object
#' @param trials prob
#' @return  : the data frame 
bin_variable = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  lst=list(trials, prob)
  names(lst)=c("trials", "prob")
  class(lst)="binvar"
  lst
}

#' @export
print.binvar=function(x){
  cat('"Binomial variable"\n\n')
  cat('Paramaters\n')
  cat('- number of trials: ',x$trials)
  cat('\n')
  cat('- prob of success : ',x$prob)
  invisible(x)
}

bin1 <- bin_variable(trials = 10, p = 0.3)
bin1

#' @export
summary.binvar = function(object) {
  sumstat <- list(
    trials = object$trials,
    prob = object$prob,
    mean = aux_mean(object$trials, object$prob),
    variance = aux_variance(object$trials, object$prob),
    mode = aux_mode(object$trials, object$prob),
    skewness = aux_skewness(object$trials, object$prob),
    kurtosis = aux_kurtosis(object$trials, object$prob))
  names(sumstat) = c("trials", "prob", "mean", "variance", "mode", "skewness", "kurtosis")
  class(sumstat) = "summary.binvar"
  sumstat
}

print.summary.binvar = function(x) {
  cat('"Summary Binomial"\n\n')
  cat('Paramaters\n')
  cat('-number of trials:', summary.binvar(x)$trials, '\n')
  cat('-prob of success:', summary.binvar(x)$prob, '\n\n')
  cat('Measures', '\n')
  cat('-mean    :', summary.binvar(x)$mean, '\n')
  cat('-variance:', summary.binvar(x)$variance, '\n')
  cat('-mode    :', summary.binvar(x)$mode, '\n')
  cat('-skewness:', summary.binvar(x)$skewness, '\n')
  cat('-kurtosis:', summary.binvar(x)$kurtosis)
  invisible(x)
}

bin_mean = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  mean = aux_mean(trials, prob)
  mean
}

bin_variance = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  variance = aux_variance(trials, prob)
  variance
}

bin_mode = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  mode = aux_mode(trials, prob)
  mode
}

bin_skewness = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  skewness = aux_skewness(trials, prob)
  skewness
}

bin_kurtosis = function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  kurtosis = aux_kurtosis(trials, prob)
  kurtosis
}


