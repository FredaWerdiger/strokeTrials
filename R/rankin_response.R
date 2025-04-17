#########################################
# GENERATING RESPONSE RATES FROM mRS DATA
##########################################

getOutcomeandTreatmentVectors <- function(ctrl.vec, treat.vec){
  num.outcomes = length(treat.vec)
  num.participant = sum(treat.vec) + sum(ctrl.vec)
  mRS = NULL
  treat = NULL
  for (i in 1:length(treat.vec)){
    mRS = c(mRS, rep(i, treat.vec[i]), rep(i, ctrl.vec[i]))
    treat = c(treat, rep('treatment',treat.vec[i]),
              rep('control', ctrl.vec[i]))
  }
  return(data.frame(mRS=mRS, treat=treat))
}

getGenOddsEstimator <-function(df){
  n = length(df$mRS)
  x = genodds::genodds(df$mRS, df$treat)
  results =  x$results$`All data`
  genodds = log(results$odds)
  conf.int = log(results$conf.int)
  se = (conf.int[2] - conf.int[1]) / 3.92
  var = (se^2)*n
  return(list(mean=genodds, variance=var, sample.size=n))
}

getProbabilitiesFromOrdinalData <- function(df){
  prop.mrs = prop.table(table(df$mRS, df$treat), margin=2)
  return(list(control.resp = prop.mrs[,1], treat.resp = prop.mrs[,2]))
}
