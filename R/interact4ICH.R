## function to return interact 4 response rates for PRESTO-REACH simulations


getINTERACTcounts <- function(){
  treat.vec <- c(39, 181, 83, 169, 142, 144, 243)
  treat.vec <- (treat.vec * 522)/1000
  ctrl.vec <- c(55, 114, 39, 178, 150, 156, 308)
  ctrl.vec <- (ctrl.vec * 519) /1000
  return(list(control = ctrl.vec, treatment=treat.vec))
}

getInteractResprate <- function(){
  counts = getINTERACTcounts()
  return(getProbabilitiesFromOrdinalData(
    getOutcomeandTreatmentVectors(counts$control, counts$treatment)))
}

getOppositeResprate <- function(){
  counts = getINTERACTcounts()
  return(getProbabilitiesFromOrdinalData(
    getOutcomeandTreatmentVectors(counts$treatment, counts$control)))
}

getSameResprate <- function(){
  counts = getINTERACTcounts()
  return(getProbabilitiesFromOrdinalData(
    getOutcomeandTreatmentVectors(counts$control, counts$control)))
}


# # ischemic
# treat.vec <- c(18.6, 19.6, 7.9, 12.2, 8.8, 10.5, 22.5)
# treat.vec <- treat.vec * 599 / 100
# ctrl.vec <- c(21.8, 19.9, 8.4, 13, 7.9, 11.8, 17.1)
# ctrl.vec <- ctrl.vec * 600 / 100

getInteractConfidenceCurves = function(directory='.'){
  counts = getINTERACTcounts()
  odds.ratio = getGenOddsEstimator(
    getOutcomeandTreatmentVectors(counts$control, counts$treatment))
  out.list = confidenceCurves::makeConfidenceCurves(theta.estimator = odds.ratio$mean,
                                  treat.var = odds.ratio$variance,
                                  sample.size = odds.ratio$sample.size,
                                  directory=directory,
                                  show='BENEFIT', pval='TWO-SIDED', min.effect = 0,dir.benefit = 0,
                                  plot=TRUE,
                                  tag="interact4_ICH")
}


