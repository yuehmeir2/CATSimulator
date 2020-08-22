# source("R/itemResponseTheory.R")

#' Calculate an IRT ability estimate for the given sequence of scores and items.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(item_pars).
#' @param simulation an object defining the test that is being run.
#' @return a list with properties theta and csem.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[1:5,c("PAR_1","PAR_2","PAR_3")])
#'   scores = generateScores(item_pars, -0.4)
#'   abilityEstimate = estimateAbility(item_pars, scores, simulation)
#' @export
estimateAbility <- function (item_pars, scores, simulation) {
  if ((simulation$control$abilityEstimator == "eap") || all(scores == 0) || all(scores == 1)) {
    return(estimateAbility.eap(item_pars, scores, simulation))
  } else if(simulation$control$abilityEstimator == "mle") {
    abilityEstimate = estimateAbility.mle.nr(item_pars, scores, simulation)
    if (is.null(abilityEstimate)) {
      abilityEstimate = estimateAbility.mle.bf(item_pars, scores, simulation)
    }
    return(abilityEstimate)
  } else {
    # Algorithm unknown???
    stop("abilityEstimator not found: ", simulation$control$abilityEstimator)
    return(NULL)
  }
}

#' Calculate an IRT EAP ability estimate for the given sequence of scores and items.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(item_pars).
#' @param simulation an object defining the test that is being run.
#' @return a list with properties theta and csem.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[1:5,c("PAR_1","PAR_2","PAR_3")])
#'   scores = generateScores(item_pars, -0.4)
#'   abilityEstimate = estimateAbility.eap(item_pars, scores, simulation)
#'   abilityEstimate.irtoys = irtoys::eap(resp=scores, ip=list(est=item_pars), qu=irtoys::normal.qu(51, -5.0, 5.0, 0.0, 1.0, scaling=""))
#' @export
estimateAbility.eap <- function (item_pars, scores, simulation) {
  minTheta = ifelse(is.null(simulation$control$minTheta), -5.0, simulation$control$minTheta)
  maxTheta = ifelse(is.null(simulation$control$maxTheta), 5.0, simulation$control$maxTheta)
  nQuad = ifelse(is.null(simulation$control$eapNquad), 51, simulation$control$eapNquad)
  mean = ifelse(is.null(simulation$control$eapMean), 0.0, simulation$control$eapMean)
  var = ifelse(is.null(simulation$control$eapStdDev), 1.0, simulation$control$eapStdDev^2)

  thetaRange = seq(from=minTheta, to=maxTheta, length.out=nQuad)
  assumedDistribution = (1 / sqrt(2 * pi * var)) * exp(-((thetaRange - mean) * (thetaRange - mean)) / (2 * var));

  # Calculate the likelihood that each theta in thetaRange is the student's ability
  scoreProb = scoreProbability.scores(item_pars, thetaRange, scores)
  likelihood = apply(scoreProb, 2, prod)

  # Convert to bayesian likelihood by multiplying against an assumed population distribution
  # The assumed distribution is a normal distribution following the mean & variance parameters
  bayesianLikelihood = likelihood * assumedDistribution;

  # Estimated Theta is the weighted mean of all possible thetas, with likelihood as the weights.
  theta = sum(thetaRange * bayesianLikelihood) / sum(bayesianLikelihood);
  # Estimated Error is the root-mean-square error of all possible thetas and the actual estimated theta, with likelihood as weights.
  csem = sqrt(sum((thetaRange - theta)^2 * bayesianLikelihood) / sum(bayesianLikelihood));

  return(list(
    theta = theta,
    csem = csem,
    abilityEstimator = "eap"
  ))
}

#' Calculate an IRT MLE ability estimate for the given sequence of scores and items.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(item_pars).
#' @param simulation an object defining the test that is being run.
#' @return a list with properties theta and csem.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[1:5,c("PAR_1","PAR_2","PAR_3")])
#'   scores = generateScores(item_pars, -0.4)
#'   abilityEstimate = estimateAbility.mle.bf(item_pars, scores, simulation)
#'   abilityEstimate.irtoys = irtoys::mlebme(resp=scores, ip=list(est=item_pars))
#' @export
estimateAbility.mle.bf <- function (item_pars, scores, simulation) {
  minTheta = ifelse(is.null(simulation$control$minTheta), -5.0, simulation$control$minTheta)
  maxTheta = ifelse(is.null(simulation$control$maxTheta), 5.0, simulation$control$maxTheta)
  iterations = ifelse(is.null(simulation$control$mleBFIterations), 2, simulation$control$mleBFIterations)

  # Initialize the theta range to include the whole min/max range
  gridMinTheta = minTheta
  gridMaxTheta = maxTheta

  # Calculate the theta iteratively, one decimal place at a time
  for (precision in 0.1^(1:iterations)) {
    # Divide the theta range into strips with a width matching the decimal precision
    thetaRange = seq(from=gridMinTheta, to=gridMaxTheta, by=precision)

    # Calculate the likelihood that each theta in thetaRange is the student's ability
    scoreProb = scoreProbability.scores(item_pars, thetaRange, scores)
    logLikelihood = apply(scoreProb, 2, function(thetaScoreProb) {
      sum(log(thetaScoreProb))
    })

    # Estimated theta is the strip with the maximum likelihood
    theta = thetaRange[which.max(logLikelihood)]

    # Narrow the theta range in preparation for the next iteration
    gridMinTheta = max(minTheta, theta - precision + (precision * 0.1))
    gridMaxTheta = min(maxTheta, theta + precision - (precision * 0.1))
  }

  itemInf = itemInformation(item_pars, theta)
  return(list(
    theta = theta,
    csem = 1.0 / sqrt(sum(itemInf)),
    abilityEstimator = "mle.bf"
  ))
}

#' Calculate an IRT MLE ability estimate for the given sequence of scores and items.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(item_pars).
#' @param simulation an object defining the test that is being run.
#' @return a list with properties theta and csem.
#' @examples
#'   simulation = readRDS(system.file("example/passage-adaptive-wpm.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[1:5,c("PAR_1","PAR_2","PAR_3")])
#'   scores = generateScores(item_pars, -0.4)
#'   abilityEstimate = estimateAbility.mle.nr(item_pars, scores, simulation)
#'   abilityEstimate.irtoys = irtoys::mlebme(resp=scores, ip=list(est=item_pars))
#' @export
estimateAbility.mle.nr <- function (item_pars, scores, simulation) {
  minTheta = ifelse(is.null(simulation$control$minTheta), -5.0, simulation$control$minTheta)
  maxTheta = ifelse(is.null(simulation$control$maxTheta), 5.0, simulation$control$maxTheta)
  maxIterations = ifelse(is.null(simulation$control$mleNRIterations), 30, simulation$control$mleNRIterations)
  targetDelta = ifelse(is.null(simulation$control$mleNRDelta), 0.01, simulation$control$mleNRDelta)

  # Initialize
  iterations = 0
  theta = 0.0

  repeat {
    scoreProb = scoreProbability.theta(item_pars, theta)[,2]
    scoreProbPrime = (scores - scoreProb) * (item_pars[,1] / (1.0 - item_pars[,3])) * (scoreProb - item_pars[,3]) / scoreProb
    itemInf = itemInformation(item_pars, theta)

    tnum = sum(scoreProbPrime)
    tdem = sum(-itemInf)
    if (is.nan(tnum) || is.nan(tdem) || (tdem == 0.0)) {
      # Failed to converge
      return(NULL)
    }

    theta = theta - (tnum / tdem)
    if ((theta < (minTheta - 1.0)) || (theta > (maxTheta + 1.0))) {
      # Failed to converge
      return(NULL)
    }

    iterations = iterations + 1
    delta = abs(tnum / tdem)
    if ((iterations >= maxIterations) || (delta < targetDelta)) {
      # Stop once delta target or max iterations are reached
      break
    }
  }
  if (is.nan(theta) || (theta < minTheta) || (theta > maxTheta) || ((iterations >= maxIterations) && (delta > targetDelta))) {
    # Failed to converge
    return(NULL)
  }

  itemInf = itemInformation(item_pars, theta)
  return(list(
    theta = theta,
    csem = 1.0 / sqrt(sum(itemInf)),
    abilityEstimator = "mle.nr"
  ))
}
