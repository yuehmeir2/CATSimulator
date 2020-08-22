#' Calculate the probability of the given item/score combinations for a range of thetas
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param thetas a vector of doubles. Various ability levels
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(items).
#' @return a matrix of doubles, items by thetas, containing the probability of the given item/score combinations for a range of thetas
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   thetas = seq(-2, 2, by = 0.5)
#'   scores = rep(0, nrow(item_pars)) # sample(0:1, nrow(item_pars), replace = TRUE)
#'   scoreProb = scoreProbability.scores(item_pars, thetas, scores)
#'   options(digits=8, scipen=8)
#'   > scoreProb     -2       -1.5         -1       -0.5           0         0.5           1         1.5           2
#'   item001 0.58809834 0.50372283 0.41558368 0.32888841 0.249089805 0.180546552 0.125536324 0.084083112 0.054534047
#'   item002 0.59190964 0.51613679 0.43669503 0.35733888 0.282200703 0.214963841 0.158113716 0.112580468 0.077870790
#'   item003 0.94513409 0.91771404 0.87678454 0.81655990 0.730749143 0.615846666 0.477205951 0.333137033 0.208115753
#'   item004 0.83947786 0.79814254 0.74606866 0.68095351 0.600952839 0.506025087 0.399962837 0.291827215 0.194203063
#'   item005 0.86416067 0.82918801 0.78442626 0.72729225 0.655279966 0.567109542 0.464703815 0.355122538 0.250122827
#'   item006 ...391 total items/rows
#' @export
scoreProbability.scores <- function(item_pars, thetas, scores) {
  NC = rowSums(!is.na(item_pars))-2
  itemInd.3pl = which(NC <= 2)
  itemInd.gpc = which(NC > 2)

  # special case for speed: no gpc, all 3pl
  if (length(itemInd.gpc) <= 0) {
    return(scoreProbability.scores.3pl(item_pars, thetas, scores))
  }
  # special case for speed: no 3pl, all gpc
  if (length(itemInd.3pl) <= 0) {
    return(scoreProbability.scores.gpc(item_pars, NC, thetas, scores))
  }

  # general case, there are some of each
  scoreProb = matrix(data = as.double(NA), nrow = nrow(item_pars), ncol = length(thetas))

  # populate the rows with 3pl score probs
  scoreProb.3pl = scoreProbability.scores.3pl(item_pars[itemInd.3pl,,drop=FALSE], thetas, scores[itemInd.3pl])
  scoreProb[itemInd.3pl,] = scoreProb.3pl

  # populate the rows with gpc score probs
  scoreProb.gpc = scoreProbability.scores.gpc(item_pars[itemInd.gpc,,drop=FALSE], NC[itemInd.gpc], thetas, scores[itemInd.gpc])
  scoreProb[itemInd.gpc,] = scoreProb.gpc

  dimnames(scoreProb) = list(rownames(item_pars), thetas)
  return(scoreProb)
}

#' Calculate the probability of the given item/score combinations for a range of thetas
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param thetas a vector of doubles. Various ability levels
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(items).
#' @return a matrix of doubles, items by thetas, containing the probability of the given item/score combinations for a range of thetas
#' @examples
#'   simulation = readRDS(system.file("example/internal/gpc3pl-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   thetas = seq(-2, 2, by = 0.5)
#'   scores = rep(0, nrow(item_pars)) # sample(0:1, nrow(item_pars), replace = TRUE)
#'   scoreProb = scoreProbability.scores.3pl(item_pars, thetas, scores)
#'   options(digits=8, scipen=8)
#'   > scoreProb     -2       -1.5         -1       -0.5          0        0.5          1         1.5           2
#'   item042 0.93258889 0.91498381 0.89330699 0.86690713 0.83518019 0.79765543 0.75410271 0.704646782 0.649862455
#'   item043 0.97800364 0.96830755 0.95453608 0.93518101 0.90837694 0.87200583 0.82399418 0.762870413 0.688542983
#'   item044 0.70350254 0.65577175 0.60467498 0.55118453 0.49648327 0.44186606 0.38861989 0.337905469 0.290662804
#'   item045 0.95313713 0.92332093 0.87698279 0.80845126 0.71418415 0.59666976 0.46690424 0.341467387 0.234881989
#'   item048 0.97620356 0.96403739 0.94599523 0.91965584 0.88207156 0.83015274 0.76155526 0.676063643 0.576947540
#'   item049 ...277 total items/rows
scoreProbability.scores.3pl <- function(item_pars, thetas, scores) {
  score1Prob = t(apply(item_pars, 1, function(item.par) {
    item.par[3] + (1 - item.par[3]) / (1.0 + exp(-item.par[1] * (thetas - item.par[2])))
  }))

  scoreProb = ((1 - scores) * (1 - score1Prob)) + (scores * score1Prob)

  dimnames(scoreProb) = list(rownames(item_pars), thetas)
  return(scoreProb)
}

#' Calculate the probability of the given item/score combinations for a range of thetas
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param NC a vector of integers, one per row/item in item_pars, indicating the number of scoring categories for the item.
#' @param thetas a vector of doubles. Various ability levels
#' @param scores a vector of integers. A score for each item.  Must be same length as nrow(items).
#' @return a matrix of doubles, items by thetas, containing the probability of the given item/score combinations for a range of thetas
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   NC = itempool$NC[itempool$MODEL == "GPC"]
#'   thetas = seq(-2, 2, by = 0.5)
#'   scores = rep(0, nrow(item_pars)) # sample(0:1, nrow(item_pars), replace = TRUE)
#'   scoreProb = scoreProbability.scores.gpc(item_pars, NC, thetas, scores)
#'   options(digits=8, scipen=8)
#'   > scoreProb     -2       -1.5         -1       -0.5           0         0.5           1         1.5           2
#'   item001 0.58809834 0.50372283 0.41558368 0.32888841 0.249089805 0.180546552 0.125536324 0.084083112 0.054534047
#'   item002 0.59190964 0.51613679 0.43669503 0.35733888 0.282200703 0.214963841 0.158113716 0.112580468 0.077870790
#'   item003 0.94513409 0.91771404 0.87678454 0.81655990 0.730749143 0.615846666 0.477205951 0.333137033 0.208115753
#'   item004 0.83947786 0.79814254 0.74606866 0.68095351 0.600952839 0.506025087 0.399962837 0.291827215 0.194203063
#'   item005 0.86416067 0.82918801 0.78442626 0.72729225 0.655279966 0.567109542 0.464703815 0.355122538 0.250122827
#'   item006 ...114 total items/rows
#' @import tidyverse
scoreProbability.scores.gpc <- function(item_pars, NC, thetas, scores) {
  require(tidyverse)

  # map() will use these names, which prevents "New names: NA -> ...1" warning from bind_cols()
  names(thetas) = thetas

  scoresCols = scores+1
  scoreProb <- map(thetas, ~{
    scoreProbability.theta.gpc(item_pars, NC, theta=.x)[cbind(seq_along(scoresCols), scoresCols)]
  }) %>% bind_cols() %>% as.matrix()

  dimnames(scoreProb) = list(rownames(item_pars), thetas)
  return(scoreProb)
}

#' Calculate the probability of each possible score for the given items and a specific theta
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param theta a double. An ability level
#' @return a matrix of doubles, items by scores, containing the probability of each possible score for the given items and a specific theta
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   theta = 2.0
#'   scoreProb = scoreProbability.theta(item_pars, theta)
#'   options(digits=8, scipen=8)
#'   > scoreProb       0           1           2           3           4           5           6
#'   item001 0.054534047 0.279500656 0.665965296          NA          NA          NA          NA
#'   item002 0.077870790 0.307631523 0.614497687          NA          NA          NA          NA
#'   item003 0.208115753 0.278064930 0.513819317          NA          NA          NA          NA
#'   item004 0.194203063 0.176087171 0.197053893 0.242477093 0.190178780          NA          NA
#'   item005 0.250122827 0.155418381 0.181922731 0.242986174 0.169549887          NA          NA
#'   item006 ...391 total items/rows
#' @export
scoreProbability.theta <- function(item_pars, theta) {
  NC = rowSums(!is.na(item_pars))-2
  itemInd.3pl = which(NC <= 2)
  itemInd.gpc = which(NC > 2)

  # special case for speed: no gpc, all 3pl
  if (length(itemInd.gpc) <= 0) {
    return(scoreProbability.theta.3pl(item_pars, theta))
  }
  # special case for speed: no 3pl, all gpc
  if (length(itemInd.3pl) <= 0) {
    return(scoreProbability.theta.gpc(item_pars, NC, theta))
  }

  # general case, there are some of each
  scoreProb = matrix(data = as.double(NA), nrow = nrow(item_pars), ncol = max(2, ncol(item_pars)-2))

  # populate the rows with 3pl score probs
  scoreProb.3pl = scoreProbability.theta.3pl(item_pars[itemInd.3pl,,drop=FALSE], theta)
  scoreProb[itemInd.3pl, 1:ncol(scoreProb.3pl)] = scoreProb.3pl

  # populate the rows with gpc score probs
  scoreProb.gpc = scoreProbability.theta.gpc(item_pars[itemInd.gpc,,drop=FALSE], NC[itemInd.gpc], theta)
  scoreProb[itemInd.gpc, 1:ncol(scoreProb.gpc)] = scoreProb.gpc

  dimnames(scoreProb) = list(rownames(item_pars), colnames(scoreProb.gpc))
  return(scoreProb)
}

#' Calculate the probability of each possible score for the given 3PL items and a specific theta
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param theta a double. An ability level
#' @return a matrix of doubles, items by scores, containing the probability of each possible score for the given items and a specific theta
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   theta = 2.0
#'   scoreProb = scoreProbability.theta.3pl(item_pars, theta)
#'   options(digits=8, scipen=8)
#'   > scoreProb       0          1
#'   item042 0.649862455 0.35013754
#'   item043 0.688542983 0.31145702
#'   item044 0.290662804 0.70933720
#'   item045 0.234881989 0.76511801
#'   item048 0.576947540 0.42305246
#'   item049 ...277 total items/rows
scoreProbability.theta.3pl <- function(item_pars, theta) {
  scoreProb = matrix(data = as.double(NA), nrow = nrow(item_pars), ncol = 2)

  # process column by column, calculating the "correct" scoreProb for all items first, and then "incorrect" next
  scoreProb[,2] = item_pars[,3] + (1 - item_pars[,3]) / (1.0 + exp(-item_pars[,1] * (theta - item_pars[,2])))
  scoreProb[,1] = 1 - scoreProb[,2]

  dimnames(scoreProb) = list(rownames(item_pars), 0:1)
  return(scoreProb)
}

#' Calculate the probability of each possible score for the given GPC items and a specific theta
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param NC a vector of integers, one per row/item in item_pars, indicating the number of scoring categories for the item.
#' @param theta a double. An ability level
#' @return a matrix of doubles, items by scores, containing the probability of each possible score for the given items and a specific theta
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   NC = itempool$NC[itempool$MODEL == "GPC"]
#'   theta = 2.0
#'   scoreProb = scoreProbability.theta.gpc(item_pars, NC, theta)
#'   options(digits=8, scipen=8)
#'   > scoreProb       0           1           2           3           4           5           6
#'   item001 0.054534047 0.279500656 0.665965296          NA          NA          NA          NA
#'   item002 0.077870790 0.307631523 0.614497687          NA          NA          NA          NA
#'   item003 0.208115753 0.278064930 0.513819317          NA          NA          NA          NA
#'   item004 0.194203063 0.176087171 0.197053893 0.242477093 0.190178780          NA          NA
#'   item005 0.250122827 0.155418381 0.181922731 0.242986174 0.169549887          NA          NA
#'   item006 ...114 total items/rows
scoreProbability.theta.gpc <- function(item_pars, NC, theta) {
  n1 = nrow(item_pars)
  n2 = ncol(item_pars)-2

  scoreProbMatrix = matrix(data = NA, nrow = n1, ncol = n2)
  scoreProbMatrix0 = matrix(data = NA, nrow = n1, ncol = n2)

  maxNC = max(NC)
  sumList <- vector("list", maxNC)

  for (i in 1:maxNC) {
    if (i > 1) {
      sumList[[i]] = sumList[[i-1]] + item_pars[,1] * (theta - item_pars[,2] + item_pars[,2+i])
    } else {
      sumList[[i]] = item_pars[,1] * (theta - item_pars[,2] + item_pars[,2+i])
    }

    scoreProbMatrix0[,i] = exp(sumList[[i]])
    naAt = which(is.na(scoreProbMatrix0[,i]))
    if (length(naAt) > 0) {
      scoreProbMatrix0[naAt,i] = 0.0
    }
  }

  sumOfSum = vector("double", nrow(item_pars))
  for (i in 1:maxNC) {
    sumOfSum = sumOfSum+scoreProbMatrix0[,i]
  }
  for (i in 1:maxNC) {
    scoreProbMatrix[,i] = scoreProbMatrix0[,i]/sumOfSum
  }

  dimnames(scoreProbMatrix) = list(rownames(item_pars), 0:(ncol(scoreProbMatrix)-1))
  return(scoreProbMatrix)
}

#' Generate a score for each item, based on the difficulty of the item vs the theta ability level.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param theta a double. An ability level
#' @return a vector integers. A score for each item based on the given theta ability
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   theta = 2.0
#'   set.seed(1001)
#'   scores = generateScores(item_pars, theta)
#'   options(digits=8, scipen=8)
#'   > scores
#'   item001 item002 item003 item004 item005 item006 item007 item008 item009 item010
#'   0       2       2       3       2       0       6       6       2       0
#'   item011 item012 item013 item014 item015 item016 item017 item018 item019 item020
#'   2       2       1       2       1       0       2       2       2       1
#'   item021 item022 item023 item024 item025 item026 item027 item028 item029 item030
#'   2       3       0       2       2       0       1       2       1       3
#'   item031 item032 item033 item034 item035 item036 item037 item038 item039 item040
#'   2       0       0       1       2       2       4       1       0       2
#'   item041 item042 item043 item044 item045 item046 item047 item048 item049 item050
#'   1       1       0       1       0       4       2       1       0       0
#'   item051 ...391 total scores
#' @export
generateScores <- function (item_pars, theta) {
  scoreProb = scoreProbability.theta(item_pars, theta)
  randProb = runif(nrow(scoreProb))

  scores = vapply(1:nrow(scoreProb), function(itemInd) {
    # itemInd = 1
    nc = sum(!is.na(scoreProb[itemInd,]))
    sumProb = 0
    for (score in (nc-1):0) { # in descending order to iterate backward
      # score = 1
      sumProb = sumProb + scoreProb[[itemInd, score + 1]]
      if (randProb[[itemInd]] <= sumProb) {
        return(score)
      }
    }
    return(as.integer(nc-1))
  }, as.integer(0), USE.NAMES = FALSE)

  names(scores) = rownames(item_pars)
  return(scores)
}

#' Calculate the information value of each item, based on the difficulty of the item vs the theta ability level.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param theta a double. An ability level
#' @return a vector of doubles. An information value for each item based on the given theta ability
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   theta = 2.0
#'   itemInf = itemInformation(item_pars, theta)
#'   options(digits=8, scipen=8)
#'   > itemInf
#'   item001      item002      item003      item004      item005      item006      item007      item008      item009      item010
#'   0.1068118604 0.0998542776 0.4043015953 0.3817266261 0.3456384832 0.3908781653 0.8798895310 1.3384592783 0.3008433423 0.3069516829
#'   item011      item012      item013      item014      item015      item016      item017      item018      item019      item020
#'   0.0986341211 0.0484800772 0.1874231844 0.0810430934 0.2301688162 0.0886660486 0.0707584918 0.1133582940 0.5546982004 0.0746211906
#'   item021      item022      item023      item024      item025      item026      item027      item028      item029      item030
#'   0.1027127914 0.1285767322 0.1009738653 0.1600664222 0.2461996880 0.0114159873 0.9977338491 1.6097182929 0.1077239682 0.3897308353
#'   item031      item032      item033      item034      item035      item036      item037      item038      item039      item040
#'   0.5815460829 0.2585189679 0.1117031765 0.0489117847 0.0883738236 0.0372521735 0.2572516322 0.6930609211 0.9941486814 0.1084227738
#'   item041      item042      item043      item044      item045      item046      item047      item048      item049      item050
#'   0.1250331028 0.0573824324 0.1207351701 0.0397438702 0.1975182873 0.5563700696 0.1703637978 0.1767498486 0.3782198973 0.0579802303
#'   item051 ...391 total values
#' @export
itemInformation <- function(item_pars, theta) {
  NC = rowSums(!is.na(item_pars))-2
  itemInd.3pl = which(NC <= 2)
  itemInd.gpc = which(NC > 2)

  # special case for speed: no gpc, all 3pl
  if (length(itemInd.gpc) <= 0) {
    return(itemInformation.3pl(item_pars, theta))
  }
  # special case for speed: no 3pl, all gpc
  if (length(itemInd.3pl) <= 0) {
    return(itemInformation.gpc(item_pars, NC, theta))
  }

  # general case, there are some of each
  itemInf = double(nrow(item_pars))

  # populate the rows with 3pl score probs
  itemInf.3pl = itemInformation.3pl(item_pars[itemInd.3pl,,drop=FALSE], theta)
  itemInf[itemInd.3pl] = itemInf.3pl
  names(itemInf)[itemInd.3pl] = names(itemInf.3pl)

  # populate the rows with gpc score probs
  itemInf.gpc = itemInformation.gpc(item_pars[itemInd.gpc,,drop=FALSE], NC[itemInd.gpc], theta)
  itemInf[itemInd.gpc] = itemInf.gpc
  names(itemInf)[itemInd.gpc] = names(itemInf.gpc)

  return(itemInf)
}

#' Calculate the information value of each item, based on the difficulty of the item vs the theta ability level.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param theta a double. An ability level
#' @return a vector of doubles. An information value for each item based on the given theta ability
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   theta = 2.0
#'   itemInf = itemInformation.3pl(item_pars, theta)
#'   options(digits=8, scipen=8)
#'   > itemInf
#'   item042      item043      item044      item045      item048      item049      item050      item051      item052      item053
#'   0.0573824324 0.1207351701 0.0397438702 0.1975182873 0.1767498486 0.3782198973 0.0579802303 0.4984781223 0.2398761585 0.2363306368
#'   item054      item055      item056      item057      item058      item059      item060      item063      item064      item065
#'   0.2150341441 0.0508455836 0.0331527420 0.1038234622 0.0574888741 0.5218887394 0.0457055147 0.0293236847 0.4238179761 0.1010473900
#'   item066      item067      item068      item069      item070      item071      item072      item073      item075      item077
#'   0.0442108431 0.1380087003 0.5188451693 0.1325508768 0.2427019647 0.0326758174 0.0362685411 0.7360391894 0.1957009888 0.0401299204
#'   item078      item079      item080      item081      item082      item083      item084      item086      item087      item088
#'   0.4182005072 0.0882336121 0.0735453800 0.0284117031 0.0472563735 0.4007789174 0.5844731126 0.1430387814 0.3377729572 0.0288792556
#'   item089      item090      item091      item092      item093      item095      item096      item097      item098      item099
#'   0.0618553272 0.0234041262 0.0532715538 0.0191953506 0.0440831510 0.1729177177 0.1811829028 0.0750468411 0.0288081602 0.0430959982
#'   item100 ...277 total values
itemInformation.3pl <- function(item_pars, theta) {
  scoreProb = scoreProbability.theta.3pl(item_pars, theta)
  itemInf = item_pars[,1] * item_pars[,1] * scoreProb[,1] * (scoreProb[,2] - item_pars[,3])^2 / (1 - item_pars[,3])^2 / scoreProb[,2]
  return(itemInf)
}

#' Calculate the information value of each item, based on the difficulty of the item vs the theta ability level.
#'
#' @param item_pars a matrix of doubles, items by params. Must have columns for $PAR_* matching the $MODEL
#' @param NC a vector of integers, one per row/item in item_pars, indicating the number of scoring categories for the item.
#' @param theta a double. An ability level
#' @return a vector of doubles. An information value for each item based on the given theta ability
#' @examples
#'   simulation = readRDS(system.file("example/passage-optimal.rds", package = "CATSimulator"))
#'   item_pars = as.matrix(simulation$itempool[,startsWith(colnames(simulation$itempool), "PAR_")])
#'   dimnames(item_pars) = list(simulation$itempool$ITEM_ID, colnames(simulation$itempool)[startsWith(colnames(simulation$itempool), "PAR_")])
#'   NC = itempool$NC[itempool$MODEL == "GPC"]
#'   theta = 2.0
#'   itemInf = itemInformation.gpc(item_pars, NC, theta)
#'   options(digits=8, scipen=8)
#'   > itemInf
#'   item001     item002     item003     item004     item005     item006     item007     item008     item009     item010     item011
#'   0.106811860 0.099854278 0.404301595 0.381726626 0.345638483 0.390878165 0.879889531 1.338459278 0.300843342 0.306951683 0.098634121
#'   item012     item013     item014     item015     item016     item017     item018     item019     item020     item021     item022
#'   0.048480077 0.187423184 0.081043093 0.230168816 0.088666049 0.070758492 0.113358294 0.554698200 0.074621191 0.102712791 0.128576732
#'   item023     item024     item025     item026     item027     item028     item029     item030     item031     item032     item033
#'   0.100973865 0.160066422 0.246199688 0.011415987 0.997733849 1.609718293 0.107723968 0.389730835 0.581546083 0.258518968 0.111703177
#'   item034     item035     item036     item037     item038     item039     item040     item041     item046     item047     item061
#'   0.048911785 0.088373824 0.037252174 0.257251632 0.693060921 0.994148681 0.108422774 0.125033103 0.556370070 0.170363798 0.515111483
#'   item062     item074     item076     item085     item094     item137     item174     item211     item216     item230     item231
#'   0.797755937 0.680314723 0.414549846 0.413245770 0.146860489 0.369058561 0.517985766 0.298505817 0.774725278 0.482406447 0.166592254
#'   item235 ...114 total values
itemInformation.gpc <- function(item_pars, NC, theta) {
  scoreProb = scoreProbability.theta.gpc(item_pars, NC, theta)
  itemInf = double(nrow(item_pars))

  nItems = nrow(item_pars)
  scoreM = matrix(rep(0:(ncol(scoreProb)-1), nItems), nrow=nItems, byrow = T)

  KTimesP = scoreM * scoreProb
  sumKTimesP_M = rowSums(KTimesP, na.rm = T)
  sumK2TimesP_M = rowSums(scoreM * KTimesP, na.rm = T)
  return(item_pars[,1] * item_pars[,1] * (sumK2TimesP_M - (sumKTimesP_M * sumKTimesP_M)))
}
