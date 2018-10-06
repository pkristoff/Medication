# This tries to figure out which independent variables work will with the output.

#' @include DelayData.R

# Based on https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
# install.packages("lubridate")
library(lubridate)
# install.packages("gdata")
# library(gdata)
# install.packages("leaps")
library(leaps)
library(car)
# install.packages("bestglm")
# library(bestglm)

bestFormulaLeeps <- function() {

  dateAndTimeStr <-
      apply(cbind(as.character(delay.data[,'date']), as.character(delay.data[, 'eattime'])), 1, function(x) paste(x, collapse=" "))
  # print(dateAndTimeStr)
  dateAndTime <- mdy_hm(dateAndTimeStr, tz='US/Eastern') #"%m/%d/%y %I:%M %p"
  # print(dateAndTime)
  delay.seed.len <- length(dateAndTimeStr) - 1
    delay.seed <- data.frame(
      # dateAndTimeStr = dateAndTimeStr[1:delay.seed.len],
      # dateAndTime = dateAndTime[1:delay.seed.len],
      # date =      delay.data[1:delay.seed.len, "date"],
      eatTime =   delay.data[1:delay.seed.len, "eatTime"],
      fat =       delay.data[1:delay.seed.len, "fat"],
      netcarb =   delay.data[1:delay.seed.len, "netcarb"],
      fiber =     delay.data[1:delay.seed.len, "fiber"],
      protien =   delay.data[1:delay.seed.len, "protien"],
      nextpill =  delay.data[1:delay.seed.len, "nextpill"],
      delaytime = delay.data[1:delay.seed.len, "delaytime"]
  )

  # lbw <- delay.seed
  # lbw <- read.xls("http://www.umass.edu/statdata/statdata/data/lowbwt.xls")
  names(delay.seed) <- tolower(names(delay.seed))

  # scatter plot
  # pairs(delay.seed)

  # print(delay.seed[,"dateandtimestr" ])

  print("---------leaps-------")
  # leaps (regression subset selection)
  # Regression subset selection including exhaustive search. This is only for linear regression.
  #
  # Reference: http://www.statmethods.net/stats/regression.html
  #
  # Perform all subset regression, and choose “nbest” model(s) for each number of predictors up to nvmax.

  delay.seed.names <- names(delay.seed)
  formuStr <- paste("delaytime ~ ", paste(delay.seed.names[-length(delay.seed.names)], collapse = "+"), sep = "")
  print(paste("formula(leeps)=", formuStr))
  formu <- as.formula(formuStr)

  regsubsets.out <-
    regsubsets(formu,
               data = delay.seed,
               nbest = 1,       # 1 best model for each number of predictors
               nvmax = NULL,    # NULL for no limit on number of variables
               force.in = NULL, force.out = NULL,
               method = "exhaustive")

  # print("---------regsubsets.out-------")
  # print(regsubsets.out)
  # Best model at each variable number
  #
  # The best model in the 10-variable case includes all variables, as that is the only way to have 10 variables.

  summary.out <- summary(regsubsets.out)
  # # print summary.out$outmat
  # print(as.data.frame(summary.out$outmat))

  # Graphical table of best subsets (plot.regsubsets)
  #
  # By adjusted \( R^2 \), the best model includes lwt, race.cat, preterm, ht, and ui (variables that have black boxes at the higest Y-axis value).

  # Plot Output from regsubsets Function in leaps package
  #
  # This is just another way of presenting the same information for adjusted \( R^2 \). The model with 7 variables (counting dummy variables seprately) has the highest adjusted \( R^2 \).
  #
  # Mallow Cp is used to decide on the number of predictors to include. The stopping rule is to start with the smallest model and gradually increase number of variables, and stop when Mallow Cp is approximately (number of regressors + 1, broken line) for the first time. In this case, the model with 6 regressors is the first one to achieve such a condition.

  layout(matrix(1:2, ncol = 2))
  ## Adjusted R2
  res.legend <-
    subsets(regsubsets.out, statistic="adjr2", legend = FALSE, min.size = 5, main = "Adjusted R^2")
  # print(res.legend)
  # Mallow Cp
  res.legend <-
    subsets(regsubsets.out, statistic="cp", legend = FALSE, min.size = 5, main = "Mallow Cp")
  abline(a = 1, b = 1, lty = 2)
  # print(res.legend)

  # maxIndependentVariables <- which.max(summary.out$adjr2)
  # print("---maxIndependentVariables---")
  # print(maxIndependentVariables)
  # chosenIndependentVarialbes <- summary.out$which[maxIndependentVariables,]
  # print("---chosenIndependentVarialbes---")
  # print(chosenIndependentVarialbes)
  # print(typeof(chosenIndependentVarialbes))
  # str(chosenIndependentVarialbes)

  # print('-----summary.out$which-----')
  # print(summary.out$which)

  # i = 1
  # civ <- c()
  # for (iv in chosenIndependentVarialbes) {
  #   # print(iv)
  #   if (iv && i != 1) {
  #     nm <- names(chosenIndependentVarialbes)[i]
  #     # print(nm)
  #     if (!is.na(nm)) {
  #       civ <- append(civ, nm)
  #     }
  #   }
  #   i <- i + 1
  # }
  # print("---civ---")
  # print(civ)
  # print("---civ end---")

  bestModelVars <- summary.out$which[which.max(summary.out$adjr2),]
  # print(names(bestModelVars[bestModelVars==TRUE])[-1])

  formuStr <- paste("delaytime ~ ", paste(names(bestModelVars[bestModelVars==TRUE])[-1],collapse = "+"), sep = "")
  print(paste("formula(leeps)=", formuStr))
  formu <- as.formula(formuStr)

  best.model <- lm(formu, data = delay.seed)
  # print(summary(best.model))

  best.model
}

bestFormulaLeeps()


