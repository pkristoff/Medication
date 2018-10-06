# This tries to figure out which independent variables work will with the output.

#' @include DelayData.R

# Based on https://rstudio-pubs-static.s3.amazonaws.com/2897_9220b21cfc0c43a396ff9abf122bb351.html
# install.packages("lubridate")
library(lubridate)
# install.packages("gdata")
# library(gdata)
# library(car)
# install.packages("bestglm")
library(bestglm)

#' Generate best formula via glm
#'
#' Assumes delay.data exists
#'
#' @return The best model
#' @examples
#' bestFormulaGlm()
bestFormulaGlm <- function() {

  dateAndTimeStr <-
      apply(cbind(as.character(delay.data[,'date']), as.character(delay.data[, 'eattime'])), 1, function(x) paste(x, collapse=" "))
  dateAndTime <- mdy_hm(dateAndTimeStr, tz='US/Eastern') #"%m/%d/%y %I:%M %p"

  delay.seed.len <- length(dateAndTimeStr) - 1
    delay.seed <- data.frame(
      # dateAndTimeStr = dateAndTimeStr[1:delay.seed.len],
      # dateAndTime = dateAndTime[1:delay.seed.len],
      date =      delay.data[1:delay.seed.len, "date"],
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

  print("---------bestglm-------")

  lbw.for.bestglm <- within(delay.seed, {
        y    <- delaytime         # delaytime into y
        delaytime  <- NULL        # Delete delaytime
  })
  # print("---------lbw.for.bestglm-------")
  # print(lbw.for.bestglm)
  res.bestglm <-
    bestglm(Xy = lbw.for.bestglm,
            family = gaussian,
            IC = "AIC",                 # Information criteria for
            method = "exhaustive")

  # print("---------res.bestglm$BestModels-------")
  # print(res.bestglm$BestModels)


  # print("---------res.bestglm$BestModels: summary-------")
  # print(summary(res.bestglm$BestModel))

  print("---------bestglm end-------")

  # str(res.bestglm$BestModels[1])
  # print("---------BestModel-------")
  # print(typeof(res.bestglm$BestModels))
  bestModel <- res.bestglm$BestModels[1,]
  # print(typeof(bestModel))
  # print(bestModel)
  # convert bestModel from named list to a named vector.
  # unfortunately, the TRUE/FALSE are converted to 1.0/0.0 respectively.
  bestModelVec <- unlist(bestModel, use.names = TRUE)
  # print(bestModelVec)
  formuStr <- paste("delaytime ~ ", paste(names(bestModelVec[bestModelVec==1.0])[-length(bestModelVec)],collapse = "+"), sep = "")
  print(paste("formula(glm)=", formuStr))
  formu <- as.formula(formuStr)

  best.model <- lm(formu, data = delay.seed)
  # print(summary(best.model))

  best.model
}

bestFormulaGlm()
