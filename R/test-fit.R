#' @include best-fit-leeps.R
#' @include best-fit-glm.R

mod.prediction <- function(){
  # combine date & time
  dateAndTimeStr <- apply(cbind(as.character(delay.data[,'date']), as.character(delay.data[, 'eattime'])), 1, function(x) paste(x, collapse=" "))
  # print(dateAndTimeStr)
  # pairs(delay.data)
  delay.seed.len <- length(dateAndTimeStr) - 1
  delay.seed <- data.frame(
    dateAndTime = dateAndTimeStr[1:delay.seed.len],# "%m/%d/%y %I:%m %p"),
    date = delay.data[1:delay.seed.len, "date"],
    eatTime =   delay.data[1:delay.seed.len, "eatTime"],
    fat =       delay.data[1:delay.seed.len, "fat"],
    netcarb =   delay.data[1:delay.seed.len, "netcarb"],
    fiber =     delay.data[1:delay.seed.len, "fiber"],
    protien =   delay.data[1:delay.seed.len, "protien"],
    nextpill =  delay.data[1:delay.seed.len, "nextpill"],
    delaytime = delay.data[1:delay.seed.len, "delaytime"]
  )
  delay.test.len <- length(dateAndTimeStr)
  delay.test <- data.frame(
    dateAndTime = dateAndTimeStr[delay.test.len],
    date = delay.data[delay.test.len, "date"],
    eatTime =   delay.data[delay.test.len, "eatTime"],
    fat =       delay.data[delay.test.len, "fat"],
    netcarb =   delay.data[delay.test.len, "netcarb"],
    fiber =     delay.data[delay.test.len, "fiber"],
    protien =   delay.data[delay.test.len, "protien"],
    nextpill =  delay.data[delay.test.len, "nextpill"],
    delaytime = delay.data[delay.test.len, "delaytime"]
  )
  names(delay.test) <- tolower(names(delay.test))
  # print(delay.seed)
  # print(delay.test)

  test.prediction <- function(mod.pred, delay.test) {
    delaytime.actual <- delay.test[, "delaytime"]
    delaytime.pred <- mod.pred[, 1]

    # print('delaytime.actual')
    # print(delaytime.actual)

    delaytime.comp <- data.frame(
      actual = delaytime.actual,
      predict = delaytime.pred,
      percentDiff = ((delaytime.actual - delaytime.pred) / delaytime.actual) * 100,
      diff = delaytime.actual == delaytime.pred
    )
    print(delaytime.comp)
    delaytime.comp
  }

  mod.glm <- bestFormulaGlm()
  mod.leeps <- bestFormulaLeeps()

  # mod <- lm(delaytime ~ fiber + nextpill, data = delay.seed)
  # mod <- lm(delaytime ~ eatTime + netcarb + fiber + protien, data = delay.seed)
  # mod <- lm(delaytime ~ date + eatTime + netcarb + fiber + protien + nextpill, data = delay.seed)

  # print(summary(mod))

  mod.glm.pred.conf <-
    predict(mod.glm, delay.test, level = 0.95, interval = "confidence")
  mod.glm.pred.pred <-
    predict(mod.glm, delay.test, level = 0.95, interval = "prediction")

  mod.leeps.pred.conf <-
    predict(mod.leeps, delay.test, level = 0.95, interval = "confidence")

  mod.leeps.pred.pred <-
    predict(mod.leeps, delay.test, level = 0.95, interval = "prediction")

  # plot(delay.seed[c("delaytime","fiber","nextpill")])

  print("----------mod.leeps.pred.conf-------")
  # print(summary(mod.leeps.pred.conf))
  # print(mod.leeps.pred.conf)
  test.prediction(mod.leeps.pred.conf, delay.test)
  # predplot(mod.pred.conf,ncomp=3,newdata=delay.test,asp=1,line=TRUE)
  print("----------mod.leeps.pred.pred-------")
  # print(summary(mod.leeps.pred.pred))
  # print(mod.leeps.pred.pred)
  # str(mod.pred.pred)
  test.prediction(mod.leeps.pred.pred, delay.test)
  print("----------end-------")

  print("----------mod.glm.pred.conf-------")
  # print(summary(mod.glm.pred.conf))
  # print(mod.glm.pred.conf)
  test.prediction(mod.glm.pred.conf, delay.test)
  # predplot(mod.pred.conf,ncomp=3,newdata=delay.test,asp=1,line=TRUE)
  print("----------mod.glm.pred.pred-------")
  # print(summary(mod.glm.pred.pred))
  # print(mod.glm.pred.pred)
  # str(mod.pred.pred)
  test.prediction(mod.glm.pred.pred, delay.test)
  print("----------end-------")

  # ddd <- delay.seed[, 'zzz']
  # print(ddd)
  # print(weekdays(ddd))
  # print(as.Date(delay.data[, "date"]))
}

mod.prediction()
