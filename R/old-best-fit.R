# Best fit based on youtube.

# https://www.youtube.com/watch?v=px72eCYPuvc

#pairs(delay.seed) # get scatter plots

#cor(delay.seed)  # get correlation

# individual independent variables lms

processLM <- function(keys, mod){
  print(keys)
  mod.summ <- summary(mod)
  # print(mod.summ$call)
  # print(mod.summ)
  # str(mod.summ)
  #print(paste0("F-statistic: ", mod.summ$fstatistic[1]))
  mod.anova = anova(mod)
  # print(mod.anova)

  if(length(keys) > 1) {
    # http://www.sthda.com/english/articles/39-regression-model-diagnostics/160-multicollinearity-essentials-and-vif-in-r/
    mod.vif <- car::vif(mod)
    for (vif in mod.vif){
      if (vif > 8){
        print(paste("VIF:", vif))
      }
    }
  }

  i <- 1
  for (key in keys){
    print(paste0("F value for ", key, "(x", i, "): ", mod.anova$`F value`[i]))
    good <- ""
    if (mod.anova$`Pr(>F)`[i] <= 0.005){
      good <- " ***"
    } else {
      if (mod.anova$`Pr(>F)`[i] <= 0.05){
        good <- " **"
      }
    }
    print(paste0("P value for ", key, "(x", i, "): ", mod.anova$`Pr(>F)`[i], good))
    i<-i+1
  }

  # str(mod.anova)

  print(paste0("coeff p-value for x0: ", mod.summ$coefficients[1,4]))
  i <- 2
  for (key in keys){
  print(paste0("coeff p-value for ", key, "(x", i-1, "): ", mod.summ$coefficients[i,4]))
    i<-i+1
  }

  print(paste0("Std Error: ", mod.summ$sigma))
  print(paste0("R Squared(adj): ", mod.summ$adj.r.squared))

  # predictive R^2
  print(paste0("R Squared(pred): ", predicted.r.squared(mod)))
  print("-----------------------------")
  return(mod)
}

# https://tomhopper.me/2014/05/16/can-we-do-better-than-r-squared/
predicted.r.squared <- function (mod){
  pr <- residuals(mod)/(1 - lm.influence(mod)$hat)
  PRESS <- sum(pr^2)
  # anova to calculate residual sum of squares
  mod.anova <- anova(mod)
  tss <- sum(mod.anova$"Sum Sq")
  pred.r.squared <- 1 - PRESS/(tss)
  return(pred.r.squared)
}

combiner <- function(comb){
  delay.seed <- data.frame(delay.seed <- data.frame(
    date = c('9/12/18', '9/12/18', '9/12/18', '9/12/18', '9/13/18', '9/13/18', '9/13/18', '9/14/18', '9/14/18', '9/14/18', '9/14/18'),
    eatTime =   c(  1,   2,   3,   4,   1,   2,   3,   1,   2,   3,   4),
    fat =       c( 17,  18,  19,  16,  13,  26,  60,  15,  26,  38,  10),
    netcarb =   c( 65,  69,  49,  12,  70,  24, 101,  46,  24,  20,  37),
    fiber =     c( 11,  12,  12,   3,   9,   5,  20,   9,   5,   4,   5),
    protien =   c( 25,  24,  16,  33,  21,  27,  40,  19,  27,  54,   6),
    nextpill =  c(170, 160, 180, 160, 170, 150, 120, 170, 135, 150, 105),
    delaytime = c( 60,  30,  50,  45,  45,  60,  20,  35,  60, 120, 105)

  )
  )
  formuStr <- paste("delaytime ~ ", paste(comb,collapse = "+"), sep = "")
  formu <- as.formula(formuStr)
  # print(formuStr)
  processLM(comb, lm(formu, data = delay.seed))
}
indVars <- c("eatTime","fat","netcarb","fiber","protien","nextpill")
combn(indVars,1,combiner)
combn(indVars,2,combiner)
combn(indVars,3,combiner)
combn(indVars,4,combiner)
combn(indVars,5,combiner)
combn(indVars,6,combiner)

