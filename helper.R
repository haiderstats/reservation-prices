findProfit = function(cost, reservationPrice, curves, predictedTimes){
  cprop = seq(0,.95,by = 0.05)
  times = seq(0,19, length.out = 500) #19 is the max reservation price across all chocolates
  probabilities = sapply(1:ncol(curves), function(x) MTLR:::predict_prob(curves[,x],predictedTimes, times))
  profits = c()
  for(p in cprop){
    bestPrices = times[sapply(1:ncol(curves),function(x) which.max((times - cost[x]*p)*probabilities[,x]))]
    avgProfit = mean((bestPrices <= reservationPrice)*(bestPrices - p*cost))
    profits = c(profits,avgProfit)
  }
  return(profits)
}

findBestPriceML = function(model,testRow,train){
  times = seq(0,max(train$Retail), length.out = 500) #19 is the max reservation price across all chocolates
  row.names(testRow) = NULL
  newTest = cbind.data.frame(Retail = times,testRow)
  cl = attr(model, "class")[1]
  probs = switch(cl,
                 randomForest = predict(model,newTest,type = "prob")[,2],
                 glm = predict(model, newTest,type =  "response"),
                 lognet = predict(model, as.matrix(newTest), type = "response"),
                 naiveBayes  = {p  = predict(model,newTest, type = "raw")[,2]; ifelse(p ==1,0,p)},
                 svm.formula = attr(predict(model,as.matrix(newTest), probability = TRUE),"prob")[,1],
                 lda = {p = predict(model,newTest); p$posterior[,2]}
  )
  return(probs)
}

findProfitML = function(cost, model,test,train){
  cprop = seq(0,.95,by = 0.05)
  times = seq(0,19, length.out = 500) #19 is the max reservation price across all chocolates
  row.names(test) = NULL
  probabilities = sapply(1:nrow(test), function(x) findBestPriceML(model, test[x,-c(1,2,3)], train))
  profits = c()
  for(p in cprop){
    bestPrices = times[sapply(1:nrow(test),function(x) which.max((times - cost[x]*p)*probabilities[,x]))]
    avgProfit = mean((bestPrices <= test$RP)*(bestPrices - p*cost))
    profits = c(profits,avgProfit)
  }
  return(profits)
}

survfunc = function (object, t, newdata, name = "t") {
  #Altered from origina: I am going to add an ID to every row so we can retrieve the individuals easily from the output.
  #I gave a weird ID variable name so if the original data came in with a variable ("ID") it won't break our system.
  newdata$ID_SurvivalCurves = 1:nrow(newdata)
  newdata <- do.call(rbind, rep(list(newdata), length(t)))
  t <- rep(t, each = nrow(newdata)/length(t))
  if (class(object) != "survreg") 
    stop("not a survreg object")
  lp <- predict(object, newdata = newdata, type = "lp")
  if (object$dist %in% c("weibull", "exponential")) {
    newdata$pdf <- dweibull(t, 1/object$scale, exp(lp))
    newdata$cdf <- ifelse(t == 0,0,
                          ifelse(is.nan(pweibull(t, 1/object$scale, exp(lp))),1,pweibull(t, 1/object$scale, exp(lp))))
    newdata$haz <- exp(dweibull(t, 1/object$scale, exp(lp), 
                                log = TRUE) - pweibull(t, 1/object$scale, exp(lp), 
                                                       lower.tail = FALSE, log.p = TRUE))
  }
  else if (object$dist == "lognormal") {
    newdata$pdf <- dlnorm(t, lp, object$scale)
    newdata$cdf <- plnorm(t, lp, object$scale)
    newdata$haz <- exp(dlnorm(t, lp, object$scale, log = TRUE) - 
                         plnorm(t, lp, object$scale, lower.tail = FALSE, log.p = TRUE))
  }
  else if (object$dist == "gaussian") {
    newdata$pdf <- dnorm(t, lp, object$scale)
    newdata$cdf <- pnorm(t, lp, object$scale)
    newdata$haz <- exp(dnorm(t, lp, object$scale, log = TRUE) - 
                         pnorm(t, lp, object$scale, lower.tail = FALSE, log.p = TRUE))
  }
  else if (object$dist == "loglogistic") {
    newdata$pdf <- dlogis(log(t), lp, object$scale)/t
    newdata$cdf <- plogis(log(t), lp, object$scale)
    newdata$haz <- exp(dlogis(log(t), lp, object$scale, log = TRUE) - 
                         log(t) - plogis(log(t), lp, object$scale, lower.tail = FALSE, 
                                         log.p = TRUE))
  }
  else if (object$dist == "logistic") {
    newdata$pdf <- dlogis(t, lp, object$scale)
    newdata$cdf <- plogis(t, lp, object$scale)
    newdata$haz <- exp(dlogis(t, lp, object$scale, log = TRUE) - 
                         dlogis(t, lp, object$scale, lower.tail = FALSE, log.p = TRUE))
  }
  else {
    stop("unknown distribution")
  }
  newdata$sur <- 1 - newdata$cdf
  newdata[name] <- t
  return(newdata)
}