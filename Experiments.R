library(icenReg)
library(prodlim)
library(e1071)
library(MASS)
#devtools::install_github("haiderstats/MTLR")
library(MTLR)

lindt = read.csv(paste("./Data/lindt",".csv",sep=""), header=F)
lindt$brand = "lindt"
hersheys = read.csv(paste("./Data/hersheys",".csv",sep=""), header=F)
hersheys$brand = "hersheys"
valrhona = read.csv(paste("./Data/valrhona",".csv",sep=""), header=F)
valrhona$brand = "valrhona"
godiva = read.csv(paste("./Data/godiva",".csv",sep=""), header=F)
godiva$brand = "godiva"

fullChocolate = rbind.data.frame(lindt,hersheys,valrhona,godiva)
lretail = median(lindt$V1)
hretail = median(hersheys$V1)
vretail = median(valrhona$V1)
gretail = median(godiva$V1)

retail = rep(c(lretail,hretail,vretail,gretail), each = 722)
counter = 0
chocDat = fullChocolate
names(chocDat)[1:3] = c("RP","Retail","Decision")
chocDat$Decision = ifelse(chocDat$Decision == -1,0,1)
filler = matrix(0,ncol = 5,nrow = 100)
errorBaselineMat = data.frame(filler)
errorKMMat = data.frame(filler)
errorCoxMat = data.frame(filler)
errorAFTMat = data.frame(filler)
errorMTLRMat = data.frame(filler)
classificationBaselineMat = data.frame(filler)
classificationKMMat = data.frame(filler)
classificationCoxMat = data.frame(filler)
classificationAFTMat = data.frame(filler)
classificationMTLRMat = data.frame(filler)
names(errorBaselineMat) = names(errorKMMat) = names(errorCoxMat) = names(errorAFTMat) = names(errorMTLRMat)= 
  names(classificationBaselineMat) = names(classificationKMMat) = names(classificationAFTMat)  =
  names(classificationCoxMat) = names(classificationMTLRMat) = c("Godiva","Hersheys","Lindt","Valrhona","Total")
ncols = length(seq(0,0.95, by = 0.05))
profitRetail = array(dim = c(10,ncols,10,5))
profitOptimalPrice = array(dim = c(10,ncols,10))
profitKM = array(dim = c(10,ncols,10,5))
profitAFT = array(dim = c(10,ncols,10,5)) 
profitCox = array(dim = c(10,ncols,10,5))
profitMTLR = array(dim = c(10,ncols,10,5))
profitLRNet = array(dim = c(10,ncols,10,5))
profitLR = array(dim = c(10,ncols,10,5))
profitNB = array(dim = c(10,ncols,10,5))
profitLDA = array(dim = c(10,ncols,10,5))

set.seed(42)
for(nRepeat in 1:10){
  testInd = create_folds(chocDat$Retail, chocDat$Decision,10,foldtype = "censorstrat")
  for(i in 1:10){
    train = chocDat[-testInd[[i]],]
    test = chocDat[testInd[[i]],]
    gInd = test$brand == "godiva"
    lInd = test$brand == "lindt"
    hInd = test$brand == "hersheys"
    vInd = test$brand == "valrhona"
    
    gIndT = train$brand == "godiva"
    lIndT = train$brand == "lindt"
    hIndT = train$brand == "hersheys"
    vIndT = train$brand == "valrhona"
    #Baseline###
    errorBaselineG = mean(abs(median(train$RP[gIndT]) - test$RP[gInd]))
    errorBaselineH = mean(abs(median(train$RP[hIndT]) - test$RP[hInd]))
    errorBaselineL = mean(abs(median(train$RP[lIndT]) - test$RP[lInd]))
    errorBaselineV = mean(abs(median(train$RP[vIndT]) - test$RP[vInd]))
    errorBaseline = (errorBaselineG*sum(gInd) + errorBaselineH*sum(hInd) + errorBaselineL*sum(lInd) +errorBaselineV*sum(vInd))/nrow(test)
    errorBaselineMat[i + (nRepeat-1)*10,] = c(errorBaselineG,errorBaselineH,errorBaselineL,errorBaselineV,errorBaseline)
    
    classificationBaselineG = max(sum(test$Decision[gInd]), length(test$Decision[gInd]) - sum(test$Decision[gInd]))/length(test$Decision[gInd])
    classificationBaselineH = max(sum(test$Decision[hInd]), length(test$Decision[hInd]) - sum(test$Decision[hInd]))/length(test$Decision[hInd])
    classificationBaselineL = max(sum(test$Decision[lInd]), length(test$Decision[lInd]) - sum(test$Decision[lInd]))/length(test$Decision[lInd])
    classificationBaselineV = max(sum(test$Decision[vInd]), length(test$Decision[vInd]) - sum(test$Decision[vInd]))/length(test$Decision[vInd])
    classificationBaseline = (classificationBaselineG*sum(gInd) + classificationBaselineH*sum(hInd) + classificationBaselineL*sum(lInd) +classificationBaselineV*sum(vInd))/nrow(test)
    classificationBaselineMat[i + (nRepeat-1)*10,] = c(classificationBaselineG,classificationBaselineH,classificationBaselineL,classificationBaselineV,classificationBaseline)
    
    col = 0
    for(cost in seq(0,0.95,by = 0.05)){
      col = col+1
      profitG = mean((retail[testInd[[i]]][gInd]  <= test$RP[gInd]) * (retail[testInd[[i]]][gInd] - cost*(retail[testInd[[i]]][gInd])))
      profitH = mean((retail[testInd[[i]]][hInd]  <= test$RP[hInd]) * (retail[testInd[[i]]][hInd] - cost*(retail[testInd[[i]]][hInd])))
      profitL = mean((retail[testInd[[i]]][lInd]  <= test$RP[lInd]) * (retail[testInd[[i]]][lInd] - cost*(retail[testInd[[i]]][lInd])))
      profitV = mean((retail[testInd[[i]]][vInd]  <= test$RP[vInd]) * (retail[testInd[[i]]][vInd] - cost*(retail[testInd[[i]]][vInd])))
      
      profit = mean((retail[testInd[[i]]]  <= test$RP) * (retail[testInd[[i]]] - cost*(retail[testInd[[i]]])))
      profitRetail[i,col, nRepeat,1] = profitG
      profitRetail[i,col, nRepeat,2] = profitH
      profitRetail[i,col, nRepeat,3] = profitL
      profitRetail[i,col, nRepeat,4] = profitV
      profitRetail[i,col, nRepeat,5] = profit
    }

l = ifelse(train$RP > train$Retail,train$Retail, 0)
u = ifelse(train$RP > train$Retail,Inf, train$Retail)
#KM#####
np_fit = ic_np(cbind(l, u) ~ brand, data = cbind.data.frame(l,u,brand= train$brand))
KMcurveG = np_fit$scurves$godiva$S_curves$baseline
KMcurveL = np_fit$scurves$lindt$S_curves$baseline
KMcurveH = np_fit$scurves$hersheys$S_curves$baseline
KMcurveV = np_fit$scurves$valrhona$S_curves$baseline

KMcurveG = KMcurveG[order(KMcurveG
                          ,decreasing = T)]
KMcurveL = KMcurveL[order(KMcurveL,decreasing = T)]
KMcurveH = KMcurveH[order(KMcurveH,decreasing = T)]
KMcurveV = KMcurveV[order(KMcurveV,decreasing = T)]

KMtimesG = np_fit$scurves$godiva$Tbull_ints
KMtimesL = np_fit$scurves$lindt$Tbull_ints
KMtimesH = np_fit$scurves$hersheys$Tbull_ints
KMtimesV = np_fit$scurves$valrhona$Tbull_ints

indG = length(KMcurveG) - findInterval(.5,rev(KMcurveG))
indL = length(KMcurveL) - findInterval(.5,rev(KMcurveL))
indH = length(KMcurveH) - findInterval(.5,rev(KMcurveH))
indV = length(KMcurveV) - findInterval(.5,rev(KMcurveV))

medianG = KMtimesG[indG,2]
medianL = KMtimesL[indL,2]
medianH = KMtimesH[indH,2]
medianV = KMtimesV[indV,2]

errorKMG = mean(abs(medianG - test$RP[gInd]))
errorKML = mean(abs(medianL - test$RP[lInd]))
errorKMH = mean(abs(medianH - test$RP[hInd]))
errorKMV = mean(abs(medianV - test$RP[vInd]))

errorKM = (errorKMG*sum(gInd) + errorKML*sum(lInd) + 
             errorKMH*sum(hInd) +  errorKMV*sum(vInd))/nrow(test)

errorKMMat[i + (nRepeat-1)*10,] = c(errorKMG,errorKMH,errorKML,errorKMV,errorKM)


classificationKMG = mean(as.numeric((medianG >= test$Retail[test$brand == "godiva"])) == test$Decision[test$brand == "godiva"])
classificationKML = mean(as.numeric((medianL >= test$Retail[test$brand == "lindt"])) == test$Decision[test$brand == "lindt"])
classificationKMH = mean(as.numeric((medianH >= test$Retail[test$brand == "hersheys"])) == test$Decision[test$brand == "hersheys"])
classificationKMV = mean(as.numeric((medianV >= test$Retail[test$brand == "valrhona"])) == test$Decision[test$brand == "valrhona"])

classificationKM = (classificationKMG*length(which(test$brand == "godiva")) + classificationKML*length(which(test$brand == "lindt")) + 
                      classificationKMH*length(which(test$brand == "hersheys")) +  classificationKMV*length(which(test$brand == "valrhona")))/nrow(test)
classificationKMMat[i + (nRepeat-1)*10,] = c(classificationKMG,classificationKMH,classificationKML,classificationKMV,classificationKM)

numG = sum(test$brand=="godiva")
numH = sum(test$brand == "hersheys")
numL = sum(test$brand == "lindt")
numV = sum(test$brand == "valrhona")

curvesG = matrix(rep(KMcurveG,numG), nrow = length(KMcurveG), byrow = F)
curvesG = cbind.data.frame(KMtimesG[,1], curvesG)
curvesG = rbind.data.frame(c(0,rep(1, nrow(test))), curvesG)
profitG = findProfit(retail[testInd[[i]]][test$brand == "godiva"],
                     test$RP[test$brand=="godiva"], curvesG[,-1], curvesG[,1])

curvesH = matrix(rep(KMcurveH,numH), nrow = length(KMcurveH), byrow = F)
curvesH = cbind.data.frame(KMtimesH[,1], curvesH)
curvesH = rbind.data.frame(c(0,rep(1, nrow(test))), curvesH)
profitH = findProfit(retail[testInd[[i]]][test$brand == "hersheys"],
                     test$RP[test$brand == "hersheys"], curvesH[,-1], curvesH[,1])

curvesL = matrix(rep(KMcurveL,numL), nrow = length(KMcurveL), byrow = F)
curvesL = cbind.data.frame(KMtimesL[,1], curvesL)
curvesL = rbind.data.frame(c(0,rep(1, nrow(test))), curvesL)
profitL = findProfit(retail[testInd[[i]]][test$brand =="lindt"],
                     test$RP[test$brand == "lindt"], curvesL[,-1], curvesL[,1])

curvesV = matrix(rep(KMcurveV,numV), nrow = length(KMcurveV), byrow = F)
curvesV = cbind.data.frame(KMtimesV[,1], curvesV)
curvesV = rbind.data.frame(c(0,rep(1, nrow(test))), curvesV)
profitV = findProfit(retail[testInd[[i]]][test$brand =="valrhona"],
                     test$RP[test$brand=="valrhona"], curvesV[,-1], curvesV[,1])
  
profit = (numG*profitG + numH*profitH + numL*profitL +numV*profitV)/nrow(test)

  profitKM[i,, nRepeat,1] = profitG
  profitKM[i,, nRepeat,2] = profitH
  profitKM[i,, nRepeat,3] = profitL
  profitKM[i,, nRepeat,4] = profitV
  profitKM[i,, nRepeat,5] = profit
#Cox###
    l = ifelse(train$RP > train$Retail,train$Retail, 0)
    u = ifelse(train$RP > train$Retail,Inf, train$Retail)
    weights = ifelse(train$Decision==0,1.5,1)
    fit_ph <- ic_sp(cbind(l, u) ~ ., model = 'ph',
                    data = cbind.data.frame(l,u,train[-c(1,2,3)]))#, weights = weights)
    curvesCox = getSCurves(fit_ph, test)
    timesCox = unname(sapply(curvesCox$S_curves, function(x) curvesCox$Tbull_ints[length(x) - findInterval(.5,rev(x)),2]))
    errorCoxG = mean(abs(timesCox[gInd] - test$RP[gInd]))
    errorCoxH = mean(abs(timesCox[hInd] - test$RP[hInd]))
    errorCoxL = mean(abs(timesCox[lInd] - test$RP[lInd]))
    errorCoxV = mean(abs(timesCox[vInd] - test$RP[vInd]))
    errorCox = mean(abs(timesCox - test$RP))
    errorCoxMat[i + (nRepeat-1)*10,] = c(errorCoxG,errorCoxH,errorCoxL,errorCoxV,errorCox)
    classificationCoxG = mean(as.numeric((timesCox[gInd] >= test$Retail[gInd])) == test$Decision[gInd])
    classificationCoxH = mean(as.numeric((timesCox[hInd] >= test$Retail[hInd])) == test$Decision[hInd])
    classificationCoxL = mean(as.numeric((timesCox[lInd] >= test$Retail[lInd])) == test$Decision[lInd])
    classificationCoxV = mean(as.numeric((timesCox[vInd] >= test$Retail[vInd])) == test$Decision[vInd])
    classificationCox = mean(as.numeric((timesCox >= test$Retail)) == test$Decision)
    classificationCoxMat[i + (nRepeat-1)*10,] = c(classificationCoxG,classificationCoxH,classificationCoxL,classificationCoxV,classificationCox)
    curves = as.data.frame(curvesCox$S_curves)
    curves = cbind.data.frame(curvesCox$Tbull_ints[,1], curves)
    curves = rbind.data.frame(c(0,rep(1, nrow(test))), curves)
    profitG = findProfit(retail[testInd[[i]]][gInd], test$RP[gInd], curves[,-1][gInd], curves[,1])
    profitH = findProfit(retail[testInd[[i]]][hInd], test$RP[hInd], curves[,-1][hInd], curves[,1])
    profitL = findProfit(retail[testInd[[i]]][lInd], test$RP[lInd], curves[,-1][lInd], curves[,1])
    profitV = findProfit(retail[testInd[[i]]][vInd], test$RP[vInd], curves[,-1][vInd], curves[,1])
    profit = findProfit(retail[testInd[[i]]], test$RP, curves[,-1], curves[,1])
    
    profitCox[i,, nRepeat,1] = profitG
    profitCox[i,, nRepeat,2] = profitH
    profitCox[i,, nRepeat,3] = profitL
    profitCox[i,, nRepeat,4] = profitV
    profitCox[i,, nRepeat,5] = profit
    ########
    #AFT###
    time1 = ifelse(train$RP > train$Retail,train$Retail, NA)
    time2 = ifelse(train$RP > train$Retail,NA, train$Retail)
    mod = survreg(Surv(time1,time2,type = "interval2")~., data = cbind.data.frame(time1,time2,train[-c(1,2,3)]), dist = "lognormal")
    prices = predict(mod,test,type = "quantile", p = .5)
    errorAFTG = mean(abs(prices[gInd] - test$RP[gInd]))
    errorAFTH = mean(abs(prices[hInd] - test$RP[hInd]))
    errorAFTL = mean(abs(prices[lInd] - test$RP[lInd]))
    errorAFTV = mean(abs(prices[vInd] - test$RP[vInd]))
    errorAFT = mean(abs(prices - test$RP))
    errorAFTMat[i + (nRepeat-1)*10,] = c(errorAFTG,errorAFTH,errorAFTL,errorAFTV,errorAFT)
    
    classificationAFTG = mean(as.numeric((prices[gInd] >= test$Retail[gInd])) == test$Decision[gInd])
    classificationAFTH = mean(as.numeric((prices[hInd] >= test$Retail[hInd])) == test$Decision[hInd])
    classificationAFTL = mean(as.numeric((prices[lInd] >= test$Retail[lInd])) == test$Decision[lInd])
    classificationAFTV = mean(as.numeric((prices[vInd] >= test$Retail[vInd])) == test$Decision[vInd])
    classificationAFT = mean(as.numeric((prices >= test$Retail)) == test$Decision)
    classificationAFTMat[i + (nRepeat-1)*10,] = c(classificationAFTG,classificationAFTH,classificationAFTL,classificationAFTV,classificationAFT)
    survivalCurves = survfunc(mod, newdata = test, t = seq(0,16,length.out = 100))
    probabilities = survivalCurves$sur
    probabilityMatrix = matrix(probabilities, ncol = nrow(test),byrow = T)
    curves = cbind.data.frame(time= seq(0,16,length.out = 100), probabilityMatrix)

    profitG = findProfit(retail[testInd[[i]]][gInd], test$RP[gInd], curves[,-1][gInd], curves[,1])
    profitH = findProfit(retail[testInd[[i]]][hInd], test$RP[hInd], curves[,-1][hInd], curves[,1])
    profitL = findProfit(retail[testInd[[i]]][lInd], test$RP[lInd], curves[,-1][lInd], curves[,1])
    profitV = findProfit(retail[testInd[[i]]][vInd], test$RP[vInd], curves[,-1][vInd], curves[,1])
    profit = findProfit(retail[testInd[[i]]], test$RP, curves[,-1], curves[,1])
    
    profitAFT[i,, nRepeat,1] = profitG
    profitAFT[i,, nRepeat,2] = profitH
    profitAFT[i,, nRepeat,3] = profitL
    profitAFT[i,, nRepeat,4] = profitV
    profitAFT[i,, nRepeat,5] = profit
    #LRMod
    lrMod = glm(Decision ~., data = train[-1], family = binomial())
    profitG = findProfitML(retail[testInd[[i]]][gInd],lrMod, test[gInd,], train)
    profitH = findProfitML(retail[testInd[[i]]][hInd],lrMod, test[hInd,], train)
    profitL = findProfitML(retail[testInd[[i]]][lInd],lrMod, test[lInd,], train)
    profitV = findProfitML(retail[testInd[[i]]][vInd],lrMod, test[vInd,], train)
    profit = findProfitML(retail[testInd[[i]]],lrMod, test, train)
    
    profitLR[i,, nRepeat,1] = profitG
    profitLR[i,, nRepeat,2] = profitH
    profitLR[i,, nRepeat,3] = profitL
    profitLR[i,, nRepeat,4] = profitV
    profitLR[i,, nRepeat,5] = profit
    ##LDA
    ldaMod = lda(Decision ~., data = train[-1])
    
    profitG = findProfitML(retail[testInd[[i]]][gInd],ldaMod, test[gInd,], train)
    profitH = findProfitML(retail[testInd[[i]]][hInd],ldaMod, test[hInd,], train)
    profitL = findProfitML(retail[testInd[[i]]][lInd],ldaMod, test[lInd,], train)
    profitV = findProfitML(retail[testInd[[i]]][vInd],ldaMod, test[vInd,], train)
    profit = findProfitML(retail[testInd[[i]]],ldaMod, test, train)
    
    profitLDA[i,, nRepeat,1] = profitG
    profitLDA[i,, nRepeat,2] = profitH
    profitLDA[i,, nRepeat,3] = profitL
    profitLDA[i,, nRepeat,4] = profitV
    profitLDA[i,, nRepeat,5] = profit
    #NB
    nbMod = naiveBayes(Decision ~., data = train[-1])
    profitG = findProfitML(retail[testInd[[i]]][gInd],nbMod, test[gInd,], train)
    profitH = findProfitML(retail[testInd[[i]]][hInd],nbMod, test[hInd,], train)
    profitL = findProfitML(retail[testInd[[i]]][lInd],nbMod, test[lInd,], train)
    profitV = findProfitML(retail[testInd[[i]]][vInd],nbMod, test[vInd,], train)
    profit = (profitG*sum(gInd) + profitH*sum(hInd)+profitL*sum(lInd)+profitV*sum(vInd))/nrow(test)

    profitNB[i,, nRepeat,1] = profitG
    profitNB[i,, nRepeat,2] = profitH
    profitNB[i,, nRepeat,3] = profitL
    profitNB[i,, nRepeat,4] = profitV
    profitNB[i,, nRepeat,5] = profit
    #MTLR
      weights = ifelse(train$brand=="godiva",1,1)
      time1 = ifelse(train$RP > train$Retail,train$Retail, NA)
      time2 = ifelse(train$RP > train$Retail,NA, train$Retail)
      C1 = mtlr_cv(Surv(time1,time2,type = "interval2")~., data = cbind.data.frame(time1,time2,train[-c(1,2,3)]),C1_vec = seq(.2,.4,length.out = 9),
                  train_biases = F, verbose =F, weights = weights, threshold = .000001, nfolds = 3)
      #print(C1$avg_loss)
      C1 = C1$bestC1      
      modMTLR = mtlr(Surv(time1,time2,type = "interval2")~., data = cbind.data.frame(time1,time2,train[-c(1,2,3)]),C1 = C1, weights = weights,train_biases = F,
                     threshold = .000001)
      pricesMTLR = predict(modMTLR,test, type = "median")
      errorMTLRG = mean(abs(pricesMTLR[gInd] - test$RP[gInd]))
      errorMTLRH = mean(abs(pricesMTLR[hInd] - test$RP[hInd]))
      errorMTLRL = mean(abs(pricesMTLR[lInd] - test$RP[lInd]))
      errorMTLRV = mean(abs(pricesMTLR[vInd] - test$RP[vInd]))
      errorMTLR = mean(abs(pricesMTLR - test$RP))
      errorMTLRMat[i + (nRepeat-1)*10,] = c(errorMTLRG,errorMTLRH,errorMTLRL,errorMTLRV,errorMTLR)
      
      classificationMTLRG = mean(as.numeric((pricesMTLR[gInd] >= test$Retail[gInd])) == test$Decision[gInd])
      classificationMTLRH = mean(as.numeric((pricesMTLR[hInd] >= test$Retail[hInd])) == test$Decision[hInd])
      classificationMTLRL = mean(as.numeric((pricesMTLR[lInd] >= test$Retail[lInd])) == test$Decision[lInd])
      classificationMTLRV = mean(as.numeric((pricesMTLR[vInd] >= test$Retail[vInd])) == test$Decision[vInd])
      classificationMTLR = mean(as.numeric((pricesMTLR >= test$Retail)) == test$Decision)
      
      classificationMTLRMat[i + (nRepeat-1)*10,] = c(classificationMTLRG,classificationMTLRH,classificationMTLRL,classificationMTLRV,classificationMTLR)
      curves = predict(modMTLR, test)

      profitG = findProfit(retail[testInd[[i]]][gInd], test$RP[gInd], curves[,-1][gInd], curves[,1])
      profitH = findProfit(retail[testInd[[i]]][hInd], test$RP[hInd], curves[,-1][hInd], curves[,1])
      profitL = findProfit(retail[testInd[[i]]][lInd], test$RP[lInd], curves[,-1][lInd], curves[,1])
      profitV = findProfit(retail[testInd[[i]]][vInd], test$RP[vInd], curves[,-1][vInd], curves[,1])
      profit = findProfit(retail[testInd[[i]]], test$RP, curves[,-1], curves[,1])
      
      profitMTLR[i,, nRepeat,1] = profitG
      profitMTLR[i,, nRepeat,2] = profitH
      profitMTLR[i,, nRepeat,3] = profitL
      profitMTLR[i,, nRepeat,4] = profitV
      profitMTLR[i,, nRepeat,5] = profit
      print(i)
      print(profitCox[i,,nRepeat,5] - profitMTLR[i,,nRepeat,5])
      }
    }

   
####################

findProfit = function(cost, reservationPrice, curves, predictedTimes){
  cprop = seq(0,.95,by = 0.05)
  times = seq(0,19, length.out = 500) #19 is the max reservation price across all chocolates
  probabilities = sapply(1:ncol(curves), function(x) predict_prob(curves[,x],predictedTimes, times))
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



