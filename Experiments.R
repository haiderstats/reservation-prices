#This R file was written by Humza Haider (hshaider@ualberta.ca) to analyze survival prediction models abillity to predict reservation
#prices. We compare Kaplan-Meier, Accelerated Failure Time, Cox-PH, Multi-Task Logistic Regression, and also compare profitabillity to
#Logisitic Regression, Naive Bayes, and Linear Discriminat Analysis. Below you will find the code used to conduct these experiments.

#Library calls#######################################################################################################################
library(icenReg) #Need for Cox, KM
library(e1071) #Need for Naive Bayes  
library(MASS) #Need for LDA
#devtools::install_github("haiderstats/MTLR")
library(MTLR) #Use for MTLR
library(survival) #Need for MTLR, AFT

#Helper Functions####################################################################################################################
source("helper.R") #This will give us the functions used to calculate the profits for each model.

#Data Preparation####################################################################################################################
#Read in all the data files and give brand indicator:
lindt = read.csv(paste("./Data/lindt",".csv",sep=""), header=F)
lindt$brand = "lindt"
hersheys = read.csv(paste("./Data/hersheys",".csv",sep=""), header=F)
hersheys$brand = "hersheys"
valrhona = read.csv(paste("./Data/valrhona",".csv",sep=""), header=F)
valrhona$brand = "valrhona"
godiva = read.csv(paste("./Data/godiva",".csv",sep=""), header=F)
godiva$brand = "godiva"

chocDat = rbind.data.frame(lindt,hersheys,valrhona,godiva)

#Calculate retail price(median reservation price).
lretail = median(lindt$V1)
hretail = median(hersheys$V1)
vretail = median(valrhona$V1)
gretail = median(godiva$V1)

retail = rep(c(lretail,hretail,vretail,gretail), each = 722)
#Give names to primary variables, the rest are just features.
names(chocDat)[1:3] = c("RP","Retail","Decision")
chocDat$Decision = ifelse(chocDat$Decision == -1,0,1)

#We will make empty dataframes to fill with results:
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

#Empty arrays to hold profit results. ncols will be the number of cost points.
ncols = length(seq(0,0.95, by = 0.05))
#Each of the profits will be a 10 folds x ncols x 10 repeats x 5 types (godiva,hersh,lindt,valrhona,total)
profitRetail = array(dim = c(10,ncols,10,5))
profitKM = array(dim = c(10,ncols,10,5))
profitAFT = array(dim = c(10,ncols,10,5)) 
profitCox = array(dim = c(10,ncols,10,5))
profitMTLR = array(dim = c(10,ncols,10,5))
profitLR = array(dim = c(10,ncols,10,5))
profitNB = array(dim = c(10,ncols,10,5))
profitLDA = array(dim = c(10,ncols,10,5))

set.seed(42)
#For our experiments we will do a 10 times repeated 10 fold cross validation. 
#Thus we have 2 loops, one doing the 10 repetitions and an inner loop doing the 10 fold CV.
for(nRepeat in 1:10){
  #create_folds is from the MTLR package, it stratifies the data by censor status.
  testInd = create_folds(chocDat$Retail, chocDat$Decision,10,foldtype = "censorstrat")
  for(i in 1:10){
    train = chocDat[-testInd[[i]],]
    test = chocDat[testInd[[i]],]
    #We will use these indicators to get the brand level errors.
    gInd = test$brand == "godiva"
    lInd = test$brand == "lindt"
    hInd = test$brand == "hersheys"
    vInd = test$brand == "valrhona"
    
    gIndT = train$brand == "godiva"
    lIndT = train$brand == "lindt"
    hIndT = train$brand == "hersheys"
    vIndT = train$brand == "valrhona"
    
    #Baseline Model##################################################################################################################

    #Mean Absolute Error (MAE)
    #Our baseline model computes the median over the training set and uses this as the estimate:
    errorBaselineG = mean(abs(median(train$RP[gIndT]) - test$RP[gInd]))
    errorBaselineH = mean(abs(median(train$RP[hIndT]) - test$RP[hInd]))
    errorBaselineL = mean(abs(median(train$RP[lIndT]) - test$RP[lInd]))
    errorBaselineV = mean(abs(median(train$RP[vIndT]) - test$RP[vInd]))
    #For the overall average we take the weighted average of averages.
    errorBaseline = (errorBaselineG*sum(gInd) + errorBaselineH*sum(hInd) + errorBaselineL*sum(lInd) +
                       errorBaselineV*sum(vInd))/nrow(test)
    errorBaselineMat[i + (nRepeat-1)*10,] = c(errorBaselineG,errorBaselineH,errorBaselineL,errorBaselineV,errorBaseline)
    
    #Classiciation Accuracy
    #Our baseline model takes the max of the purchase/nonpurchase and uses this as the guess.
    #To make the model slightly stronger we use the test set and not the training set 
    #though there shouldn't be any real difference.
    classificationBaselineG = max(sum(test$Decision[gInd]), length(test$Decision[gInd]) -
                                    sum(test$Decision[gInd]))/length(test$Decision[gInd])
    classificationBaselineH = max(sum(test$Decision[hInd]), length(test$Decision[hInd]) -
                                    sum(test$Decision[hInd]))/length(test$Decision[hInd])
    classificationBaselineL = max(sum(test$Decision[lInd]), length(test$Decision[lInd]) - 
                                    sum(test$Decision[lInd]))/length(test$Decision[lInd])
    classificationBaselineV = max(sum(test$Decision[vInd]), length(test$Decision[vInd]) -
                                    sum(test$Decision[vInd]))/length(test$Decision[vInd])
    
    #Take the weighted average of averages for the overall accuracy
    classificationBaseline = (classificationBaselineG*sum(gInd) + classificationBaselineH*sum(hInd) +
                                classificationBaselineL*sum(lInd) +classificationBaselineV*sum(vInd))/nrow(test)
    
    classificationBaselineMat[i + (nRepeat-1)*10,] = c(classificationBaselineG,classificationBaselineH,
                                                       classificationBaselineL,classificationBaselineV,classificationBaseline)
    #Profit
    #For the baseline profit, we use the retail price (median reservation price) as a fixed price for every customer.
    #If the retail price is below the reservation price, the profit is equal to that retail price minus the cost (which is a
    #proportion of the retail price).
    col = 0
    for(cost in seq(0,0.95,by = 0.05)){
      col = col+1
      profitG = mean((retail[testInd[[i]]][gInd]  <= test$RP[gInd]) * (retail[testInd[[i]]][gInd] - cost*(retail[testInd[[i]]][gInd])))
      profitH = mean((retail[testInd[[i]]][hInd]  <= test$RP[hInd]) * (retail[testInd[[i]]][hInd] - cost*(retail[testInd[[i]]][hInd])))
      profitL = mean((retail[testInd[[i]]][lInd]  <= test$RP[lInd]) * (retail[testInd[[i]]][lInd] - cost*(retail[testInd[[i]]][lInd])))
      profitV = mean((retail[testInd[[i]]][vInd]  <= test$RP[vInd]) * (retail[testInd[[i]]][vInd] - cost*(retail[testInd[[i]]][vInd])))
      #overll profit
      profit = mean((retail[testInd[[i]]]  <= test$RP) * (retail[testInd[[i]]] - cost*(retail[testInd[[i]]])))
      profitRetail[i,col, nRepeat,1] = profitG
      profitRetail[i,col, nRepeat,2] = profitH
      profitRetail[i,col, nRepeat,3] = profitL
      profitRetail[i,col, nRepeat,4] = profitV
      profitRetail[i,col, nRepeat,5] = profit
    }

    #Survival Models##################################################################################################################
    #Kaplan-Meier Model###############################################################################################################
    #For ic_np (from icen_Reg) we have to specify left censored observations as (0, event time) and right as (event time, Inf).
    l = ifelse(train$RP > train$Retail,train$Retail, 0)
    u = ifelse(train$RP > train$Retail,Inf, train$Retail)
    #Make 4 models, one for each chocolate brand.
    np_fit = ic_np(cbind(l, u) ~ brand, data = cbind.data.frame(l,u,brand= train$brand))
    KMcurveG = np_fit$scurves$godiva$S_curves$baseline
    KMcurveL = np_fit$scurves$lindt$S_curves$baseline
    KMcurveH = np_fit$scurves$hersheys$S_curves$baseline
    KMcurveV = np_fit$scurves$valrhona$S_curves$baseline
    
    #There was an issue with KM Curves not being monotonically decreasing due to machine prescicion so we put them in order here.
    KMcurveG = KMcurveG[order(KMcurveG
                              ,decreasing = T)]
    KMcurveL = KMcurveL[order(KMcurveL,decreasing = T)]
    KMcurveH = KMcurveH[order(KMcurveH,decreasing = T)]
    KMcurveV = KMcurveV[order(KMcurveV,decreasing = T)]
    
    KMtimesG = np_fit$scurves$godiva$Tbull_ints
    KMtimesL = np_fit$scurves$lindt$Tbull_ints
    KMtimesH = np_fit$scurves$hersheys$Tbull_ints
    KMtimesV = np_fit$scurves$valrhona$Tbull_ints
    
    #Find the median reservation price for each curve.
    indG = length(KMcurveG) - findInterval(.5,rev(KMcurveG))
    indL = length(KMcurveL) - findInterval(.5,rev(KMcurveL))
    indH = length(KMcurveH) - findInterval(.5,rev(KMcurveH))
    indV = length(KMcurveV) - findInterval(.5,rev(KMcurveV))
    
    medianG = KMtimesG[indG,2]
    medianL = KMtimesL[indL,2]
    medianH = KMtimesH[indH,2]
    medianV = KMtimesV[indV,2]
    
    #Mean Absolute Error:
    errorKMG = mean(abs(medianG - test$RP[gInd]))
    errorKML = mean(abs(medianL - test$RP[lInd]))
    errorKMH = mean(abs(medianH - test$RP[hInd]))
    errorKMV = mean(abs(medianV - test$RP[vInd]))
    
    errorKM = (errorKMG*sum(gInd) + errorKML*sum(lInd) + 
                 errorKMH*sum(hInd) +  errorKMV*sum(vInd))/nrow(test)
    
    errorKMMat[i + (nRepeat-1)*10,] = c(errorKMG,errorKMH,errorKML,errorKMV,errorKM)
    
    #Classification Accuracy:
    classificationKMG = mean(as.numeric((medianG >= test$Retail[test$brand == "godiva"])) == test$Decision[test$brand == "godiva"])
    classificationKML = mean(as.numeric((medianL >= test$Retail[test$brand == "lindt"])) == test$Decision[test$brand == "lindt"])
    classificationKMH = mean(as.numeric((medianH >= test$Retail[test$brand == "hersheys"])) == test$Decision[test$brand == "hersheys"])
    classificationKMV = mean(as.numeric((medianV >= test$Retail[test$brand == "valrhona"])) == test$Decision[test$brand == "valrhona"])
    
    classificationKM = (classificationKMG*length(which(test$brand == "godiva")) +
                          classificationKML*length(which(test$brand == "lindt")) + 
                          classificationKMH*length(which(test$brand == "hersheys")) +
                          classificationKMV*length(which(test$brand == "valrhona")))/nrow(test)
    
    classificationKMMat[i + (nRepeat-1)*10,] = c(classificationKMG,classificationKMH,classificationKML,
                                                 classificationKMV,classificationKM)
    #Profit
    #We will use the number of each chocolate to repeat the KM curve that many times:
    numG = sum(gInd)
    numH = sum(hInd)
    numL = sum(lInd)
    numV = sum(vInd)
    
    #Get profits for each brand:
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
    
    #Overall profit:
    profit = (numG*profitG + numH*profitH + numL*profitL +numV*profitV)/nrow(test)

    profitKM[i,, nRepeat,1] = profitG
    profitKM[i,, nRepeat,2] = profitH
    profitKM[i,, nRepeat,3] = profitL
    profitKM[i,, nRepeat,4] = profitV
    profitKM[i,, nRepeat,5] = profit
  
    #Cox##############################################################################################################################
    #l,u are needed again and were specified in the KM section.
    fit_ph <- ic_sp(cbind(l, u) ~ ., model = 'ph',
                    data = cbind.data.frame(l,u,train[-c(1,2,3)]))
    curvesCox = getSCurves(fit_ph, test)
    #Median reservation price
    medianCox = unname(sapply(curvesCox$S_curves, function(x) curvesCox$Tbull_ints[length(x) - findInterval(.5,rev(x)),2]))
    
    #Mean Average Error:
    errorCoxG = mean(abs(medianCox[gInd] - test$RP[gInd]))
    errorCoxH = mean(abs(medianCox[hInd] - test$RP[hInd]))
    errorCoxL = mean(abs(medianCox[lInd] - test$RP[lInd]))
    errorCoxV = mean(abs(medianCox[vInd] - test$RP[vInd]))
    errorCox = mean(abs(medianCox - test$RP))
    errorCoxMat[i + (nRepeat-1)*10,] = c(errorCoxG,errorCoxH,errorCoxL,errorCoxV,errorCox)
    
    #Classification Accuracy:
    classificationCoxG = mean(as.numeric((medianCox[gInd] >= test$Retail[gInd])) == test$Decision[gInd])
    classificationCoxH = mean(as.numeric((medianCox[hInd] >= test$Retail[hInd])) == test$Decision[hInd])
    classificationCoxL = mean(as.numeric((medianCox[lInd] >= test$Retail[lInd])) == test$Decision[lInd])
    classificationCoxV = mean(as.numeric((medianCox[vInd] >= test$Retail[vInd])) == test$Decision[vInd])
    classificationCox = mean(as.numeric((medianCox >= test$Retail)) == test$Decision)
    classificationCoxMat[i + (nRepeat-1)*10,] = c(classificationCoxG,classificationCoxH,classificationCoxL,classificationCoxV,classificationCox)
    
    #Profit:
    curves = as.data.frame(curvesCox$S_curves)
    curves = cbind.data.frame(curvesCox$Tbull_ints[,1], curves)
    curves = rbind.data.frame(c(0,rep(1, nrow(test))), curves)
    profitG = findProfit(retail[testInd[[i]]][gInd], test$RP[gInd], curves[,-1][gInd], curves[,1])
    profitH = findProfit(retail[testInd[[i]]][hInd], test$RP[hInd], curves[,-1][hInd], curves[,1])
    profitL = findProfit(retail[testInd[[i]]][lInd], test$RP[lInd], curves[,-1][lInd], curves[,1])
    profitV = findProfit(retail[testInd[[i]]][vInd], test$RP[vInd], curves[,-1][vInd], curves[,1])
    #Overall profit
    profit = findProfit(retail[testInd[[i]]], test$RP, curves[,-1], curves[,1])
    
    profitCox[i,, nRepeat,1] = profitG
    profitCox[i,, nRepeat,2] = profitH
    profitCox[i,, nRepeat,3] = profitL
    profitCox[i,, nRepeat,4] = profitV
    profitCox[i,, nRepeat,5] = profit

    #AFT##############################################################################################################################
    #For AFT we use the survreg function which we use "interval2" type censoring which uses NAs for left/right censoring.
    time1 = ifelse(train$RP > train$Retail,train$Retail, NA)
    time2 = ifelse(train$RP > train$Retail,NA, train$Retail)
    mod = survreg(Surv(time1,time2,type = "interval2")~., data = cbind.data.frame(time1,time2,train[-c(1,2,3)]), dist = "lognormal")
    
    #Get median price
    medianAFT = predict(mod,test,type = "quantile", p = .5)
    
    #Mean Average Error:
    errorAFTG = mean(abs(medianAFT[gInd] - test$RP[gInd]))
    errorAFTH = mean(abs(medianAFT[hInd] - test$RP[hInd]))
    errorAFTL = mean(abs(medianAFT[lInd] - test$RP[lInd]))
    errorAFTV = mean(abs(medianAFT[vInd] - test$RP[vInd]))
    errorAFT = mean(abs(medianAFT - test$RP))
    errorAFTMat[i + (nRepeat-1)*10,] = c(errorAFTG,errorAFTH,errorAFTL,errorAFTV,errorAFT)
    
    #Classification Accuracy:
    classificationAFTG = mean(as.numeric((medianAFT[gInd] >= test$Retail[gInd])) == test$Decision[gInd])
    classificationAFTH = mean(as.numeric((medianAFT[hInd] >= test$Retail[hInd])) == test$Decision[hInd])
    classificationAFTL = mean(as.numeric((medianAFT[lInd] >= test$Retail[lInd])) == test$Decision[lInd])
    classificationAFTV = mean(as.numeric((medianAFT[vInd] >= test$Retail[vInd])) == test$Decision[vInd])
    classificationAFT = mean(as.numeric((medianAFT >= test$Retail)) == test$Decision)
    classificationAFTMat[i + (nRepeat-1)*10,] = c(classificationAFTG,classificationAFTH,
                                                  classificationAFTL,classificationAFTV,classificationAFT)
    
    #Profit:
    #Get survival curves:
    survivalCurves = survfunc(mod, newdata = test, t = seq(0,16,length.out = 100))
    probabilities = survivalCurves$sur
    probabilityMatrix = matrix(probabilities, ncol = nrow(test),byrow = T)
    curves = cbind.data.frame(time= seq(0,16,length.out = 100), probabilityMatrix)
    
    profitG = findProfit(retail[testInd[[i]]][gInd], test$RP[gInd], curves[,-1][gInd], curves[,1])
    profitH = findProfit(retail[testInd[[i]]][hInd], test$RP[hInd], curves[,-1][hInd], curves[,1])
    profitL = findProfit(retail[testInd[[i]]][lInd], test$RP[lInd], curves[,-1][lInd], curves[,1])
    profitV = findProfit(retail[testInd[[i]]][vInd], test$RP[vInd], curves[,-1][vInd], curves[,1])
    #Overall profit:
    profit = findProfit(retail[testInd[[i]]], test$RP, curves[,-1], curves[,1])
    
    profitAFT[i,, nRepeat,1] = profitG
    profitAFT[i,, nRepeat,2] = profitH
    profitAFT[i,, nRepeat,3] = profitL
    profitAFT[i,, nRepeat,4] = profitV
    profitAFT[i,, nRepeat,5] = profit
    
    #MTLR#############################################################################################################################
    #MTLR also uses interval2 type censoring so we make use of time1 and time2 again.
    #First we do internal CV to find a regularization parameter. Note we dont train biases or uncensored data first
    #since all the data is censored.
    #Finding C1 takes awhile, maybe ~5 minutes per iteration.
    C1 = mtlr_cv(Surv(time1,time2,type = "interval2")~., data = cbind.data.frame(time1,time2,train[-c(1,2,3)]),
                 C1_vec = seq(.2,.4,length.out = 9),train_biases = F, train_uncensored = F, verbose =F,
                 threshold = .000001, nfolds = 3)
    C1 = C1$best_C1      
    modMTLR = mtlr(Surv(time1,time2,type = "interval2")~., data = cbind.data.frame(time1,time2,train[-c(1,2,3)]),C1 = C1,
                   train_biases = F, train_uncensored = F, threshold = .000001)
    medianMTLR = predict(modMTLR,test, type = "median")
    
    #Mean Average Error:
    errorMTLRG = mean(abs(medianMTLR[gInd] - test$RP[gInd]))
    errorMTLRH = mean(abs(medianMTLR[hInd] - test$RP[hInd]))
    errorMTLRL = mean(abs(medianMTLR[lInd] - test$RP[lInd]))
    errorMTLRV = mean(abs(medianMTLR[vInd] - test$RP[vInd]))
    errorMTLR = mean(abs(medianMTLR - test$RP))
    errorMTLRMat[i + (nRepeat-1)*10,] = c(errorMTLRG,errorMTLRH,errorMTLRL,errorMTLRV,errorMTLR)
    
    #Classification Accuracy:
    classificationMTLRG = mean(as.numeric((medianMTLR[gInd] >= test$Retail[gInd])) == test$Decision[gInd])
    classificationMTLRH = mean(as.numeric((medianMTLR[hInd] >= test$Retail[hInd])) == test$Decision[hInd])
    classificationMTLRL = mean(as.numeric((medianMTLR[lInd] >= test$Retail[lInd])) == test$Decision[lInd])
    classificationMTLRV = mean(as.numeric((medianMTLR[vInd] >= test$Retail[vInd])) == test$Decision[vInd])
    classificationMTLR = mean(as.numeric((medianMTLR >= test$Retail)) == test$Decision)
    
    classificationMTLRMat[i + (nRepeat-1)*10,] = c(classificationMTLRG,classificationMTLRH,classificationMTLRL,
                                                   classificationMTLRV,classificationMTLR)
    
    #Profit:
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
    
    #ML MODELS########################################################################################################################
    #Here we only calcualte profit and not MAE or classification accuracy.
    
    #Logistic Regression####
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
    
    #Linear Discriminant Analysis#####################################################################################################
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
    
    #Naive Bayes######################################################################################################################
    #Naiver Bayes takes awhile to run for the *findProfit()* function, maybe ~3 minutes to complete all 4 runs.
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
    
    #Print Progress####
    #There will be 10 repetitons of (1,2,3,...10).
    print(i)
  }
}

#Save Data############################################################################################################################
#Now that we have finished the experiment we will save the experimental results.

#Save MAE:
saveRDS(errorBaselineMat, "RDSFiles/mae/errorBaseline")
saveRDS(errorKMMat, "RDSFiles/mae/errorKM")
saveRDS(errorAFTMat, "RDSFiles/mae/errorAFT")
saveRDS(errorCoxMat, "RDSFiles/mae/errorCox")
saveRDS(errorMTLRMat, "RDSFiles/mae/errorMTLR")

#Save Classification Accuracy:
saveRDS(classificationBaselineMat, "RDSFiles/classification/classificationBaseline")
saveRDS(classificationKMMat, "RDSFiles/classification/classificationKM")
saveRDS(classificationAFTMat, "RDSFiles/classification/classificationAFT")
saveRDS(classificationCoxMat, "RDSFiles/classification/classificationCox")
saveRDS(classificationMTLRMat, "RDSFiles/classification/classificationMTLR")

#Save Profit:
saveRDS(profitRetail, "RDSFiles/profit/profitRetaj,")
saveRDS(profitKM, "RDSFiles/profit/profitKM")
saveRDS(profitAFT, "RDSFiles/profit/profitAFT")
saveRDS(profitCox, "RDSFiles/profit/profitCox")
saveRDS(profitMTLR, "RDSFiles/profit/profitMTLR")
saveRDS(profitLR, "RDSFiles/profit/profitLR")
saveRDS(profitLDA, "RDSFiles/profit/profitLDA")
saveRDS(profitNB, "RDSFiles/profit/profitNB")
