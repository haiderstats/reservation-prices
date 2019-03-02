library(xtable)
i =4
profitRetail = readRDS("profitRetail");profitRetailM = apply(profitRetail,c(2,4),mean)[,i]

profitKM = readRDS("profitKM"); profitKMM = apply(profitKM,c(2,4),mean)[,i]
profitAFT = readRDS("profitAFT"); profitAFTM = apply(profitAFT,c(2,4),mean)[,i]
profitCox = readRDS("profitCox"); profitCoxM = apply(profitCox,c(2,4),mean)[,i]
profitMTLR = readRDS("profitMTLR"); profitMTLRM = apply(profitMTLR,c(2,4),mean)[,i]

profitLR = readRDS("profitLR"); profitLRM = apply(profitLR,c(2,4),mean)[,i]
#profitLRNet = readRDS("profitLRNet"); profitLRNetM = apply(profitLRNet,c(2,4),mean)[,i]
profitLDA = readRDS("profitLDA"); profitLDAM = apply(profitLDA,c(2,4),mean)[,i]
profitNB = readRDS("profitNB"); profitNBM = apply(profitNB,c(2,4),mean)[,i]

profitDataframe = rbind.data.frame(profitRetailM,profitRetailM,profitLRM,profitLDAM,profitNBM,profitKMM,profitAFTM,profitCoxM,profitMTLRM)
profitDataframe$Model = c("Retail","Retail", "LR","LDA","NB","KM","AFT","Cox","MTLR")
names(profitDataframe)  = c("0.00","0.05","0.10","0.15","0.20","0.25","0.30","0.35","0.40", "0.45","0.50",
                            "0.55","0.60","0.65","0.70", "0.75","0.80","0.85","0.90","0.95", "Model")
profitDataframe=profitDataframe[c(ncol(profitDataframe), 1:(ncol(profitDataframe)-1))]
profitDataframe$Type = c("Survival","ML","ML","ML","ML","Survival","Survival","Survival", "Survival")
profitDataframe = profitDataframe[-c(2,3,21)]
profitRed = profitDataframe[-c(2,3,4,5,21)]
print(xtable(t(profitDataframe), digits = 3),include.rownames=F)
print(xtable(profitRed, digits = 2),include.rownames=F)

library(ggplot2)
library(reshape2)
profitLong  = melt(profitDataframe,c("Model", "Type"))
profitLong$variable = as.numeric(as.character(profitLong$variable))
profitLong$Model = factor(profitLong$Model, levels = c("MTLR","Cox","AFT","KM",
                                                       "Retail","LR","LDA","NB"))
profitLong$Type = factor(profitLong$Type, levels = c("ML", "Survival"))
library(wesanderson)
ggplot(profitLong, aes(variable,value, group = Model, color = Model))+
  geom_point(size =2) + 
  geom_line(size= 0.75) + 
  facet_grid(~Type)+
  scale_x_continuous(breaks = seq(0.1,0.9,by = 0.1)) +
  scale_y_continuous(breaks = c(0.25,0.5,0.75,1.0,1.25,1.5))+
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey95")) + 
  labs(y = "Profit", x = "Cost of Production (Proportion of Retail Price)")+
  geom_vline(xintercept=0.75,linetype = "dashed") + 
  scale_color_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A",
                                "#984EA3", "magenta1", "tomato2", "#A65628",
                     "cornflowerblue"))+
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  theme(text = element_text(size = 15, face = "bold"))
  


errorBaseline = readRDS("errorBaseline")
errorKM = readRDS("errorKM")
errorAFT = readRDS("errorAFT")
errorCox = readRDS("errorCox")
errorMTLR = readRDS("errorMTLR")

errorToPlot = rbind.data.frame(errorBaseline,errorKM,errorAFT,errorCox,errorMTLR)
names(errorToPlot)[c(2,5)] = c("Hershey's", "Overall")
errorToPlot=errorToPlot
errorToPlot$Model = rep(c("Base","KM","AFT","Cox","MTLR"), each = 100)
library(reshape2)
meltedError = melt(errorToPlot)
names(meltedError) = c("Model","Brand","Error")
meltedError$Model = factor(meltedError$Model, levels  = c("Base","KM","AFT","Cox","MTLR"))
meltedError$Brand = factor(meltedError$Brand, levels = c("Godiva","Lindt","Valrhona","Hershey's","Overall") )
ggplot(meltedError, aes(x = Model, y=Error, fill = Model)) +
  geom_boxplot() + 
  facet_grid(~Brand)+
  theme_bw() + 
  labs(fill = "")+
  scale_y_continuous(breaks = c(0,0.5,1,1.5,2,2.5), limits = c(.4,2.8))+
  theme(axis.title  = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = "top",
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size=  13, face = "bold"),
        axis.text = element_text(size = 9, face = "bold")) 
  



classificationBaseline = readRDS("classificationBaseline")
classificationKM = readRDS("classificationKM")
classificationAFT = readRDS("classificationAFT")
classificationCox = readRDS("classificationCox")
classificationMTLR = readRDS("classificationMTLR")

classificationToPlot = rbind.data.frame(classificationBaseline,classificationKM,classificationAFT,classificationCox,classificationMTLR)
names(classificationToPlot)[c(2,5)] = c("Hershey's", "Overall")
classificationToPlot=classificationToPlot
classificationToPlot$Model = rep(c("Base","KM","AFT","Cox","MTLR"), each = 100)
library(reshape2)
meltedclassification = melt(classificationToPlot)
names(meltedclassification) = c("Model","Brand","classification")
meltedclassification$Model = factor(meltedclassification$Model, levels  = c("Base","KM","AFT","Cox","MTLR"))
meltedclassification$Brand = factor(meltedclassification$Brand, levels = c("Godiva","Lindt","Valrhona","Hershey's","Overall") )
ggplot(meltedclassification, aes(x = Model, y=classification, fill = Model)) +
  geom_boxplot() + 
  facet_grid(~Brand)+
  theme_bw() + 
  labs(fill = "", y = "Classification Accuracy")+
  scale_y_continuous(breaks = c(0.5,0.6,0.7,0.8,0.9))+
  theme(axis.title  = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 15, face = "bold"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = "top",
        strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(size=  13, face = "bold"),
        axis.text = element_text(size = 9, face = "bold")) 






