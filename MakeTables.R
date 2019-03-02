#This file was written by Humza Haider (hshaider@ualberta.ca) to make the tables/graphs for the paper utilizing survival predictions
#models for reservation price estimation.

#We will use xtable for latex tables.
library(xtable)


#Profit###############################################################################################################################

#I use a placeholder (i) to get the column I want depending on the table.
#i = 1,2,3,4,5 corresponds to "godiva","hersheys","lindt","valrhona", and "overall"
i =5
#Get all the profits and the mean profits:

#Surival Models:
profitRetail = readRDS("RDSFiles/profit/profitRetail");profitRetailM = apply(profitRetail,c(2,4),mean)[,i]
profitKM = readRDS("RDSFiles/profit/profitKM"); profitKMM = apply(profitKM,c(2,4),mean)[,i]
profitAFT = readRDS("RDSFiles/profit/profitAFT"); profitAFTM = apply(profitAFT,c(2,4),mean)[,i]
profitCox = readRDS("RDSFiles/profit/profitCox"); profitCoxM = apply(profitCox,c(2,4),mean)[,i]
profitMTLR = readRDS("RDSFiles/profit/profitMTLR"); profitMTLRM = apply(profitMTLR,c(2,4),mean)[,i]

#ML Models:
profitLR = readRDS("RDSFiles/profit/profitLR"); profitLRM = apply(profitLR,c(2,4),mean)[,i]
profitLDA = readRDS("RDSFiles/profit/profitLDA"); profitLDAM = apply(profitLDA,c(2,4),mean)[,i]
profitNB = readRDS("RDSFiles/profit/profitNB"); profitNBM = apply(profitNB,c(2,4),mean)[,i]

#Combine all the profits into one data frame, do Retail twice for a graph later on.
profitDataframe = rbind.data.frame(profitRetailM,profitRetailM,profitLRM,profitLDAM,profitNBM,
                                   profitKMM,profitAFTM,profitCoxM,profitMTLRM)
profitDataframe$Model = c("Retail","Retail", "LR","LDA","NB","KM","AFT","Cox","MTLR")
names(profitDataframe)  = c("0.00","0.05","0.10","0.15","0.20","0.25","0.30","0.35","0.40", "0.45","0.50",
                            "0.55","0.60","0.65","0.70", "0.75","0.80","0.85","0.90","0.95", "Model")
#Reorder data frame.
profitDataframe=profitDataframe[c(ncol(profitDataframe), 1:(ncol(profitDataframe)-1))]
profitDataframe$Type = c("Survival","ML","ML","ML","ML","Survival","Survival","Survival", "Survival")
#Here we shrink the table down and ignore the 0.00, 0.05, 0.95 cost of production price points.
profitDataframe = profitDataframe[-c(2,3,21)]

#This will give me a table for profits.
print(xtable(t(profitDataframe), digits = T),include.rownames=F)

#Now we will make some graphs.
library(ggplot2)
library(reshape2)
#Put the data into long form and order the model factos how I want them in the graph.
profitLong  = melt(profitDataframe,c("Model", "Type"))
profitLong$variable = as.numeric(as.character(profitLong$variable))
profitLong$Model = factor(profitLong$Model, levels = c("MTLR","Cox","AFT","KM",
                                                       "Retail","LR","LDA","NB"))
profitLong$Type = factor(profitLong$Type, levels = c("ML", "Survival"))

#mak the profit plot.
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
  

#Mean Average Error###################################################################################################################

#Get error tables.
errorBaseline = readRDS("RDSFiles/mae/errorBaseline")
errorKM = readRDS("RDSFiles/mae/errorKM")
errorAFT = readRDS("RDSFiles/mae/errorAFT")
errorCox = readRDS("RDSFiles/mae/errorCox")
errorMTLR = readRDS("RDSFiles/mae/errorMTLR")

errorToPlot = rbind.data.frame(errorBaseline,errorKM,errorAFT,errorCox,errorMTLR)
#rename columns to be correct.
names(errorToPlot)[c(2,5)] = c("Hershey's", "Overall")

#Give an indicator to model type.
errorToPlot$Model = rep(c("Base","KM","AFT","Cox","MTLR"), each = 100)

#Get long form data.
meltedError = melt(errorToPlot)
names(meltedError) = c("Model","Brand","Error")
meltedError$Model = factor(meltedError$Model, levels  = c("Base","KM","AFT","Cox","MTLR"))
meltedError$Brand = factor(meltedError$Brand, levels = c("Godiva","Lindt","Valrhona","Hershey's","Overall") )

#Make error plot.
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
  
#Classification Accuracy##############################################################################################################

#Get classification tables.
classificationBaseline = readRDS("classificationBaseline")
classificationKM = readRDS("classificationKM")
classificationAFT = readRDS("classificationAFT")
classificationCox = readRDS("classificationCox")
classificationMTLR = readRDS("classificationMTLR")
classificationToPlot = rbind.data.frame(classificationBaseline,classificationKM,classificationAFT,classificationCox,classificationMTLR)

#rename columns to be correct.
names(classificationToPlot)[c(2,5)] = c("Hershey's", "Overall")

#Give an indicator to model type.
classificationToPlot$Model = rep(c("Base","KM","AFT","Cox","MTLR"), each = 100)

#Get long form data.
meltedclassification = melt(classificationToPlot)
names(meltedclassification) = c("Model","Brand","classification")
meltedclassification$Model = factor(meltedclassification$Model, levels  = c("Base","KM","AFT","Cox","MTLR"))
meltedclassification$Brand = factor(meltedclassification$Brand, levels = c("Godiva","Lindt","Valrhona","Hershey's","Overall") )

#Make classification plot.
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