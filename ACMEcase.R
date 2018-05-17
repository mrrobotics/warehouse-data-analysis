library(tidyverse)
library(forcats)
library(stringr)
library(caTools)

library(DT)
library(data.table)
library(pander)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(corrplot)
library(VIM) 
library(knitr)
library(vcd)
library(caret)

library(xgboost)
library(MLmetrics)
library('randomForest') 
library('rpart')
library('rpart.plot')
library('car')
library('e1071')
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet) 
library(alluvial)


#ACMECase=read.csv("C:/Users/Mrrobotics/Dropbox/stat analysis/ACMEcase.csv",T,sep=",")
attach(ACMECase)
numsku<-length(sku)
numsku
avgcube <- mean(`skucube (ft^3)`)
avgcube
avgcrtqty <- mean(ctnqty)
avgcrtqty
palwidth <- mean(`palwid (in)`)
pallength <- mean(`pallen (in)`)
palhight <- mean(`palht (in)`)
print(paste0("length ",pallength, "     width ", palwidth, "     height ",palhight))
palsize <- mean(palletctn)
palsize

##avg cost of goods sold according to the description is 25$

qtypallets <- matrix(0,numsku,5) 
for(j in 1:length(sku)){
  tempqty = qtyonhand[j]
  tempcarton = ctnqty[j]
  temppal = palletctn[j]
  tempcarts <- ceiling(tempqty/tempcarton)
  tempfulls <- tempcarts%/%temppal
  tempfullsremainder <- tempcarts %% temppal
  temphalves <- tempfullsremainder %/% (temppal/2)
  temphalvesremainder <- tempfullsremainder %% (temppal/2)
  tempquarters <- ceiling(temphalvesremainder/(temppal/4))
  qtypallets[j,1] <- sku[j]
  qtypallets[j,2] <- tempfulls
  qtypallets[j,3] <- temphalves
  qtypallets[j,4] <- tempquarters
  qtypallets[j,5]<-abcdcateg[j]
}
qtyfulls <- sum(as.numeric(qtypallets[,2]))
qtyhalves <- sum(as.numeric(qtypallets[,3]))
qtyquarters <- sum(as.numeric(qtypallets[,4]))
print(paste0("fulls  ", qtyfulls , "   halves: ", qtyhalves , "  quarters", qtyquarters))
qtyfullsa<-0
qtyhalvesa<-0
qtyquartersa<-0
qtyfullsb<-0
qtyhalvesb<-0
qtyquartersb<-0
qtyfullsc<-0
qtyhalvesc<-0
qtyquartersc<-0
qtyfullsd<-0
qtyhalvesd<-0
qtyquartersd<-0
qtyfullse<-0
qtyhalvese<-0
qtyquarterse<-0
palsoldA <-0
palsoldB <- 0
ctnsoldA<- 0
ctnsoldB <- 0
eachsoldA <- 0
eachsoldB <- 0
palsoldC <-0
palsoldD <- 0
ctnsoldC<- 0
ctnsoldD <- 0
eachsoldC <- 0
eachsoldD <- 0
palsoldE <-0
ctnsoldE<- 0
eachsoldE <- 0
askus <-0
bskus <- 0


for(j in 1: length(qtypallets)){
  if (qtypallets[j,5] == "A"){
    qtyfullsa = qtyfullsa+ as.numeric(qtypallets[j,2])
    qtyhalvesa = qtyhalvesa + as.numeric(qtypallets[j,3])
    qtyquartersa = qtyquartersa + as.numeric(qtypallets[j,4])
    palsoldA = palsoldA+palsold[j]*12*16
    ctnsoldA = ctnsoldA + ctnsold[j]*12
    eachsoldA = eachsoldA + eachsold[j]
    askus = askus +1
  }
  if (qtypallets[j,5] == "B"){
    qtyfullsb = qtyfullsb+ as.numeric(qtypallets[j,2])
    qtyhalvesb = qtyhalvesb + as.numeric(qtypallets[j,3])
    qtyquartersb = qtyquartersb + as.numeric(qtypallets[j,4])
    palsoldB = palsoldB+palsold[j]*12*16
    ctnsoldB = ctnsoldB + ctnsold[j]*12
    eachsoldB = eachsoldB + eachsold[j]
    bskus = bskus + 1
  }
  if (qtypallets[j,5] == "C"){
    qtyfullsc = qtyfullsc+ as.numeric(qtypallets[j,2])
    qtyhalvesc = qtyhalvesc + as.numeric(qtypallets[j,3])
    qtyquartersc = qtyquartersc + as.numeric(qtypallets[j,4])
    palsoldC = palsoldC+palsold[j]*12*16
    ctnsoldC = ctnsoldC + ctnsold[j]*12
    eachsoldC = eachsoldC + eachsold[j]
  }
  if (qtypallets[j,5] == "D"){
    qtyfullsd = qtyfullsd +as.numeric(qtypallets[j,2])
    qtyhalvesd = qtyhalvesd + as.numeric(qtypallets[j,3])
    qtyquartersd = qtyquartersd + as.numeric(qtypallets[j,4])
    palsoldD = palsoldD+palsold[j]*12*16
    ctnsoldD = ctnsoldD + ctnsold[j]*12
    eachsoldD = eachsoldD + eachsold[j]
  }
  if (qtypallets[j,5] == "E"){
    qtyfullse = qtyfullse+ as.numeric(qtypallets[j,2])
    qtyhalvese = qtyhalvese + as.numeric(qtypallets[j,3])
    qtyquarterse = qtyquarterse + as.numeric(qtypallets[j,4])
    palsoldE = palsoldE+palsold[j]*12*16
    ctnsoldE = ctnsoldE + ctnsold[j]*12
    eachsoldE = eachsoldE + eachsold[j]
  }
}
print(paste0("A's   fulls  ", qtyfullsa , "   halves: ", qtyhalvesa , "  quarters", qtyquartersa)) 
print(paste0("B's   fulls  ", qtyfullsb , "   halves: ", qtyhalvesb , "  quarters", qtyquartersb))
print(paste0("C's   fulls  ", qtyfullsc , "   halves: ", qtyhalvesc , "  quarters", qtyquartersc))
print(paste0("D's   fulls  ", qtyfullsd , "   halves: ", qtyhalvesd , "  quarters", qtyquartersd))
print(paste0("E's   fulls  ", qtyfullse , "   halves: ", qtyhalvese , "  quarters", qtyquarterse))

print(paste0("A items sold ", palsoldA , " items in pallets, ", ctnsoldA , " items in cartons, and " , eachsoldA , " as eaches"))
print(paste0("B items sold ", palsoldB , " items in pallets, ", ctnsoldB , " items in cartons, and " , eachsoldB , " as eaches"))
print(paste0("C items sold ", palsoldC , " items in pallets, ", ctnsoldC , " items in cartons, and " , eachsoldC , " as eaches"))
print(paste0("D items sold ", palsoldD , " items in pallets, ", ctnsoldD , " items in cartons, and " , eachsoldD , " as eaches"))
print(paste0("E items sold ", palsoldE , " items in pallets, ", ctnsoldE , " items in cartons, and " , eachsoldE , " as eaches"))


#as cartons
print(paste0("A items sold ", palsoldA/12 , " items in pallets, ", ctnsoldA/12 , " items in cartons, and " , eachsoldA/12 , " as eaches in terms of cartons"))
print(paste0("B items sold ", palsoldB/12 , " items in pallets, ", ctnsoldB/12 , " items in cartons, and " , eachsoldB/12 , " as eaches in terms of cartons"))
print(paste0("C items sold ", palsoldC/12 , " items in pallets, ", ctnsoldC/12 , " items in cartons, and " , eachsoldC/12 , " as eaches in terms of cartons"))
print(paste0("D items sold ", palsoldD/12 , " items in pallets, ", ctnsoldD/12 , " items in cartons, and " , eachsoldD/12 , " as eaches in terms of cartons"))
print(paste0("E items sold ", palsoldE/12 , " items in pallets, ", ctnsoldE/12 , " items in cartons, and " , eachsoldE/12 , " as eaches in terms of cartons"))


print(paste0("A items sold ", palsoldA/(12*16) , " items in pallets, ", ctnsoldA/(12*16) , " items in cartons, and " , eachsoldA/(12*16) , " as eaches in terms of pallets"))
print(paste0("B items sold ", palsoldB/(12*16) , " items in pallets, ", ctnsoldB/(12*16) , " items in cartons, and " , eachsoldB/(12*16) , " as eaches in terms of pallets"))
print(paste0("C items sold ", palsoldC/(12*16) , " items in pallets, ", ctnsoldC/(12*16) , " items in cartons, and " , eachsoldC/(12*16) , " as eaches in terms of pallets"))
print(paste0("D items sold ", palsoldD/(12*16) , " items in pallets, ", ctnsoldD/(12*16) , " items in cartons, and " , eachsoldD/(12*16) , " as eaches in terms of pallets"))
print(paste0("E items sold ", palsoldE/(12*16) , " items in pallets, ", ctnsoldE/(12*16) , " items in cartons, and " , eachsoldE/(12*16) , " as eaches in terms of pallets"))

palworthA = (palsoldA+ctnsoldA+eachsoldA)/(12*16*249)
palworthB = (palsoldB+ctnsoldB+eachsoldB)/(12*16*249)
palworthA
palworthB

askus 
bskus

eachsoldA/(249*7.5)
eachsoldB/(249*7.5)

ACMECase <- as.data.frame(ACMECase)

tbl_summary <- ACMECase %>%
  group_by(abcdcateg,abcdea, abcdctn,abcdpal) %>%
  summarise(N = n()) %>%
  ungroup %>%
  na.omit

alluvial(tbl_summary[,c(1:4)],
         freq = tbl_summary$N/10,
         col = "blue",
         cex = 0.75,
         border = "red",
         blocks = TRUE)

ggplot(ACMECase, aes(abcdcateg)) + ## initializes GGplot on the data ACMECase, with a focus on the abcdcateg data
  geom_bar(position = "stack") + ##sets the graph type. in this case: a bar graph
  scale_y_continuous(labels = comma) + ## sets how the y axis presents its scale. in this case, as commas
  xlab("SKU Category") + ## sets the label for the X axis
  ylab("number of sku's") + ## sets the label for the y axis
  ggtitle("ACME Shoe Co. number of sku's per SKU type") + ## sets the graph title. 
  theme_minimal()