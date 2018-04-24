ACMECata=read.csv("C:/Users/Mrrobotics/Dropbox/stat analysis/ACMEcase.R",T,sep=",")
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

for(j in 1: length(qtypallets)){
  if (qtypallets[j,5] == "A"){
    qtyfullsa = qtyfullsa+ as.numeric(qtypallets[j,2])
    qtyhalvesa = qtyhalvesa + as.numeric(qtypallets[j,3])
    qtyquartersa = qtyquartersa + as.numeric(qtypallets[j,4])
  }
  if (qtypallets[j,5] == "B"){
    qtyfullsb = qtyfullsb+ as.numeric(qtypallets[j,2])
    qtyhalvesb = qtyhalvesb + as.numeric(qtypallets[j,3])
    qtyquartersb = qtyquartersb + as.numeric(qtypallets[j,4])
  }
  if (qtypallets[j,5] == "C"){
    qtyfullsc = qtyfullsc+ as.numeric(qtypallets[j,2])
    qtyhalvesc = qtyhalvesc + as.numeric(qtypallets[j,3])
    qtyquartersc = qtyquartersc + as.numeric(qtypallets[j,4])
  }
  if (qtypallets[j,5] == "D"){
    qtyfullsd = qtyfullsd +as.numeric(qtypallets[j,2])
    qtyhalvesd = qtyhalvesd + as.numeric(qtypallets[j,3])
    qtyquartersd = qtyquartersd + as.numeric(qtypallets[j,4])
  }
  if (qtypallets[j,5] == "E"){
    qtyfullse = qtyfullse+ as.numeric(qtypallets[j,2])
    qtyhalvese = qtyhalvese + as.numeric(qtypallets[j,3])
    qtyquarterse = qtyquarterse + as.numeric(qtypallets[j,4])
  }
}
print(paste0("A's   fulls  ", qtyfullsa , "   halves: ", qtyhalvesa , "  quarters", qtyquartersa)) 
print(paste0("B's   fulls  ", qtyfullsb , "   halves: ", qtyhalvesb , "  quarters", qtyquartersb))
print(paste0("C's   fulls  ", qtyfullsc , "   halves: ", qtyhalvesc , "  quarters", qtyquartersc))
print(paste0("D's   fulls  ", qtyfullsd , "   halves: ", qtyhalvesd , "  quarters", qtyquartersd))
print(paste0("E's   fulls  ", qtyfullse , "   halves: ", qtyhalvese , "  quarters", qtyquarterse))

