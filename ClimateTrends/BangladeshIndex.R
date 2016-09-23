#BangladeshTrend

#source("BMDReader.R") #Getting the functions of the BMDReader file into the memory to further use
#setwd("C:/Users/hansoh/Documents/BMD/P_Temp/HOH")

BMD.M.Deviation <- function(StNr=923,StYear=1981,EndYear=2010,StYearN=1981,EndYearN=2010){
  Filename = paste("tm",StNr,".csv",sep="") #Constructin the filename
  StId <- 41000 + StNr #Using the shot verion of the number to make the komplete WMO number
  Obs <- BMDTempReader(file=Filename) #Reading the max and min temperature.
  Mnd <- cbind(Obs[,1:4],rowMeans(Obs[,5:35],na.rm=TRUE),NA)
  Mnd <- Mnd[Mnd[,2]>=StYear & Mnd[,2]<=EndYear,]
  MndIndex <- cbind(Mnd[Mnd[,4]==1,1:3],(Mnd[Mnd[,4]==1,5:6]+Mnd[Mnd[,4]==2,5:6])/2)
  MndIndex[is.nan(MndIndex[,4]),4] <- NA
  Norm <- c()
  #Creating normals
  for (m in 1:12){
    #print(MndIndex[MndIndex[,3]==m,4])
    Norm <- c(Norm,
              mean(MndIndex[MndIndex[,2]<=EndYearN &
                              MndIndex[,2]>=StYearN &
                              MndIndex[,3]==m &
                              !is.na(MndIndex[,4]),4],na.rm = TRUE))
  }
  #Create deviations from normals
  #print(Norm)
  for (m in 1:12){
    MndIndex[MndIndex[,3]==m,5] <- MndIndex[MndIndex[,3]==m,4] - Norm[m]
  }
  MndIndex
}

BangladeshIndex <- function(StNrlist=c(923,923,923),StYear=1981,EndYear=2010,StYearN=1981,EndYearN=2010){
  Resultat <- c()
  LS <- length(StNrlist)
  for(Y in StYear:EndYear){
    for (m in 1:12){
      Resultat <- rbind(Resultat, c(Y,m,array(NA,LS)))
    }
  }
  for(n in 1:LS){
    StationData <- BMD.M.Deviation(StNrlist[n],StYear=StYear,EndYear=EndYear,StYearN=StYearN,EndYearN=EndYearN)
    for(Y in StYear:EndYear){
      for (m in 1:12){
        if (length(StationData[StationData[,2]==Y & StationData[,3]==m,5])>0){
          Resultat[Resultat[,1]==Y & Resultat[,2]==m,n+2]<-StationData[StationData[,2]==Y & StationData[,3]==m,5]
        }
      }
    }
  }
  #Resultat <- cbind(StationData[,2:3],Resultat)
  #print(Resultat)
  BangladeshID <- cbind(Resultat[,1:2],rowMeans(Resultat[,3:(LS+2)],na.rm = T),NA)
  LBI <- length(BangladeshID[,1])
  for (n in 1:LBI){
    #print(Resultat[n,])
    V2 <- Resultat[n,]
#    print(V2[!is.na(V2)])
    BangladeshID[n,4]<-length(V2[!is.na(V2)])-2
  }
  BangladeshID
}

BangladeshIndexYear <- function(StNrlist=c(923,859,858),StYear=1981,EndYear=2010,StYearN=1981,EndYearN=2010){
  Data <- BangladeshIndex(StNrlist = StNrlist,StYear = StYear,EndYear = EndYear,StYearN = StYear,EndYearN = EndYear)
  #print(Data)
  Resultat <- c()
  for(Y in StYear:EndYear){
    Resultat <- rbind(Resultat,
                      c(Y,
                        mean(Data[Data[,1]==Y,3]),
                        mean(Data[Data[,1]==Y,4])))
  }
  #print("Hi")
  Modell <- lm(Resultat[,2]~Resultat[,1])
  print(summary(Modell))
  f <- summary(Modell)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  Resultat
  subtitle <- paste("Number of stations: ", length(StNrlist),
                    ", Trend: ", signif(Modell$coefficients[2]*10,2),
                    "C/dec, p-value: ", signif(p,2))
  plot(Resultat[,1],Resultat[,2],type="l",col="darkred",main="Temperaturechange in Bangladesh",ylab="Temperature deviation", xlab="",sub=subtitle,lwd=2)
  lines(c(0,10000),c(Modell$coefficients[1],Modell$coefficients[1]+Modell$coefficients[2]*10000),lwd=3)
  plotpar <- par()
  ymin <- plotpar$yaxp[1]
  ymax <- plotpar$yaxp[2]
  yrange <- ymax - ymin
  DataMin <- min(Resultat[,2],na.rm=T)
  Colors <- c("red","orange","yellow","green")
  points(Resultat[,1],array(DataMin-yrange/25,length(Resultat[,1])),col=Colors[round(Resultat[,3]*4/length(StNrlist))],pch=16)
  print(Resultat*4/length(StNrlist))
}

BangladeshIndexYearSeason <- function(StNrlist=c(923,923,923),StYear=1981,EndYear=2010,StYearN=1981,EndYearN=2010,months=c(12,1,3)){
  Data <- BangladeshIndex(StNrlist = StNrlist,StYear = StYear,EndYear = EndYear,StYearN = StYear,EndYearN = EndYear)
  #print(Data)
  D2 <- c()
  for (m in months){
    D2 <- rbind(D2,Data[Data[,2]==m,])
  }
  Data <- D2
  Resultat <- c()
  for(Y in StYear:EndYear){
    Resultat <- rbind(Resultat,
                      c(Y,
                        mean(Data[Data[,1]==Y,3]),
                        mean(Data[Data[,1]==Y,4])))
  }
  Modell <- lm(Resultat[,2]~Resultat[,1])
  print(summary(Modell))
  f <- summary(Modell)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  Resultat
  subtitle <- paste("Number of stations: ", length(StNrlist),
                    ", Trend: ", signif(Modell$coefficients[2]*10,2),
                    "C/dec, p-value: ", signif(p,2))
  plot(Resultat[,1],Resultat[,2],type="l",col="darkred",main="Temperaturechange in Bangladesh",ylab="Temperature deviation", xlab="",sub=subtitle,lwd=2)
  lines(c(0,10000),c(Modell$coefficients[1],Modell$coefficients[1]+Modell$coefficients[2]*10000),lwd=3)
}

#BangladeshIndexYear(c(858,859,863,883,886,891,895,907,909,915,923,926,929,933,936,939,941,943,946,947,950,951,953,958,960,963,964,965,966,978,984,989,992,998))
