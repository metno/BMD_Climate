setwd("~/BMD/P_Temp/HOH/TempCorr")

require(RgoogleMaps)

BMD.Cor.St.Plot.Year.Temp <- function(St1=11111,MaxMin=1,StYear=1981,EndYear=2010){
  StList <- BMD.Reader.Station()
  Kart <- GetMap(center=c(23.5,90), zoom=7, destfile = "kart.png")
  StPos <- StList[StList[,10]==St1,5:6]
  tmp <- PlotOnStaticMap(Kart, lat = as.numeric(StPos[1]), lon = as.numeric(StPos[2]), destfile = "Kart1.png", cex=1.5,pch=20, add=FALSE)
  StId <- StList[,10]
  StId<- StId[StId!=St1]
  CorMat <- c()
  for(St2 in StId){
    #print(St2)
    StCorRes <- c(St1,St2,NA)
    try(StCorRes <-BMD.Cor.St2.Year.Temp(St1,St2,MaxMin=MaxMin,StYear = StYear, EndYear = EndYear),silent=TRUE)
    print(StCorRes)
    CorMat<-rbind(CorMat,
                  StCorRes)
    if(!is.na(StCorRes[3])){
      StPos2 <- StList[StList[,10]==St2,5:6]
      Latitudes <- c(as.numeric(StPos[1]),as.numeric(StPos2[1]))
      Longitudes <- c(as.numeric(StPos[2]),as.numeric(StPos2[2]))
      LCol="Purple"
      LWD=1
      if(StCorRes[3]>=0.4){LCol="Darkred";LWD=2}
      if(StCorRes[3]>=0.5){LCol="Red";LWD=2}
      if(StCorRes[3]>=0.6){LCol="Orange";LWD=2}
      if(StCorRes[3]>=0.7){LCol="Yellow";LWD=3}
      if(StCorRes[3]>=0.8){LCol="Green";LWD=3}
      if(StCorRes[3]>=0.9){LCol="Darkgreen";LWD=3}
      PlotOnStaticMap(Kart, lat = Latitudes, lon = Longitudes, destfile = "Kart1.png", FUN=lines, lwd=LWD, col=LCol, add=TRUE)
    }
  }
  CorMat

}

BMD.Cor.St2.Year.Temp <- function(St1=11111,St2=10609,MaxMin=1,StYear=1981,EndYear=2010){
  Data <- BMD.Data.Matcher.Temp(St1=St1,St2=St2,MaxMin=MaxMin,StYear=StYear,EndYear=EndYear,Month=TRUE,Day=FALSE,Norm=TRUE)
  Result <- c(St1,St2,
              cor(Data[,3],Data[,4],use="pairwise.complete.obs"))
  Result
}

BMD.Reader.Temp <- function(StNr=NA){#StNr is the local ID from BMD
  Data <- read.table("temp_2015.csv",header=TRUE,sep=",")
  if (!is.na(StNr)){Data<-Data[Data[,1]==StNr,]}
  Data
}

BMD.Reader.Station <- function(StNr=NA){
  Data <- read.csv("StList.csv",sep=";",dec=",")
  if (!is.na(StNr)){Data<-Data[Data[,10]==StNr,]}
  Data
}

BMD.Data.Matcher.Temp <- function(St1=11111,St2=10609,MaxMin=1,StYear=1981,EndYear=2010,Month=FALSE,Day=TRUE,Norm=FALSE){
  Cols <- c(1:4,(4+MaxMin))
  D1 <- BMD.Reader.Temp(StNr=St1)[,Cols]
  D2 <- BMD.Reader.Temp(StNr=St2)[,Cols]
  if (Month){
    Data <- BMD.Data.Matcher.Temp.Month(D1,D2,StYear=StYear,EndYear=EndYear,Norm=Norm)
  }
  if(Day){
    Data <- BMD.Data.Matcher.Temp.Day(D1,D2,StYear=StYear,EndYear=EndYear)
  }
  Data
}

BMD.Data.Matcher.Temp.Day <- function(D1,D2,StYear,EndYear){
  DaysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  Result <- c()
  for(Year in StYear:EndYear){
    for (m in 1:12){
      for (d in 1:DaysInMonth[m]){
        d1 <- NA
        d2 <- NA
        try(d1 <- D1[D1[,2]==Year & D1[,3]==m & D1[,4]==d,5],silent=T)
        try(d2 <- D2[D2[,2]==Year & D2[,3]==m & D2[,4]==d,5],silent=T)
        Result <- rbind(Result,
                        c(Year,m,d,d1,d2))
      }
    }
  }
  Result
}

BMD.Data.Matcher.Temp.Month <- function(D1,D2,StYear,EndYear,Norm=FALSE){
  DaysInMonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  Result <- c()
  for(Year in StYear:EndYear){
    for (m in 1:12){
        d1 <- NA
        d2 <- NA
        try(d1 <- mean(D1[D1[,2]==Year & D1[,3]==m,5],na.rm=TRUE),silent=T)
        try(d2 <- mean(D2[D2[,2]==Year & D2[,3]==m,5],na.rm=TRUE),silent=T)
        Result <- rbind(Result,
                        c(Year,m,d1,d2))
    }
  }
  if (Norm){
    Result <- BMD.Data.Normilasation.Temp.Month(Result)
  }
  Result
}

BMD.Data.Normilasation.Temp.Month <- function(D,StNorm=1981,EndNorm=2010){
  D2 <- D[D[,1]>=StNorm & D[,2]<=EndNorm,]
    Norms <- c()
    for(m in 1:12){
      Norms <- rbind(Norms,
                     colMeans(D2[D2[,2]==m,],na.rm = TRUE))
      for (Kol in 3:4){
        D[D[,2]==m,Kol] <- D[D[,2]==m,Kol]-Norms[m,Kol]}
    }
  D
}
