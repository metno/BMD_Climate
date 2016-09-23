
BMDTempReader <- function(file="tm895.csv"){
# test <-  read.table(file,widths=c(5,5,3,array(4,31)),sep",",dec=".",header=FALSE)
  test <- read.table(file,sep=",",dec=".",header=FALSE)
  test[test==-1]<-NA
#  test2 <- cbind(test[1:3],c(1:2),test[4:34]/10)
  test2 <- cbind(test[1:3],c(1:2),test[4:34])
  test2
}

BMDTempRearranger <- function(file="tm895.csv"){##MM=1 => max
  Data <- BMDTempReader(file)
  print(dim(Data))
  Max <- Data[Data[,4]==1,]
  Min <- Data[Data[,4]==2,]
  print(dim(Data))
  LD <- length(Min[,1])
  L<-31
  NewData <- c()
  for (Row in 1:LD){
    m <- Min[Row,3]
    y <- Min[Row,2]
    print(c(m,y))
      if (m==1){L<-31}
      if (m==2){L<-28}
      if (m==3){L<-31}
      if (m==4){L<-30}
      if (m==5){L<-31}
      if (m==6){L<-30}
      if (m==7){L<-31}
      if (m==8){L<-31}
      if (m==9){L<-30}
      if (m==10){L<-31}
      if (m==11){L<-30}
      if (m==12){L<-31}
    if (y/4==round(y/4) & m==2){L<-29}
      D2 <- cbind(array(Min[Row,2],L),
                  array(Min[Row,3],L),
                  c(1:L),
                  t(Min[Row,5:(L+4)]),
                  t(Max[Row,5:(L+4)]))
      NewData <- rbind(NewData,D2)
    }
  NewData
}

BMDTempDevYr <- function(file="tm895.csv"){ 
  data <- BMDTempReader(file=file)
  Years <- unique(data[,2])
  DataYear<-c()
  for (year in Years){
    DataYear <- rbind(DataYear,
                      c(year,
                        mean(colMeans(data[data[,2]==year&data[,4]==1,5:35],na.rm=T),na.rm=T),
                        mean(colMeans(data[data[,2]==year,5:35],na.rm=T),na.rm=T),
                        mean(colMeans(data[data[,2]==year&data[,4]==2,5:35],na.rm=T),na.rm=T)))
  }
  plot(DataYear[,1],DataYear[,2],
       main=data[1,1],ylab="Temperature",xlab="year",pch=19,col="darkred",
      # ylim=c(20,32))
       ylim=c(18,34))
  points(DataYear[,1],DataYear[,3],pch=19,col="darkgreen")
  points(DataYear[,1],DataYear[,4],pch=19,col="darkblue")
  lines(c(0,20000),c(25,25),lty=3)
  lines(c(0,20000),c(30,30),lty=3)
  lines(c(0,20000),c(20,20),lty=3)
  DataYear
}

BMDTempDevLimit <- function(file="tm895.csv",Limit=30){ #tm960
  data <- BMDTempReader(file=file)
  D2 <- data[,5:35]
  LD <- length(data[,1])
  D2 <- floor(D2/Limit)
  data <- cbind(data[,1:4],D2)
  Years <- unique(data[,2])
  DataYear<-c()
  for (year in Years){
    DataYear <- rbind(DataYear,
                      c(year,
                        sum(data[data[,2]==year&data[,4]==1,5:35],na.rm=T),
                        sum(data[data[,2]==year&data[,4]==2,5:35],na.rm=T)))
  }
  plot(DataYear[,1],DataYear[,2],
       main=data[1,1],ylab="Days",xlab="year",pch=19,col="darkred")
  points(DataYear[,1],DataYear[,3],pch=19,col="darkblue")
#  lines(c(0,20000),c(25,25),lty=3)
#  lines(c(0,20000),c(30,30),lty=3)
#  lines(c(0,20000),c(20,20),lty=3)
  DataYear
}
