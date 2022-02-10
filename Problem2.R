#Problem 2
library(devtools)
library(regtools)
library(rectools)
library(qeML)
#part1
getML100K <- function(needDownload=FALSE){
  if (needDownload) {
    # 5 Mb
    download.file(
      'http://files.grouplens.org/datasets/movielens/ml-100k.zip',
      'ml-100k.zip')
    unzip('ml-100k.zip')
  }
  currdir <- getwd()  # leave a trail of bread crumbs
  datadir <- 'ml-100k'  # easier to hard code
  setwd(datadir)
  on.exit(setwd(currdir))
  
  # make matrices ud, uu and ui, for the main ratings data, user info
  # an item info
  
  ud <- read.table('u.data',header=F,sep='\t')
  colnames(ud) <- c('user','item','rating','timestamp')
  ud <- as.data.frame(ud)
  
  uu <- read.table('u.user',header=F,sep='|',stringsAsFactors=TRUE)
  ur <- split(ud[,3],ud[,1])  # ratings by user
  uu <- cbind(uu,sapply(ur,mean))
  uu <- cbind(uu,sapply(ur,length))
  colnames(uu) <- c('user','age','gender','occ','zip','userMean','Nuser')
  
  # reading u.item is tricky, with some problematic records etc.;
  # fortunately, we only need the last 19 fields
  z <- readLines('u.item')
  zs <- strsplit(z,'|',fixed=TRUE)  # splits to single characters
  zgl <- lapply(zs,function(charvec) charvec[6:24])  # get the genre dummies
  zgls <- t(sapply(zgl,as.integer))  # create matrix of genres
  ui <- cbind(1:nrow(zgls),zgls)
  ir <- split(ud[,3],ud[,2])  # ratings by item
  ui <- cbind(ui,sapply(ir,mean))
  ui <- cbind(ui,sapply(ir,length))
  colnames(ui) <- c('item',paste('G',1:19,sep=''),'itemMean','Nitem')
  
  setwd(currdir) # follow the trail back 
  uduu <- merge(ud,uu)
  uduuui <- merge(uduu,ui)
  # this ends up in (item,user) order, whereas we need the opposite
  outdf <- uduuui[,c(2,1,3:ncol(uduuui))]
  attr(outdf,'useritemCovs') <- c(4,4)
  attr(outdf,'userCovs') <- c(5,10)
  attr(outdf,'itemCovs') <- c(11,31)
  outdf
}
ml100k <-getML100K()

#guess: unknown movie(item mean)
#user,item,rating,_,age,gender,occ,_,usermean,_,11:29,itemMean
rawData <- ml100k[,c(1,2,3,5,6,7,9,11:30)]
data <-rawData[rawData$user %in% names(which(table(rawData$user)>=20)), ]
data <-data[data$item %in% names(which(table(data$item)>=20)), ]
qeout<-qeLin(data,"rating")
for (i in 1:20){
  answer<-predict(qeout,data.frame(data[i,c(1,2,4:ncol(data))]))
  print(answer)
}
qeLin(data,"rating")$testAcc
qeLin(data,"rating")$baseAcc


#part2
data2<-getInstEval()

data2 <-data2[data2$s %in% names(which(table(data2$s)>=50)), ]
data2 <-data2[data2$d %in% names(which(table(data2$d)>=50)), ]
qeout2<-qeLin(data2,"y")
data2$s<-as.factor(data2$s)
for (i in 1:20){
  answer2<-predict(qeout2,data.frame(data2[i,c(1,2,4:ncol(data2))]))
  print(answer2)
}
qeLin(data2,"y")$testAcc
qeLin(data2,"y")$baseAcc
#part3
#user ID, item ID, rating, side information format
d <- read.table('house-votes-84.data',sep=",")

dataFrame<-data.frame(userId = 1,itemId = 1, rating = 1,sideInfo = 1,test = 0)
#names(dataFrame)<-c("userID","itemID","rating","sideInfo","testcase")
for (i in 1:nrow(d)){
  for (j in 2:17){
    #republican=1,democrat=2
    party <-0
    if (d[i,1] == "republican"){
      party <- 1
    }else if (d[i,1] == "democrat"){
      party <- 2
    }
    #testcase
    test <- 0
    if (d[i,j] =="?"){
      d[i,j] <- sample(c("true","false"),1)
      test <- 1
    }else if (d[i,j] =="y"){
      d[i,j] <- "true"
    }else if (d[i,j] =="n"){
      d[i,j] <- "false"
    }
    v <- c(i,j,as.logical(d[i,j]),party,test)
    
    dataFrame<-rbind(dataFrame,v)
  }
}
dataFrame<-dataFrame[-1,]
dataFrame$userId <- as.factor(dataFrame$userId)
dataFrame$itemId <- as.factor(dataFrame$itemId)
dataFrame$rating<- as.factor(dataFrame$rating)
qeout <- qeLogit(dataFrame[,1:4],"rating")
for (i in 1:nrow(dataFrame)){
  if (dataFrame[i,]$test == 1){
    answer <- predict(qeout,data.frame(dataFrame[i,c(1,2,4)]))$predClasses
    dataFrame[i,3]<- answer
  }
}
answer23<-dataFrame[dataFrame$test == 1,1:4]
qeLogit(dataFrame[,1:4],"rating")$testAcc
qeLogit(dataFrame[,1:4],"rating")$baseAcc



