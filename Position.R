#Distinct
library(dplyr)
#Voronoi
library(deldir)
#Set Working Directory
wd <-"/Users/Gota/Desktop/研究/Test/Data/BySecond"
setwd(wd)

#file name
homePlayers<-NA
awayPlayers<-NA

AllData(wd)

#Procedure
AllData<-function(wd)
{
  print("Let's get started")
  Li<-list.files(wd)
  GameList<-Li[order(Li)]
  n<-length(GameList)
  for(i in 1:n)
  {
    print(GameList[i])
    DataFromNum(GameList[i])
  }
}

DataFromNum<-function(filename)
{
  data<-read.csv(filename,header=T)
  HPlayers<-data[data$ホームアウェイF==1,]
  APlayers<-data[data$ホームアウェイF==2,]
  homePlayersNum<-unique(HPlayers$背番号)
  awayPlayersNum<-unique(APlayers$背番号)
  homeNumbers<-NA;awayNumbers<-NA
  
  #TeamNum<-BothTeamNumbers(homePlayersNum,awayPlayersNum)
  homeSummary<-NA
  awaySummary<-NA
  #FOR HOME TEAM
  for(i in homePlayersNum)
  {
    p<-getPlayerWithNum(HPlayers,num=i)
    #Data Frame
    names<-c("Num","mean_X","mean_Y","sd_X","sd_Y")
    df<-data.frame(p$背番号[1],mean(p$座標X),mean(p$座標Y),sd(p$座標X),sd(p$座標Y))
    colnames(df)<-names
    homeSummary<-na.omit(rbind(homeSummary,df))
  }
  #FOR AWAY TEAM
  for(i in awayPlayersNum)
  {
    p<-getPlayerWithNum(APlayers,num=i)
    #Data Frame
    names<-c("Num","mean_X","mean_Y","sd_X","sd_Y")
    df<-data.frame(p$背番号[1],mean(p$座標X),mean(p$座標Y),sd(p$座標X),sd(p$座標Y))
    colnames(df)<-names
    awaySummary<-na.omit(rbind(awaySummary,df))
  }
  
    names<-c("Num","mean_X","mean_Y","sd_X","sd_Y")
    df<-data.frame(numPlayer$背番号[1],mean(numPlayer$座標X),mean(numPlayer$座標Y),sd(numPlayer$座標X),sd(numPlayer$座標Y))
    colnames(df)<-names
    #Plot
    drawVoronoi(homeSummary$mean_X,homeSummary$mean_Y,"blue")
    par(new=T)
    drawVoronoi(-1*awaySummary$mean_X,-1*awaySummary$mean_Y,"red")
    par(new=F)
    #Home VS Away
    drawVoronoi(homeSummary$mean_X,homeSummary$mean_Y,"blue")
    par(new=T)
    drawVoronoi(awaySummary$mean_X,awaySummary$mean_Y,"red")
    par(new=F)
}
#Voronoi
#SampleData
x<-awaySummary$mean_X
y<-awaySummary$mean_Y
#With color
drawVoronoi<-function(x,y,col_="black")
{
  #Limits
  x_max=5250
  x_min=-1*x_max
  y_max=3400
  y_min=-1*y_max
  #Plot
  res<-deldir(x[1:11],y[1:11],plot=TRUE,rw=c(x_min,x_max,y_min,y_max),main="Voronoi and Delaunay",sub="team:",asp=1,col=col_)
  tiles<-tile.list(res)
  #plot(c(min(x),max(x)),c(min(y),max(y)),type="n",xlab="x",ylab="y",xlim=c(x_min,x_max),ylim=c(y_min,y_max))
  #for(i in 1:res$n.data){ polygon(tiles[[i]]) }
  #points(x[1:11],y[1:11])
}
#Mini Functions

BothTeamNumbers<-function(home,away)
{
  idx<-1
  for(i in home)
  {
    homeNumbers[idx]<-i
    idx<-idx+1
  }
  idx<-1
  for(i in away)
  {
    awayNumbers[idx]<-i
    idx<-idx+1
  }  
  return(c(homeNumbers,awayNumbers))
}
getPlayerWithNum<-function(data,num,team)
{
  return(data[((data$背番号==num)&(data$ホームアウェイF==team)),])
}
getPlayerWithNum<-function(data,num)
{
  return(data[data$背番号==num,])
}
maxSecond<-function(data)
{
  return(data[which.max(data$秒),]$秒)
}
