#/Users/Gota/Desktop/研究/Test/Data
#2016092502_浦和vs広島_1stHalf.csv
library(sqldf)
#functions
AllData<-function(wd)
{
  print("Let's get started")
  Li<-list.files(wd)
  GameList<-Li[order(Li)]
  n<-length(GameList)
  for(i in 1:n)
  {
    print(GameList[i])
    if(i%%2==1)
    {
      df<-extract(GameList[i],wd,0,NA)
    }
    else
    {
      secData<-df
      sec<-max(secData$Sec)+1
      df<-extract(GameList[i],wd,second_ =  sec,SecondData_ =  secData)
    }
    print("One finished")
  }
}

extract<-function(filename,wd,second_,SecondData_)
{
  #Set Work Directory
  setwd(wd)
  one_if_csv<-grep("\\.csv",filename)
  if(one_if_csv==1)
  {  
    data<-read.csv(filename, header=T,fileEncoding = "cp932") 
    
    dataRows<-nrow(data)
    initFrame<-data[2,2]
    finalFrame<-data[dataRows,2]
    #Initialization
    second<-second_
    oldHomeNumber<-NA
    oldAwayNumber<-NA
    
    secondData<-SecondData_
    series<-seq(from=initFrame,to=finalFrame,by=25)
    for(i in series)
    {
      #フレーム番号iのデータ抽出（i/25秒目）
      currentData<-data[data$フレーム番号==i,]
      #H&Aの選手の数チェック（増えていないか）
      homePlayers<-currentData[currentData$ホームアウェイF==1,]
      homePlayers.ordered<-homePlayers[order(homePlayers$背番号),]
      
      awayPlayers<-currentData[currentData$ホームアウェイF==2,]
      awayPlayers.ordered<-awayPlayers[order(awayPlayers$背番号),]
      Players<-rbind(homePlayers,awayPlayers)
      #後半のデータはx,yを反転させる
      if(!is.na(SecondData_))
      {
        Players$座標X<- -1*Players$座標X
        Players$座標Y<- -1*Players$座標Y
      }
      #Number of players for each team
      newHomeNumber<-nrow(homePlayers)
      newAwayNumber<-nrow(awayPlayers)
      PlayersNum<-newHomeNumber+newAwayNumber
      
      Sec<-rep(second,PlayersNum)
      Players<-cbind(Players,Sec)
      #交代時に選手増える
      if(!is.na(oldAwayNumber)&&!is.na(oldHomeNumber))
      {
        if(oldAwayNumber<newAwayNumber)
        {
          
        }
        if(oldHomeNumber<newAwayNumber)
        {
          
        }
      }
      
      #データを合わせる
      secondData<-na.omit(rbind(secondData,Players))
      second <-second+1
      
      oldHomeNumber<-newHomeNumber
      oldAwayNumber<-newAwayNumber
    }

    MatchOrNot_2nd<-grep("2nd",filename)
    if(length(MatchOrNot_2nd)!=0)
    {
      MatchName<-gsub("_2ndHalf", "",filename)
      outputName<-paste(gsub("\\..+$", "",MatchName),"BySecond",sep = "_")
      outputName.ext<-paste(outputName,"csv",sep = ".")
      SwapPath("Origin","BySecond")
      write.csv(secondData,outputName.ext,row.names = FALSE)
    }
    else
    {
      print(second)
    }
    dataF<-secondData
    return(dataF)
  }
  else
  {
    return(NULL)
  }
}


AddPath<-function(path_)
{
  path<-paste(getwd(),path_,sep = "/")
  setwd(path)
}

SwapPath<-function(origin,swap)
{
  path<-getwd()
  setwd(sub(origin,swap,path))
}
#file name
wd <-"/Users/Gota/Desktop/研究/Test/Data/Origin"

#main
AllData(wd)

#test


