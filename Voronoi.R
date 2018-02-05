library(deldir)

drawVoronoi<-function(x,y)
{
  res<-deldir(x,y)
  tile<-tile.list(res)
  
  plot(c(min(x),max(x)),c(min(y),max(y)),type="n",xlab="x",ylab="y")

}

