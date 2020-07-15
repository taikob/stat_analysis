dataname='file.csv'#file path
data=read.table(dataname, sep=',')

startv <- c(1, 4,0.1,0)
endv   <- c(1,15,0.9,1)
difv   <- c(1, 1,0.1,0.1)

numv <- c()
for (i in c(1:length(startv))){
  numv <- c(numv,((endv[i]-startv[i])/difv[i]+1))
}
nump <- length(data)-length(numv)

xl <- c(startv[4],endv[4])
x <- seq(startv[4], endv[4], by = difv[4])
plotdata <- array(dim = c(3,numv[3],numv[4]))
vi<-1
for (v in seq(startv[3], endv[3], by = difv[3])){
  wi<-1
  for (w in seq(startv[4], endv[4], by = difv[4])){
    for (i in c(1:(length(data$V1)))){
      if(data[i,1]==1 && data[i,2]==4 && as.integer(10*data[i,3])==as.integer(10*v) && as.integer(10*data[i,4])==as.integer(10*w)){
        plotdata[1,vi,wi]<-data[i,5]
        plotdata[2,vi,wi]<-data[i,6]
        plotdata[3,vi,wi]<-data[i,7]
        break
      }
    }
    wi=wi+1
  }
  vi=vi+1
}


#mx=max(plotdata[1,,])
#mn=min(plotdata[2,,])

if (abs(mx) > abs(mn)){
  yl<-c(-mx,mx)
} else {
  yl<-c(mn,-mn)
}

plot(x,plotdata[3,1,],xlim=xl,ylim=yl,type="l", ann=F, col = 1)
for(i in c(2:length(plotdata[1,,1]))){
  par(new=T)
  plot(x,plotdata[3,i,],xlim=xl,ylim=yl,type="l", ann=F, col = i)
}