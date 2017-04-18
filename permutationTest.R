library(perm)

testGroups(gradData$G1, gradData$G2)
testGroups(gradData$G1, gradData$G3)
testGroups(gradData$G1, gradData$G4)
testGroups(gradData$G1, gradData$G5)

testGroups(underData$X1A,underData$X1B)
testGroups(underData$X1A,underData$X1C)
testGroups(underData$X1A,underData$X1D)
testGroups(underData$X1A,underData$X1E)
testGroups(underData$X1A,underData$X1F)
testGroups(underData$X1A,underData$X1G)
underData[complete.cases(underData), ]
testGroups <- function (col1,col2)
{
  data2<-data.frame(cbind(col1,col2))
  colnames(data2) <- c('x','y')
  data2<- data2[complete.cases(data2), ]
  data2$group<-NA
  
  if(length(data2[data2$x<=5&data2$y>=5,]$group)) data2[data2$x<=5&data2$y>=5,]$group <- 'A'
  if(length(data2[data2$x<=5&data2$y<=5,]$group)) data2[data2$x<=5&data2$y<=5,]$group <- 'B'
  if(length(data2[data2$x>=5&data2$y>=5,]$group)) data2[data2$x>=5&data2$y>=5,]$group <- 'C'
  if(length(data2[data2$x>=5&data2$y<=5,]$group)) data2[data2$x>=5&data2$y<=5,]$group <- 'D'

  
  permKS(data2$y,data2$group,exact = NULL, method = 'exact.mc')
}