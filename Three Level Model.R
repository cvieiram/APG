# Packages for plotting and having multiple plots in a single screen
#install.packages('ggplot2')
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('Hmisc')
library(Hmisc)
# Package for Inferential Statistics
library(coin)
#install.packages('exactRankTests')
library(exactRankTests)


# Set the working directory
setwd("C:/Temp/Dropbox/Phd/Administrative/Mimi/APG Scripts/")
# Read the data from the csv file
data <- read.table("SurveyData.csv",header=TRUE,sep=",")
summary(data)

plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",1,FALSE)
plotModel(gradData$G1, gradData$G3, "Class Lecture", "Peer Review",1,FALSE)
plotModel(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review",1,FALSE)
plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",1,FALSE)


plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",2,FALSE)
plotModel(gradData$G1, gradData$G3, "Class Lecture", "Peer Review",2,FALSE)
plotModel(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review",2,FALSE)
plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",2,FALSE)

plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",3,FALSE)
plotModel(gradData$G1, gradData$G3, "Class Lecture", "Peer Review",3,FALSE)
plotModel(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review",3,FALSE)
plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",3,FALSE)

a<- plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",1,FALSE)
b<- plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",2,FALSE)
c<- plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",3,FALSE)
png(file="../images/threeLevels.png",width=1200,height=400)
multiplot(a,b,c, cols=3)
dev.off()


a<- plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",1,TRUE)
b<- plotModel(gradData$G1, gradData$G3, "Class Lecture", "Peer Review",1,TRUE)
c<- plotModel(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review",1,TRUE)
d<- plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",1,TRUE)
png(file="../images/lectureGradLevelOne.png",width=1000,height=800)
multiplot(a,b,c,d, cols=2)
dev.off()


a<- plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",2,TRUE)
b<- plotModel(gradData$G1, gradData$G3, "Class Lecture", "Peer Review",2,TRUE)
c<- plotModel(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review",2,TRUE)
d<- plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",2,TRUE)
png(file="../images/lectureGradLevelTwo.png",width=1000,height=800)
multiplot(a,b,c,d, cols=2)
dev.off()



a<- plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",3,TRUE)
b<- plotModel(gradData$G1, gradData$G3, "Class Lecture", "Peer Review",3,TRUE)
c<- plotModel(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review",3,TRUE)
d<- plotModel(gradData$G1, gradData$G5, "Class Lecture", "sLectures",3,TRUE)
png(file="../images/lectureGradLevelThree.png",width=1000,height=800)
multiplot(a,b,c,d, cols=2)
dev.off()


basic<- plotModel(gradData$G1, gradData$G3, "Method One", "Method Two",1,FALSE)
png(file="../images/LevelsOne.png",width=800,height=700)
basic
dev.off()

second<- plotModel(gradData$G1, gradData$G4, "Method One", "Method Two",2,FALSE)
png(file="../images/LevelsTwo.png",width=800,height=700)
second
dev.off()

third <- plotModel(gradData$G1, gradData$G4, "Method One", "Method Two",3,FALSE)
png(file="../images/LevelsThree.png",width=800,height=700)
third
dev.off()

plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",1)
plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",2)
plotModel(gradData$G1, gradData$G2, "Class Lecture", "Homework",3)



plotModel <- function (col1,col2, col1Name,col2Name, level=1, title=FALSE)
{
  if(level==1)
  {
    data2 <- aggregate(col1,by=list(x=col1,y=col2),length)
    names(data2)[3] <- "count"
    first <- (sum(data2[data2$x<data2$y,]$count) + 
                sum(data2[data2$x==data2$y,]$count/2))/sum(data2$count)
    print(first)
    
    second <- (sum(data2[data2$x>data2$y,]$count) + 
                 sum(data2[data2$x==data2$y,]$count/2) )/sum(data2$count)
    print(second)
    
    df<- data.frame(x=c(0,10), y0=c(0,10),y1=c(0,0), y2=c(10,10))
    plot <- ggplot(data = df,aes(x,y0))+
      geom_ribbon(aes(x=x, ymax=y0, ymin=y2), fill="gray39", alpha=(first)) +
      geom_ribbon(aes(x=x, ymax=y1, ymin=y0), fill="gray39", alpha=(second)) +
      annotate("text", x=2.5, y=7.5, label= paste(round(100*first, 2),'%',sep=''), size=8, color="gray0")+
      annotate("text", x=7.5, y=2.5, label= paste(round(100*second,2),'%',sep=''), size=8, color="gray0")+
      labs(x=col1Name,y=col2Name) +
      theme_bw(base_size=20)
    
    
  }else if(level==2)
  {
    data2 <- aggregate(col1,by=list(x=col1,y=col2),length)
    names(data2)[3] <- "count"
    first <- (sum(data2[data2$x<5&data2$y<5,]$count) + 
                sum(data2[data2$x==5&data2$y<5,]$count/2) +
                sum(data2[data2$x<5&data2$y==5,]$count/2) +
                sum(data2[data2$x==5&data2$y==5,]$count/4))/sum(data2$count)
    print(first)
    second <- (sum(data2[data2$x<5&data2$y>5,]$count) + 
                 sum(data2[data2$x==5&data2$y>5,]$count/2) +
                 sum(data2[data2$x<5&data2$y==5,]$count/2) +
                 sum(data2[data2$x==5&data2$y==5,]$count/4))/sum(data2$count)
    print(second)
    third <- (sum(data2[data2$x>5&data2$y<5,]$count) + 
                sum(data2[data2$x==5&data2$y<5,]$count/2) +
                sum(data2[data2$x>5&data2$y==5,]$count/2) +
                sum(data2[data2$x==5&data2$y==5,]$count/4))/sum(data2$count)
    print(third)
    fourth <- (sum(data2[data2$x>5&data2$y>5,]$count) + 
                 sum(data2[data2$x==5&data2$y>5,]$count/2) +
                 sum(data2[data2$x>5&data2$y==5,]$count/2) +
                 sum(data2[data2$x==5&data2$y==5,]$count/4))/sum(data2$count)
    print(fourth)
    df<- data.frame(x=c(2.5, 2.5, 7.5, 7.5), y=c(2.5,7.5,2.5,7.5))
    df$weights<-c(first,second,third,fourth)
    plot <- ggplot(data = df, aes(x=x, y=y, fill=weights)) + 
      geom_tile() + scale_fill_gradient(low = "white",high = "gray39", guide = FALSE)+ #high = "springgreen4")+#
      labs(x=col1Name,y=col2Name) +
      annotate("text", x=df$x, y=df$y, label= paste(round(100*df$weights,2),'%',sep=''), size=8, color="gray0")+
      theme_bw(base_size=20)

  }else
  {
    print(3)
    data2 <- aggregate(col1,by=list(x=col1,y=col2),length)
    names(data2)[3] <- "count"
    first.a <- (sum(data2[(data2$x<5&data2$y<5)&(data2$x<data2$y),]$count) +
                  sum(data2[(data2$x<5&data2$y<5)&(data2$x==data2$y),]$count)/2 + 
                  sum(data2[data2$x<5&data2$y==5,]$count/2) +
                  sum(data2[data2$x==5&data2$y==5,]$count/6))/sum(data2$count)
    first.b <- (sum(data2[(data2$x<5&data2$y<5)&(data2$x>data2$y),]$count) +
                  sum(data2[(data2$x<5&data2$y<5)&(data2$x==data2$y),]$count)/2 + 
                  sum(data2[data2$x==5&data2$y<5,]$count/2) +
                  sum(data2[data2$x==5&data2$y==5,]$count/6))/sum(data2$count)
    print(first.a)
    print(first.b)
    
    second <- (sum(data2[data2$x<5&data2$y>5,]$count) + 
                 sum(data2[data2$x==5&data2$y>5,]$count/2) +
                 sum(data2[data2$x<5&data2$y==5,]$count/2) +
                 sum(data2[data2$x==5&data2$y==5,]$count/6))/sum(data2$count)
    print(second)
    third <- (sum(data2[data2$x>5&data2$y<5,]$count) + 
                sum(data2[data2$x==5&data2$y<5,]$count/2) +
                sum(data2[data2$x>5&data2$y==5,]$count/2) +
                sum(data2[data2$x==5&data2$y==5,]$count/6))/sum(data2$count)
    print(third)
    fourth.a <- (sum(data2[(data2$x>5&data2$y>5)&(data2$x<data2$y),]$count) +
                   sum(data2[(data2$x>5&data2$y>5)&(data2$x==data2$y),]$count/2) + 
                   sum(data2[data2$x==5&data2$y>5,]$count/2) +
                   sum(data2[data2$x==5&data2$y==5,]$count/6))/sum(data2$count)
    
    fourth.b <- (sum(data2[(data2$x>5&data2$y>5)&(data2$x>data2$y),]$count) +
                   sum(data2[(data2$x>5&data2$y>5)&(data2$x==data2$y),]$count/2) + 
                   sum(data2[data2$x>5&data2$y==5,]$count/2) +
                   sum(data2[data2$x==5&data2$y==5,]$count/6))/sum(data2$count)
    print(fourth.a)
    print(fourth.b)
    
    df<- data.frame(x=c(0,10), x0=c(0,5), x1=c(5,10), y=c(0,10),y0=c(0,0),y1=c(0,5), y2=c(5,5), y3=c(5,10), y4=c(10,10))
    
    plot <-  ggplot(data = df,aes(x,y))+
      geom_ribbon(aes(x=x0, ymax=y1, ymin=y0), fill="gray39", alpha=(first.b)) +
      geom_ribbon(aes(x=x0, ymax=y2, ymin=y1), fill="gray39", alpha=(first.a)) +
      geom_ribbon(aes(x=x0, ymax=y4, ymin=y2), fill="gray39", alpha=(second)) +
      geom_ribbon(aes(x=x1, ymax=y2, ymin=y0), fill="gray39", alpha=(third)) +
      geom_ribbon(aes(x=x1, ymax=y2, ymin=y3), fill="gray39", alpha=(fourth.b)) +
      geom_ribbon(aes(x=x1, ymax=y3, ymin=y4), fill="gray39", alpha=(fourth.a)) +
      annotate("text", x=1.25, y=3.75, label= paste(round(100*first.a, 2),'%',sep=''), size=6, color="gray0")+
      annotate("text", x=3.75, y=1.25, label= paste(round(100*first.b,2),'%',sep=''), size=6, color="gray0")+
      annotate("text", x=2.5, y=7.5, label= paste(round(100*second, 2),'%',sep=''), size=6, color="gray0")+
      annotate("text", x=7.5, y=2.5, label= paste(round(100*third,2),'%',sep=''), size=6, color="gray0")+
      annotate("text", x=6.25, y=8.75, label= paste(round(100*fourth.a, 2),'%',sep=''), size=6, color="gray0")+
      annotate("text", x=8.75, y=6.25, label= paste(round(100*fourth.b,2),'%',sep=''), size=6, color="gray0")+
      geom_abline(slope = 1, size = 0.5)+
      labs(x=col1Name,y=col2Name) +
      theme_bw(base_size=20)
      
  }
  if(title){
    plot <- plot +ggtitle(paste(col1Name," vs. ", col2Name))
  }
  plot
}
