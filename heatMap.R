library(Rmisc)
library(ggplot2)

a<- addWeights(gradData$G1, gradData$G2, "Class Lecture", "Homework")
b<- addWeights(gradData$G1, gradData$G3, "Class Lecture", "Peer Review")
c<- addWeights(gradData$G1, gradData$G4, "Class Lecture", "Reading Peer Review")
d<- addWeights(gradData$G1, gradData$G5, "Class Lecture", "sLectures")
png(file="../images/lectureGrad.png",width=1000,height=800)
multiplot(a,b,c,d, cols=2)
dev.off()

gradData[gradData$G1>5,]


a<- addWeights(gradData$G2, gradData$G1, "Homework", "Class Lecture")
b<- addWeights(gradData$G2, gradData$G3, "Homework", "Peer Review")
c<- addWeights(gradData$G2, gradData$G4, "Homework", "Reading Peer Review")
d<- addWeights(gradData$G2, gradData$G5, "Homework", "sLectures")
multiplot(a,b,c,d, cols=2)

a<- addWeights(gradData$G3, gradData$G1, "Peer Review", "Class Lecture")
b<- addWeights(gradData$G3, gradData$G2, "Peer Review", "Homework")
c<- addWeights(gradData$G3, gradData$G4, "Peer Review", "Reading Peer Review")
d<- addWeights(gradData$G3, gradData$G5, "Peer Review", "sLectures")
multiplot(a,b,c,d, cols=2)

a<- addWeights(gradData$G4, gradData$G1, "Reading Peer Review", "Class Lecture")
b<- addWeights(gradData$G4, gradData$G2, "Reading Peer Review", "Homework")
c<- addWeights(gradData$G4, gradData$G3, "Reading Peer Review", "Peer Review")
d<- addWeights(gradData$G4, gradData$G5, "Reading Peer Review", "sLectures")
multiplot(a,b,c,d, cols=2)

a<- addWeights(gradData$G5, gradData$G1, "sLectures", "Class Lecture")
b<- addWeights(gradData$G5, gradData$G2, "sLectures", "Homework")
c<- addWeights(gradData$G5, gradData$G3, "sLectures", "Peer Review")
d<- addWeights(gradData$G5, gradData$G4, "sLectures", "Reading Peer Review")
multiplot(a,b,c,d, cols=2)

a<- addWeights(underData$X1A, underData$X1B, "Lecture", "Handout")
b<- addWeights(underData$X1A, underData$X1C, "Lecture", "Homework")
c<- addWeights(underData$X1A, underData$X1D, "Lecture", "Blogs")
d<- addWeights(underData$X1A, underData$X1E, "Lecture", "sLecture")
e<- addWeights(underData$X1A, underData$X1F, "Lecture", "Review sLecture")
f<- addWeights(underData$X1A, underData$X1G, "Lecture", "Reading Review")
png(file="../images/lectureUnderGrad.png",width=1000,height=1200)
multiplot(a,b,c,d,e,f, cols=2)
dev.off()




a<- addWeights(underData$X1B, underData$X1A, "Handout", "Lecture")
b<- addWeights(underData$X1B, underData$X1C, "Handout", "Homework")
c<- addWeights(underData$X1B, underData$X1D, "Handout", "Blogs")
d<- addWeights(underData$X1B, underData$X1E, "Handout", "sLecture")
e<- addWeights(underData$X1B, underData$X1F, "Handout", "Review sLecture")
f<- addWeights(underData$X1B, underData$X1G, "Handout", "Reading Review")
multiplot(a,b,c,d,e,f, cols=3)

a<- addWeights(underData$X1C, underData$X1A, "Homework", "Lecture")
b<- addWeights(underData$X1C, underData$X1B, "Homework", "Handout")
c<- addWeights(underData$X1C, underData$X1D, "Homework", "Blogs")
d<- addWeights(underData$X1C, underData$X1E, "Homework", "sLecture")
e<- addWeights(underData$X1C, underData$X1F, "Homework", "Review sLecture")
f<- addWeights(underData$X1C, underData$X1G, "Homework", "Reading Review")
multiplot(a,b,c,d,e,f, cols=3)

underData$X1C,y=underData$X1F

col1<- underData$X1A
col2<- underData$X1E
cbind(col1, col2)
addWeights <- function (col1,col2, col1Name,col2Name)
{
    data2 <- aggregate(col1,by=list(x=col1,y=col2),length)
    names(data2)[3] <- "count"
    first <- sum(data2[data2$x<5&data2$y<5,]$count) + 
      sum(data2[data2$x==5&data2$y<5,]$count/2) +
      sum(data2[data2$x<5&data2$y==5,]$count/2) +
      sum(data2[data2$x==5&data2$y==5,]$count/4)
    print(first)
    second <- sum(data2[data2$x<5&data2$y>5,]$count) + 
      sum(data2[data2$x==5&data2$y>5,]$count/2) +
      sum(data2[data2$x<5&data2$y==5,]$count/2) +
      sum(data2[data2$x==5&data2$y==5,]$count/4)
    print(second)
    third <- sum(data2[data2$x>5&data2$y<5,]$count) + 
      sum(data2[data2$x==5&data2$y<5,]$count/2) +
      sum(data2[data2$x>5&data2$y==5,]$count/2) +
      sum(data2[data2$x==5&data2$y==5,]$count/4)
    print(third)
    fourth <- sum(data2[data2$x>5&data2$y>5,]$count) + 
      sum(data2[data2$x==5&data2$y>5,]$count/2) +
      sum(data2[data2$x>5&data2$y==5,]$count/2) +
      sum(data2[data2$x==5&data2$y==5,]$count/4)
    print(fourth)
    df<- data.frame(x=c(2.5, 2.5, 7.5, 7.5), y=c(2.5,7.5,2.5,7.5))
    df$weights<-c(first,second,third,fourth)
    plot <- ggplot(data = df, aes(x=x, y=y, fill=weights)) + 
      geom_tile() + scale_fill_gradient(low = "white",high = "steelblue")+ #high = "springgreen4")+#
      ggtitle(paste(col1Name," vs. ", col2Name))+
      labs(x=col1Name,y=col2Name) +geom_abline(slope = 1, size = 1)+
      annotate("text", x=df$x, y=df$y, label= df$weights, size=10, color="gray28")
    plot
}

