# Libraries to organize the data so that we can plot the means of the gouprs
# Functions melt and ddply
install.packages('reshape')
library(reshape)
install.packages('plyr')
library(plyr)
library(Rmisc)


# Graduate Plots
plotPreferences <-  function(lessData, greatedData, act)
{
  result<- rbind(lessData,greatedData)
  
  colnames(result) <- c('Student.ID', act,'group')
  meltedResult <- melt(result, id.vars=c("Student.ID", "group"))
  
  # Compute means and standard error (se)
  means <- ddply(meltedResult, c("group", "variable"), summarise, 
                 mean=mean(value, na.rm = TRUE), se=sd(value, na.rm = TRUE)/sqrt(length(value)))
  
  # Create the plot with the error bars
  ggplot(means, aes(x=variable, y=mean, fill= group)) + 
    geom_bar(position=position_dodge(), stat="identity")+
    scale_colour_grey() +
    labs(x= 'Learning Activity', y='Average Preference per Group')+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
    scale_fill_grey(guide = guide_legend(title = "Group"), start = 0.2, end = .5)+
    scale_y_continuous(breaks=c(-3:10))
}

# Scatter plots among questions
data2 <- aggregate(gradData$G1,by=list(x=gradData$G1,y=gradData$G2),length)
names(data2)[3] <- "count"
a<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q2") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G1,by=list(x=gradData$G1,y=gradData$G3),length)
names(data2)[3] <- "count"
b<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q3") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G1,by=list(x=gradData$G1,y=gradData$G4),length)
names(data2)[3] <- "count"
c<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q4") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G1,by=list(x=gradData$G1,y=gradData$G5),length)
names(data2)[3] <- "count"
d<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G2,by=list(x=gradData$G2,y=gradData$G3),length)
names(data2)[3] <- "count"
e<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q3") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G2,by=list(x=gradData$G2,y=gradData$G4),length)
names(data2)[3] <- "count"
f<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q4") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G2,by=list(x=gradData$G2,y=gradData$G5),length)
names(data2)[3] <- "count"
g<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G3,by=list(x=gradData$G3,y=gradData$G4),length)
names(data2)[3] <- "count"
h<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q3 VS Q4") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G3,by=list(x=gradData$G3,y=gradData$G5),length)
names(data2)[3] <- "count"
i<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q3 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(gradData$G4,by=list(x=gradData$G4,y=gradData$G5),length)
names(data2)[3] <- "count"
j<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q4 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

install.packages('Rmisc')
library(Rmisc)
multiplot(a,b,c,d,e,f,g,h,i,j, cols=3)

multiplot(a,b,c, cols=3)


rcorr(as.matrix(underData[,7:13]))
data2 <- aggregate(underData$X1A,by=list(x=underData$X1A,y=underData$X1B),length)
names(data2)[3] <- "count"
a<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q2") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1A,by=list(x=underData$X1A,y=underData$X1C),length)
names(data2)[3] <- "count"
b<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q3") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1A,by=list(x=underData$X1A,y=underData$X1D),length)
names(data2)[3] <- "count"
c<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q4") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1A,by=list(x=underData$X1A,y=underData$X1E),length)
names(data2)[3] <- "count"
d<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1A,by=list(x=underData$X1A,y=underData$X1F),length)
names(data2)[3] <- "count"
e<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q6") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

multiplot(a,b,c,d,e, cols=3)


data2 <- aggregate(underData$X1A,by=list(x=underData$X1A,y=underData$X1G),length)
names(data2)[3] <- "count"
a<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q1 VS Q7") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1B,by=list(x=underData$X1B,y=underData$X1C),length)
names(data2)[3] <- "count"
b<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q3") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1B,by=list(x=underData$X1B,y=underData$X1D),length)
names(data2)[3] <- "count"
c<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q4") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1B,by=list(x=underData$X1B,y=underData$X1E),length)
names(data2)[3] <- "count"
d<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

multiplot(a, b, c, d, cols=2)

data2 <- aggregate(underData$X1B,by=list(x=underData$X1B,y=underData$X1F),length)
names(data2)[3] <- "count"
a<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q6") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1B,by=list(x=underData$X1B,y=underData$X1G),length)
names(data2)[3] <- "count"
b<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q2 VS Q7") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1C,by=list(x=underData$X1C,y=underData$X1D),length)
names(data2)[3] <- "count"
c<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q3 VS Q4") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1C,by=list(x=underData$X1C,y=underData$X1E),length)
names(data2)[3] <- "count"
d<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q3 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))


multiplot(a, b, c, d, cols=2)


data2 <- aggregate(underData$X1C,by=list(x=underData$X1C,y=underData$X1F),length)
names(data2)[3] <- "count"
a<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q3 VS Q6") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1C,by=list(x=underData$X1C,y=underData$X1G),length)
names(data2)[3] <- "count"
b<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q3 VS Q7") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1D,by=list(x=underData$X1D,y=underData$X1E),length)
names(data2)[3] <- "count"
c<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q4 VS Q5") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1D,by=list(x=underData$X1D,y=underData$X1F),length)
names(data2)[3] <- "count"
d<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q4 VS Q6") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))


multiplot(a, b, c, d, cols=2)

data2 <- aggregate(underData$X1D,by=list(x=underData$X1D,y=underData$X1G),length)
names(data2)[3] <- "count"
a<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q4 VS Q7") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1E,by=list(x=underData$X1E,y=underData$X1F),length)
names(data2)[3] <- "count"
b<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q5 VS Q6") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1E,by=list(x=underData$X1E,y=underData$X1G),length)
names(data2)[3] <- "count"
c<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q5 VS Q7") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

data2 <- aggregate(underData$X1F,by=list(x=underData$X1F,y=underData$X1G),length)
names(data2)[3] <- "count"
d<- ggplot(data2, aes(x = x, y = y)) +  ggtitle("Q6 VS Q7") + geom_abline(slope = 1) + geom_point(aes(size = count)) + coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))

multiplot(a, b, c, d, cols=2)