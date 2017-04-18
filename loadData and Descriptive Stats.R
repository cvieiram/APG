# Package to generate latex code
#install.packages('xtable')
library(xtable)

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

# Subset the data between graduate and undergrads
gradData <- data[data$Level=='G',1:6]
underData <- data[data$Level=='U',c(1,7:13)]

# Descriptive Statistics: Mean, Standard Deviatiom and Count
# This function allows to compute the descriptive statistics from one column that we pass
calculateDescriptive <- function(col)
{
  c(round(mean(col, na.rm = TRUE), digits=2), 
    round(sd(col, na.rm = TRUE), digits=2), 
    sum(!is.na(col)))
}

# I created a data frame for each of the groups to later generate the latex table

# Graduate students
# Name of the activities
activities <- c("Classroom Lecture", "Homework Assignments","Peer Review","Reading Peer Review", "sLectures")
# Compute the descriptive stats for all the questions and take them into a data frame
gradDescriptive <- as.data.frame(t(sapply(gradData[,2:6], calculateDescriptive)))
gradDescriptive <- cbind(activities, gradDescriptive)
# Set the column names
colnames(gradDescriptive) <- c('Activity','Mean','SD', 'Count')


# Undegrads
activitiesU <- c("Classroom Lecture","Hand outs" ,"Homework Assignments","Blogs","sLectures", "Review sLectures", "Reading Reviews")
undergradDescriptive<- as.data.frame(t(sapply(underData[,2:8], calculateDescriptive)))
undergradDescriptive <- cbind(activitiesU, undergradDescriptive)
colnames(undergradDescriptive) <- c('Activity','Mean','SD', 'Count')



# Create the latex code
print(xtable(gradDescriptive, caption = "\\textbf{Gradruate Student-Perceived Learning Usefulness for Different Classroom Activities}"),
      caption.placement = "top",  include.rownames=FALSE, floating = TRUE, latex.environments = "center")

print(xtable(undergradDescriptive, caption = "\\textbf{Undergraduate Student-Perceived Learning Usefulness for Different Classroom Activities}"),
      caption.placement = "top",  include.rownames=FALSE, floating = TRUE, latex.environments = "center")


# Compare students' preferences
wilcox.exact(gradData$G1, gradData$G2, paired = TRUE, alternative = "two.sided", conf.int = TRUE)
wilcox.exact(gradData$G1, gradData$G3, paired = TRUE, alternative = "two.sided", conf.int = TRUE)
wilcox.exact(gradData$G1, gradData$G4, paired = TRUE, alternative = "two.sided", conf.int = TRUE)
wilcox.exact(gradData$G1, gradData$G5, paired = TRUE, alternative = "two.sided", conf.int = TRUE)

wilcoxsign_test(G1~G2, data = gradData, paired=TRUE, distribution="exact")
wilcoxsign_test(G1~G3, data = gradData, paired=TRUE, distribution="exact")
wilcoxsign_test(G1~G4, data = gradData, paired=TRUE, distribution="exact")
wilcoxsign_test(G1~G5, data = gradData, paired=TRUE, distribution="exact")

gradData$G1G3<- gradData$G1- gradData$G3
gradData$num<-seq(1:nrow(gradData))
gradData$positive<- ifelse(gradData$G1G3>0, gradData$num, 0)
gradData$negative<- ifelse(gradData$G1G3<0, gradData$num, 0)
sum(gradData$positive)/(sum(gradData$positive)+sum(gradData$negative)) - sum(gradData$negative)/(sum(gradData$positive)+sum(gradData$negative))

gradData$G1G4<- gradData$G1- gradData$G4
gradData$num<-seq(1:nrow(gradData))
gradData$positive<- ifelse(gradData$G1G4>0, gradData$num, 0)
gradData$negative<- ifelse(gradData$G1G4<0, gradData$num, 0)
sum(gradData$positive)/(sum(gradData$positive)+sum(gradData$negative)) - sum(gradData$negative)/(sum(gradData$positive)+sum(gradData$negative))


wilcox.exact(underData$X1A, underData$X1B, paired = TRUE, alternative = "two.sided")
wilcox.exact(underData$X1A, underData$X1C, paired = TRUE, alternative = "two.sided")
wilcox.exact(underData$X1A, underData$X1D, paired = TRUE, alternative = "two.sided")
wilcox.exact(underData$X1A, underData$X1E, paired = TRUE, alternative = "two.sided")
wilcox.exact(underData$X1A, underData$X1F, paired = TRUE, alternative = "two.sided")
wilcox.exact(underData$X1A, underData$X1G, paired = TRUE, alternative = "two.sided")

wilcoxsign_test(X1A~X1B, data = underData, paired=TRUE, distribution="exact")
wilcoxsign_test(X1A~X1C, data = underData, paired=TRUE, distribution="exact")
wilcoxsign_test(X1A~X1D, data = underData, paired=TRUE, distribution="exact")
wilcoxsign_test(X1A~X1E, data = underData, paired=TRUE, distribution="exact")
wilcoxsign_test(X1A~X1F, data = underData, paired=TRUE, distribution="exact")
wilcoxsign_test(X1A~X1G, data = underData, paired=TRUE, distribution="exact")

underData<-underData[complete.cases(underData),]
underData$X1AX1D<- underData$X1A- underData$X1D
underData$num<-seq(1:nrow(underData))
underData$positive<- ifelse(underData$X1AX1D>0, underData$num, 0)
underData$negative<- ifelse(underData$X1AX1D<0, underData$num, 0)
sum(underData$positive)/(sum(underData$positive)+sum(underData$negative)) - sum(underData$negative)/(sum(underData$positive)+sum(underData$negative))

underData$X1AX1D<- underData$X1A- underData$X1D
underData$num<-seq(1:nrow(underData))
underData$positive<- ifelse(underData$X1AX1D>0, underData$num, 0)
underData$negative<- ifelse(underData$X1AX1D<0, underData$num, 0)
sum(underData$positive)/(sum(underData$positive)+sum(underData$negative)) - sum(underData$negative)/(sum(underData$positive)+sum(underData$negative))

underData$X1AX1E<- underData$X1A- underData$X1E
underData$num<-seq(1:nrow(underData))
underData$positive<- ifelse(underData$X1AX1E>0, underData$num, 0)
underData$negative<- ifelse(underData$X1AX1E<0, underData$num, 0)
sum(underData$positive)/(sum(underData$positive)+sum(underData$negative)) - sum(underData$negative)/(sum(underData$positive)+sum(underData$negative))

underData$X1AX1F<- underData$X1A- underData$X1F
underData$num<-seq(1:nrow(underData))
underData$positive<- ifelse(underData$X1AX1F>0, underData$num, 0)
underData$negative<- ifelse(underData$X1AX1F<0, underData$num, 0)
sum(underData$positive)/(sum(underData$positive)+sum(underData$negative)) - sum(underData$negative)/(sum(underData$positive)+sum(underData$negative))

underData$X1AX1G<- underData$X1A- underData$X1G
underData$num<-seq(1:nrow(underData))
underData$positive<- ifelse(underData$X1AX1G>0, underData$num, 0)
underData$negative<- ifelse(underData$X1AX1G<0, underData$num, 0)
sum(underData$positive)/(sum(underData$positive)+sum(underData$negative)) - sum(underData$negative)/(sum(underData$positive)+sum(underData$negative))


ggplot(na.omit(gradData), aes(G1)) + geom_bar()+
  geom_text(stat='count',aes(label=paste(round(prop.table(..count..)*100, digits = 2),'%'),vjust=-1))+
  labs(x= 'Perceived Usefulness', y='Number of Students')+
  coord_cartesian(ylim = c(0, 15), xlim = c(1:10))+
  scale_x_continuous(breaks=c(1:10))
  theme_set(theme_gray(base_size = 14))


ggplot(na.omit(underData), aes(X1A)) + geom_bar()+
  geom_text(stat='count',aes(label=paste(round(prop.table(..count..)*100, digits = 2),'%'),vjust=-1))+
  labs(x= 'Perceived Usefulness', y='Number of Students')+
  coord_cartesian(ylim = c(0, 15), xlim = c(1,10))+
  scale_x_continuous(breaks=c(1:10))



# Correlations among the graduate students questions
gradCorrelations <- rcorr(as.matrix(gradData[,2:6]))

