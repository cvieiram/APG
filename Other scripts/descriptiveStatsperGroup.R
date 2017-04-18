# Function to compute the descriptive and inferential statistics for two groups
# Returns a data frame that can be used to create a latex table
computeGroups<- function(less, greater, activities, gradData)
{
  # Compute descriptive stats for all the colums in the lower group
  lessDescriptive <- as.data.frame(t(sapply(less, calculateDescriptive)))
  lessDescriptive <- cbind(activities, lessDescriptive)
  lessDescriptive$V3<-as.integer(lessDescriptive$V3)
  
  # Compute descriptive stats for all the colums in the higher group
  greaterDescriptive <- as.data.frame(t(sapply(greater, calculateDescriptive)))
  greaterDescriptive <- cbind(activities, greaterDescriptive)
  greaterDescriptive$V3<-as.integer(greaterDescriptive$V3)
  
  # Compare groups (Graduate or Undegraduate students?)
  if(gradData)
  {
    a<- wilcox.exact(less$G1, greater$G1, alternative = "two.sided")
    b<- wilcox.exact(less$G2, greater$G2, alternative = "two.sided")
    c<- wilcox.exact(less$G3, greater$G3, alternative = "two.sided")
    d<- wilcox.exact(less$G4, greater$G4, alternative = "two.sided")
    e<- wilcox.exact(less$G5, greater$G5, alternative = "two.sided")
    wValues<-c(a$statistic,b$statistic,c$statistic,d$statistic,e$statistic)
    pvalues<-c(round(a$p.value, digits=3),
               round(b$p.value, digits=3),
               round(c$p.value, digits=3),
               round(d$p.value, digits=3),
               round(e$p.value, digits=3))
    
  }
  else
  {
    a<- wilcox.exact(less$X1A, greater$X1A, alternative = "two.sided")
    b<- wilcox.exact(less$X1B, greater$X1B, alternative = "two.sided")
    c<- wilcox.exact(less$X1C, greater$X1C, alternative = "two.sided")
    d<- wilcox.exact(less$X1D, greater$X1D, alternative = "two.sided")
    e<- wilcox.exact(less$X1E, greater$X1E, alternative = "two.sided")
    f<- wilcox.exact(less$X1F, greater$X1F, alternative = "two.sided")
    g<- wilcox.exact(less$X1G, greater$X1G, alternative = "two.sided")
    wValues<-c(a$statistic,b$statistic,c$statistic,d$statistic,e$statistic,f$statistic,g$statistic)
    pvalues<-c(round(a$p.value, digits=3),
               round(b$p.value, digits=3),
               round(c$p.value, digits=3),
               round(d$p.value, digits=3),
               round(e$p.value, digits=3),
               round(f$p.value, digits=3),
               round(g$p.value, digits=3))
  }
  # Put together all the results in a single data frame
  summaryGroups <- cbind(lessDescriptive[,1:4], greaterDescriptive[,2:4], wValues, pvalues)
  colnames(summaryGroups) <- c('Activity','Mean','SD', 'N','Mean','SD', 'N', 'W', 'P-value' )
  summaryGroups
}

# Get graduate descriptive stats per Groups
# Subset the data for Question 1
lessGradData <- gradData[gradData$G1<8,]
greatedGradData <- gradData[gradData$G1>7,]
summaryGroups <- computeGroups(lessGradData[,2:6],greatedGradData[,2:6], activities, TRUE)

# Question 2
lessGradData <- gradData[gradData$G2<8,]
greatedGradData <- gradData[gradData$G2>7,]
summaryGroups <- computeGroups(lessGradData[,2:6],greatedGradData[,2:6], activities, TRUE )


# Question 3
lessGradData <- gradData[gradData$G3<6,]
greatedGradData <- gradData[gradData$G3>5,]
summaryGroups <- computeGroups(lessGradData[,2:6],greatedGradData[,2:6], activities, TRUE )

# Question 4
lessGradData <- gradData[gradData$G4<6,]
greatedGradData <- gradData[gradData$G4>5,]
summaryGroups <- computeGroups(lessGradData[,2:6],greatedGradData[,2:6], activities, TRUE )

# Question 5
lessGradData <- gradData[gradData$G5<8,]
greatedGradData <- gradData[gradData$G5>7,]
summaryGroups <- computeGroups(lessGradData[,2:6],greatedGradData[,2:6], activities, TRUE )


# Get undergraduate descriptive stats per Groups
# Subset the data for Question 1
lessUnderData <- underData[underData$X1A<9,]
greatedUnderData <- underData[underData$X1A>8,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )

# Question 2
lessUnderData <- underData[underData$X1B<9,]
greatedUnderData <- underData[underData$X1B>8,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )

# Question 3
lessUnderData <- underData[underData$X1C<9,]
greatedUnderData <- underData[underData$X1C>8,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )

# Question 4
lessUnderData <- underData[underData$X1D<6,]
greatedUnderData <- underData[underData$X1D>5,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )

# Question 5
lessUnderData <- underData[underData$X1E<8,]
greatedUnderData <- underData[underData$X1E>7,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )

# Question 6
lessUnderData <- underData[underData$X1F<6,]
greatedUnderData <- underData[underData$X1F>5,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )

# Question 7
lessUnderData <- underData[underData$X1G<6,]
greatedUnderData <- underData[underData$X1G>5,]
summaryGroups <- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )


laTable <- xtable(summaryGroups, 
                  caption = "\\textbf{Graduate Students' Preferences Grouped by Perceived Usefulness of Classroom Lectures}",
                  align=c("|p{0.01\\textwidth}|","p{0.08\\textwidth}|","p{0.03\\textwidth}|","p{0.02\\textwidth}|","p{0.02\\textwidth}|",
                          "p{0.03\\textwidth}|","p{0.02\\textwidth}|","p{0.02\\textwidth}|","p{0.035\\textwidth}|","p{0.03\\textwidth}|")
)
print(laTable,caption.placement = "top",  include.rownames=FALSE, floating = TRUE, latex.environments = "center")

