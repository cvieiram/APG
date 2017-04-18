
# Substracts all columns from the classroom lectures to see which one they prefered.
substractedGrads <- sapply(gradData[,3:6], function(x,y){ x-y}, y=gradData[,2])
substractedGrads <- cbind(gradData[,1:2],substractedGrads)

# Get the less than and greater than groups
# Subset the data for Question 1
lessGradData <- substractedGrads[gradData$G1<8,]
lessGradData$group <- "Lower"
greatedGradData <- substractedGrads[gradData$G1>7,]
greatedGradData$group <- "Higher"
# Descriptive and Inferential Stats
summaryGroups <- computeGroups(lessGradData[,2:6],greatedGradData[,2:6], activities, TRUE)
# Plotting the resulting preferences
# c("Classroom Lecture", "Homework Assignments","Peer Review","Reading Peer Review", "sLectures")
act <- c("Classroom Lecture", "Reading P-R" ,"Making P-R","Projects", "Slectures")
lessGradData <- lessGradData[,c(1,2,5,4,3,6,7)]
greatedGradData <- greatedGradData[,c(1,2,5,4,3,6,7)]

plotPreferences(lessGradData, greatedGradData, act)


# Undegrads
# Substracts all columns from the classroom lectures to see which one they prefered.
substractedUnderg <- sapply(underData[,3:8], function(x,y){ x-y}, y=underData[,2])
substractedUnderg <- cbind(underData[,1:2],substractedUnderg)

lessUnderData <- substractedUnderg[substractedUnderg$X1A<9,]
lessUnderData$group <- "Lower"
greatedUnderData <- substractedUnderg[substractedUnderg$X1A>8,]
greatedUnderData$group <- "Higher"
summaryGroups<- computeGroups(lessUnderData[,2:8],greatedUnderData[,2:8], activitiesU, FALSE )
#activitiesU <- c("Classroom Lecture","Hand outs" ,"Homework Assignments","Blogs","sLectures", "Review sLectures", "Reading Reviews")
actU <- c("Blogs", "Classroom Lecture","Reading P-R","Handouts" , "Making P-R", "Homeworks","SLectures")
lessUnderData <- lessUnderData[,c(1,5,2,8,3,7,4,6,9)]
greatedUnderData <- greatedUnderData[,c(1,5,2,8,3,7,4,6,9)]

plotPreferences(lessUnderData, greatedUnderData, actU)



laTable <- xtable(summaryGroups, 
                  caption = "\\textbf{Undergraduate Students' Preferences Grouped by Perceived Usefulness of Classroom Lectures}",
                  align=c("|p{0.01\\textwidth}|","p{0.08\\textwidth}|","p{0.03\\textwidth}|","p{0.02\\textwidth}|","p{0.02\\textwidth}|",
                          "p{0.03\\textwidth}|","p{0.02\\textwidth}|","p{0.02\\textwidth}|","p{0.035\\textwidth}|","p{0.03\\textwidth}|")
)
print(laTable,caption.placement = "top",  include.rownames=FALSE, floating = TRUE, latex.environments = "center")

