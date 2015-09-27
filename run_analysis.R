#Week 3 Course Project

install.packages("data.table")
library(data.table)
library(dplyr)

#Read Labels
features<- read.table("features.txt", header = F)
activityLabel<- read.table("activity_labels.txt", header = FALSE)

#Read Training Data
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
featureTrain <- read.table("./train/X_train.txt", header = F)
activityTrain <- read.table("./train/y_train.txt", header = F)


#Read Test Data
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
featureTest <- read.table("./test/X_test.txt", header = F)
activityTest <- read.table("./test/y_test.txt", header = F)

str(featureTest)

# 1, Merges the training and the test sets to create one data set.
mergedSubject <- rbind(subjectTrain, subjectTest)
mergedActivityLabel <- rbind(activityTrain, activityTest)
mergedfeatures <- rbind(featureTrain,featureTest)

names(mergedSubject)<-c("subject")
names(mergedActivityLabel)<-c("activity")
names(mergedfeatures) <- features$V2

TotalData <- cbind(mergedfeatures,mergedSubject, mergedActivityLabel)

# 2, Extracts only the measurements on the mean and standard deviation for each measurement. 

TotalDataMeanStd <- features$V2[grep("mean\\(\\)|std\\(\\)", features$V2)]

selectedNames<-c(as.character(TotalDataMeanStd), "subject", "activity" )
Data<-subset(TotalData,select=selectedNames)

str(Data)

# 3, Uses descriptive activity names to name the activities in the data set
Data$activity <- as.character(Data$activity)
for (i in 1:6){
  Data$activity[Data$activity == i] <- as.character(activityLabel[i,2])
}
Data$activity <- as.factor(Data$activity)

# 4.Appropriately labels the data set with descriptive variable names.
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("^t", "Time", names(Data))
names(Data)<-gsub("^f", "Frequency", names(Data))
names(Data)<-gsub("tBody", "TimeBody", names(Data))
names(Data)<-gsub("-mean()", "Mean", names(Data), ignore.case = TRUE)
names(Data)<-gsub("-std()", "STD", names(Data), ignore.case = TRUE)
names(Data)<-gsub("-freq()", "Frequency", names(Data), ignore.case = TRUE)
names(Data)<-gsub("angle", "Angle", names(Data))
names(Data)<-gsub("gravity", "Gravity", names(Data))

names(Data)


# 5. From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

Data$subject <- as.factor(Data$subject)
Data <- data.table(Data)

tidyData <- aggregate(. ~subject + activity, Data, mean)
tidyData <- tidyData[order(tidyData$subject,tidyData$activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

