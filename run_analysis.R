#load libraries
library(dplyr)
library(data.table)

#load data
setwd("/Users/rxu/Downloads/UCI HAR Dataset")
y_train <- read.table("train/y_train.txt", header=FALSE)
y_test <- read.table("test/y_test.txt", header=FALSE)

features_labels <- read.table("features.txt", header=FALSE)
activity_labels <- read.table("activity_labels.txt", header=FALSE)

subject_train <- read.table("train/subject_train.txt", header=FALSE)
subject_test <- read.table("test/subject_test.txt", header=FALSE)

X_train <- read.table("train/X_train.txt", header=FALSE)
X_test <- read.table("test/X_test.txt", header=FALSE)

#merge training and test sets

subject <- rbind(subject_train, subject_test)
activity <- rbind(y_train, y_test)
features <- rbind(X_train, X_test)

colnames(features) <- t(features_labels[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
allData <- cbind(features, activity, subject)

#extract mean and dev
meanstdevdata <- grep(".*Mean.*|.*Std.*", names(allData), ignore.case=TRUE)

requiredColumns <- c(meanstdevdata, 562, 563)
dim(allData)

extractedData <- allData[,requiredColumns]
dim(extractedData)

#name activities
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activity_labels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

#label data set
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#tidyData set
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

