## REQUIREMENTS

## Merges the training and the test sets to create one data set.

## Extracts only the measurements on the mean and standard deviation for each measurement.

## Uses descriptive activity names to name the activities in the data set

## Appropriately labels the data set with descriptive variable names.

## From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

library(dplyr)

## SCRIPT

## Download data

zipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFILE <- "UCI HAR Dataset.zip"
dataPath <- "UCI HAR Dataset" 

# check if file exists
if (!file.exists(zipFILE)) {
  download.file(url = zipURL, destfile = zipFILE, method = "curl")
}

# unzip if not already unzipped
if (!file.exists(dataPath)) {
  unzip(zipFILE)
}

## Read Data

# Read test data
TestSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
TestValues <- read.table(file.path(dataPath, "test", "X_test.txt")) 
TestActivities <- read.table(file.path(dataPath, "test", "Y_test.txt"))

# Read training data
TrainSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
TrainValues <- read.table(file.path(dataPath, "train", "X_train.txt")) 
TrainActivities <- read.table(file.path(dataPath, "train", "Y_train.txt"))

# Read activity data
ActivityLabels <- read.table(file.path(dataPath, "activity_labels.txt"))
names(ActivityLabels) <- paste(c("ActivityID", "ActivityName"))

# Read features
FeatureLabels <- read.table(file.path(dataPath, "features.txt"))
names(FeatureLabels) <- paste(c("FeatureID", "FeatureName"))

## Merge Data

# Row-bind the c-bound train and test data
ActivityData <- rbind(
  cbind(TestSubjects, TestActivities, TestValues),
  cbind(TrainSubjects, TrainActivities, TrainValues)
)

names(ActivityData) <- paste(c("subject", "activity", FeatureLabels[,2]))
colnames(ActivityData) <- c("subject", "activity", as.character(FeatureLabels[,2]))

## Extract only mean and stddev (also keep activity and subject)

KeepCols <- grepl("subject|activity|mean|std", colnames(ActivityData))
ActivityData <- ActivityData[, KeepCols]

## Assign descriptive activity names

ActivityData$activity <- factor(ActivityData$activity, 
                                levels = ActivityLabels$ActivityID, labels = ActivityLabels$ActivityName)

## Label columns with describtive titles

# remove special characters by ""
colnames(ActivityData) <- gsub("[()-]", "", colnames(ActivityData))

# specify t and f: frequency and time domain
colnames(ActivityData) <- gsub("^f", "FrequencyDomain", colnames(ActivityData))
colnames(ActivityData) <- gsub("^t", "TimeDomain", colnames(ActivityData))

# correct "bodybody" typo to "body"
colnames(ActivityData) <- gsub("BodyBody", "Body", colnames(ActivityData))

# explain all other shortings
colnames(ActivityData) <- gsub("Acc", "Accelerometer", colnames(ActivityData))
colnames(ActivityData) <- gsub("Gyro", "Gyroscope", colnames(ActivityData))
colnames(ActivityData) <- gsub("Mag", "Magnitude", colnames(ActivityData))
colnames(ActivityData) <- gsub("Freq", "Frequency", colnames(ActivityData))
colnames(ActivityData) <- gsub("mean", "Mean", colnames(ActivityData))
colnames(ActivityData) <- gsub("std", "StandardDeviation", colnames(ActivityData))

## Create Data Set for each subject and activity

# Generate new data set with means for group - activity - subset
SumActivityData <- aggregate(ActivityData, 
                              by = list(ActivityData$subject, ActivityData$activity), 
                              FUN = mean)

# Clean new data set: Delete columns and edit names
SumActivityData <- select(SumActivityData, -c(3,4))
colnames(SumActivityData)[1:2] <- c("subject", "activity") 

# Output Data
write.table(ActivityData, "Tidy_ActivityData.txt", row.names = F, quote = F)
write.table(SumActivityData, "Tidy_SumActivityData.txt", row.names = F, quote = F)

saveRDS(object = ActivityData, file = "Tidy_ActivityData.rds")
saveRDS(object = SumActivityData, file = "Tidy_SumActivityData.rds")




















