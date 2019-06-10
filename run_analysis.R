## Peer-graded Assignment: Getting and Cleaning Data Course Project
## Download zip file and unzip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "dataset.zip")
unzip("dataset.zip")

## 1. Merges the training and the test sets to create one data set.
## i. Read the files
x_train <- read.table(file = "UCI HAR Dataset/train/X_train.txt",
                    sep = "", header = FALSE)
y_train <- read.table(file = "UCI HAR Dataset/train/Y_train.txt",
                      sep = "", header = FALSE)
s_train <- read.table(file = "UCI HAR Dataset/train/subject_train.txt",
                      sep = "", header = FALSE)

x_test <- read.table(file = "UCI HAR Dataset/test/X_test.txt",
                   sep = "", header = FALSE)
y_test <- read.table(file = "UCI HAR Dataset/test/Y_test.txt",
                     sep = "", header = FALSE)
s_test <- read.table(file = "UCI HAR Dataset/test/subject_test.txt",
                     sep = "", header = FALSE)

features <- read.table(file = "UCI HAR Dataset/features.txt", 
                       sep = "", header = FALSE)
activities <- read.table(file = "UCI HAR Dataset/activity_labels.txt", 
                       sep = "", header = FALSE)

## ii. Merge the datasets
s_data <- rbind(s_train, s_test)
y_data <- rbind(y_train, y_test)
x_data <- rbind(x_train, x_test)

## iv. rename columns for all data sets
colnames(s_data) <- "subject"
colnames(y_data) <- "activity" 
colnames(x_data) <- as.character(features$V2)
colnames(activities) <- c("activity", "activityName")

## 2.	Extracts only the measurements on the mean and 
##    standard deviation for each measurement.
## i. Get variables for mean and std
vars <- grep("(std|mean)\\(\\)", as.character(features$V2))
varNames <- features[vars,2]
## ii. Extract columns for mean and std
x_extracted <- x_data[, vars]

## 3.	Uses descriptive activity names to name the activities in the data set
## i. Merge all datasets
all_data <- cbind(s_data, y_data, x_extracted)

## ii. Replace activity id with activity names
library(dplyr)
all_data <- left_join(all_data, activities, by = "activity")

## 4.	Appropriately labels the data set with descriptive variable names.
## i. Get all column names
columnNames <- colnames(all_data)

## ii. Find and replace short words with descriptive words 
columnNames <- gsub("[\\(\\)\\-]", "", columnNames)
columnNames <- gsub("mean", "Mean", columnNames)
columnNames <- gsub("std", "StandardDeviation", columnNames)
columnNames <- gsub("Acc", "Accelerometer", columnNames)
columnNames <- gsub("Gyro", "Gyroscope", columnNames)
columnNames <- gsub("Mag", "Magnitude", columnNames)
columnNames <- gsub("BodyBody", "Body", columnNames)
columnNames <- gsub("^t", "Time", columnNames)
columnNames <- gsub("^f", "Frequency", columnNames)

## iii. Rename columns 
colnames(all_data) <- columnNames
all_data$activity <- all_data$activityName.x

## 5.	From the data set in step 4, creates a second, 
## independent tidy data set with the average of each variable 
## for each activity and each subject.

## i. Group by and summarize data
library(tidyr)
TidyData <- all_data %>%  group_by(subject, activityName) %>%
    summarize_each(funs(mean))
  
## ii. output to file 
write.table(TidyData, "tidy_data.txt", row.names = FALSE, 
              quote = FALSE)