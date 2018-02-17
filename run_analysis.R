# Getting and Cleaning Data Course Project

###Setting up a working environment
setwd("H:/2016_Samsung/Coursera_Video/Data Sciences/Data Cleaning/Week4")

###Preprocessing
if (!getwd() == "./Project1") {dir.create("./Project1") }

###Enable packages to use
rm(list = ls(all = TRUE))
library(plyr) # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization

###Download the zip file in Project1 folder
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile="./Project1/Dataset.zip")
unzip(zipfile="./Project1/Dataset.zip",exdir="./Project1")

###Read files in table format and create a data frame from them
Y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
Features <- read.table("UCI HAR Dataset/features.txt")
Activity_labels <- read.table("./Project1/UCI HAR Dataset/activity_labels.txt")

# Task 1: Merges the training and the test sets to create one data set.

###Fix Column Names - To merge the data, it is important to fix the names of tables X_test and X_train.
###These names are contained in the "Features" file.
colnames(X_train) <- t(Features[2])
colnames(X_test) <- t(Features[2])

###In addition, since the subjects and the activities of the experiment are in different tables,
###it is important to merge these tables (Subject_train, Subject_test) with the X_test and X_train
X_train$activity <- Y_train[, 1]
X_train$subject <- Subject_train[, 1]
X_test$activity <- Y_test[, 1]
X_test$subject <- Subject_test[, 1]

###Merge the X_test and X_train tables to create one data set
Data01 <- bind_rows(X_train, X_test)

# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
DataMean <- Data01[, grepl("mean()", colnames(Data01))]
DataStd <- Data01[, grepl("std()", colnames(Data01))]
Data02 <- bind_cols(DataMean, DataStd)

###Add colomns Subject and Activity from Data01 and rename these 2 colomns
Data02 <- cbind(Data01$subject, Data01$activity, Data02)
colnames(Data02)[1] <- "Subject"
colnames(Data02)[2] <- "Activity"

# Task 3: Uses descriptive activity names to name the activities in the data set
Data02$Activity <- factor(Data02$Activity,
                          levels = Activity_labels[, 1], labels = Activity_labels[, 2])

# Task 4: Appropriately labels the data set with descriptive variable names.
###Use gsub to replace that list
coldata02 <- gsub("-", " ", colnames(Data02))
colnames(Data02) <- coldata02

names(Data02) <- gsub("^t", "Time ", names(Data02))
names(Data02) <- gsub("^f", "Frequency ", names(Data02))
names(Data02) <- gsub("Acc", " Accelerator ", names(Data02))
names(Data02) <- gsub("Mag", " Magnitude ", names(Data02))
names(Data02) <- gsub("Gyro", " Gyroscope ", names(Data02))

# Task 5: Create a tidy data set
###Use lapply to takes the mean of every column broken down by subject and activity
Data03 <- data.table(Data02)
Data03 <- Data03[, lapply(.SD, mean), by = 'Subject,Activity']
Data03 <- Data03[order(Data03$Subject, Data03$Activity)]

write.table(Data03, file = "TidyData.txt", row.names = FALSE)
