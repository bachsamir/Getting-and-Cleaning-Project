# Getting-and-Cleaning-Project
Getting and Cleaning Smartphones Data Set 
#Setting up a working environment
setwd("H:/2016_Samsung/Coursera_Video/Data Sciences/Data Cleaning/Week4")

#Getting and Cleaning Data Course Project
#Preprocessing
if (!getwd() == "./Project1") {
  dir.create("./Project1")
}

#Activate packages
rm(list = ls(all = TRUE))
library(plyr) # load plyr first, then dplyr 
library(data.table) # a prockage that handles dataframe better
library(dplyr) # for fancy data table manipulations and organization

#Download the zip file in Project1 folder
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile="./Project1/Dataset.zip")
unzip(zipfile="./Project1/Dataset.zip",exdir="./Project1")

Y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
Features <- read.table("UCI HAR Dataset/features.txt")
Activity_labels <- read.table("./Project1/UCI HAR Dataset/activity_labels.txt")

#To view the content, format and perform tests
head(X_test)
head(Y_test)
head(Features)
head(Activity_labels)

#Fix Column Names
colnames(X_train) <- t(Features[2])
colnames(X_test) <- t(Features[2])
X_train$activities <- Y_train[, 1]
X_train$participants <- Subject_train[, 1]
X_test$activities <- Y_test[, 1]
X_test$participants <- Subject_test[, 1]

# Task 1: Merges the training and the test sets to create one data set.
Data01 <- rbind(X_train, X_test)
duplicated(colnames(Data01)) # Find if there is any colomn which is duplicated
Data01 <- Data01[, !duplicated(colnames(Data01))] # Deleted the duplicated column

# Task 2: Extracts only the measurements on the mean and standard deviation for each measurement.
Mean <- grep("mean()", names(Data01), value = FALSE, fixed = TRUE)
#In addition, we need to include 555:559 as they have means and are associated with the gravity terms
Mean <- append(Mean, 471:477)
Data02 <- Data01[Mean]
STD <- grep("std()", names(Data01), value = FALSE)
Data03 <- Data01[STD]

# Task 3: Uses descriptive activity names to name the activities in the data set
#Changing the class is useful for replacing strings
Data01$activities <- as.character(Data01$activities)
#Data01$activities <- Activity_labels$V2[Data01$activities]
Data01$activities[Data01$activities == 1] <- "Walking"
Data01$activities[Data01$activities == 2] <- "Walking Upstairs"
Data01$activities[Data01$activities == 3] <- "Walking Downstairs"
Data01$activities[Data01$activities == 4] <- "Sitting"
Data01$activities[Data01$activities == 5] <- "Standing"
Data01$activities[Data01$activities == 6] <- "Laying"
Data01$activities <- as.factor(Data01$activities)

# Task 4: Appropriately labels the data set with descriptive variable names.
names(Data01)
#Use gsub to replace that list
names(Data01) <- gsub("Acc", "Accelerator", names(Data01))
names(Data01) <- gsub("Mag", "Magnitude", names(Data01))
names(Data01) <- gsub("Gyro", "Gyroscope", names(Data01))
names(Data01) <- gsub("^t", "time", names(Data01))
names(Data01) <- gsub("^f", "frequency", names(Data01))

#Change participants names
Data01$participants <- as.character(Data01$participants)
Data01$participants[Data01$participants == 1] <- "Participant 1"
Data01$participants[Data01$participants == 2] <- "Participant 2"
Data01$participants[Data01$participants == 3] <- "Participant 3"
Data01$participants[Data01$participants == 4] <- "Participant 4"
Data01$participants[Data01$participants == 5] <- "Participant 5"
Data01$participants[Data01$participants == 6] <- "Participant 6"
Data01$participants[Data01$participants == 7] <- "Participant 7"
Data01$participants[Data01$participants == 8] <- "Participant 8"
Data01$participants[Data01$participants == 9] <- "Participant 9"
Data01$participants[Data01$participants == 10] <- "Participant 10"
Data01$participants[Data01$participants == 11] <- "Participant 11"
Data01$participants[Data01$participants == 12] <- "Participant 12"
Data01$participants[Data01$participants == 13] <- "Participant 13"
Data01$participants[Data01$participants == 14] <- "Participant 14"
Data01$participants[Data01$participants == 15] <- "Participant 15"
Data01$participants[Data01$participants == 16] <- "Participant 16"
Data01$participants[Data01$participants == 17] <- "Participant 17"
Data01$participants[Data01$participants == 18] <- "Participant 18"
Data01$participants[Data01$participants == 19] <- "Participant 19"
Data01$participants[Data01$participants == 20] <- "Participant 20"
Data01$participants[Data01$participants == 21] <- "Participant 21"
Data01$participants[Data01$participants == 22] <- "Participant 22"
Data01$participants[Data01$participants == 23] <- "Participant 23"
Data01$participants[Data01$participants == 24] <- "Participant 24"
Data01$participants[Data01$participants == 25] <- "Participant 25"
Data01$participants[Data01$participants == 26] <- "Participant 26"
Data01$participants[Data01$participants == 27] <- "Participant 27"
Data01$participants[Data01$participants == 28] <- "Participant 28"
Data01$participants[Data01$participants == 29] <- "Participant 29"
Data01$participants[Data01$participants == 30] <- "Participant 30"
Data01$participants <- as.factor(Data01$participants)

# Task 5: Create a tidy data set
Data04 <- data.table(Data01)
#This takes the mean of every column broken down by participants and activities
Data05 <- Data04[, lapply(.SD, mean), by = 'participants,activities']
write.table(Data05, file = "Data_Tidy.txt", row.names = FALSE)
#Complete [see annotations in ReadMe]
