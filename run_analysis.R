# Getting and Cleaning Data Project John Hopkins Coursera
# Author: Khin Myo Thant Maw

# 1. Merges the training and the test sets to create one data set.

activities <- read.table('../UCI HAR DataSet/activity_labels.txt', col.names = c("activityId", "activityName"), quote = "")
features <- read.table('../UCI HAR DataSet/features.txt', col.names = c("featureId", "featureName"), quote = "")

training_data <- read.table('../UCI HAR DataSet/train/X_train.txt', col.names = features$featureName)
training_activities <- read.table('../UCI HAR DataSet/train/Y_train.txt' , col.names = "Activity")
training_subject <- read.table('../UCI HAR DataSet/train/subject_train.txt', col.names = "Subject")

test_data <- read.table('../UCI HAR DataSet/test/X_test.txt', col.names = features$featureName)
test_activities <- read.table('../UCI HAR DataSet/test/Y_test.txt', col.names = "Activity")
test_subject <- read.table('../UCI HAR DataSet/test/subject_test.txt', col.names = "Subject")

training <- cbind(training_subject, training_activities, training_data)
test <- cbind(training_subject, training_activities, training_data)

allData <- rbind(training, test)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
required_features = grep("(mean|std)\\(\\)", features$featureName )
required_features = required_features + 2 
mean_and_std_data = allData[, c(1,2, c(required_features))]   # 1 = "Subject" , 2 = "Activity name"

# 3. Uses descriptive activity names to name the activities in the data set
allData$Activity <- factor(allData$Activity, levels = activities$activityId, labels = activities$activityName)
mean_and_std_data$Activity <- factor(mean_and_std_data$Activity, levels = activities$activityId, labels = activities$activityName)

# 4. Appropriately labels the data set with descriptive variable names.
labelNames = c("Subject", "Activity", features$featureName )
colnames(allData) <- labelNames
required_features = as.vector(required_features - 2)
required_labels <- c("Subject", 'Activity', features$featureName[required_features])
colnames(mean_and_std_data)= required_labels

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Use mean_and_std_data for this step
library(dplyr)
group_data_set <- group_by(mean_and_std_data, Activity, Subject)
second_data_set <- summarise(group_data_set, across(everything(), list(mean)))

# Generate second_data_set
write.table(second_data_set, "tidyDataSet.txt", row.names = FALSE)
