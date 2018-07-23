# 1. Merges the training and the test sets to create one data set.

## step 1: download zip file from website and unzip
if(!file.exists("./data")) dir.create("./data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/UCI_Dataset.zip")
listZip <- unzip("./data/UCI_Dataset.zip", exdir = "./data")

## step 2: load data into R
train.x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train.y <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
test.x <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test.y <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## step 3: merge train and test data
train_Data <- cbind(train.subject, train.y, train.x)
test_Data <- cbind(test.subject, test.y, test.x)
full_Data <- rbind(train_Data, test_Data)

#-------------------------------------------------------------------------------
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

## step 1: load feature name into R
featureName <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]

## step 2:  extract mean and standard deviation of each measurements
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
final_Data <- full_Data[, c(1, 2, featureIndex+2)]
colnames(final_Data) <- c("subject", "activity", featureName[featureIndex])

#-------------------------------------------------------------------------------
# 3. Uses descriptive activity names to name the activities in the data set

## step 1: load activity data into R
activity_Name <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

## step 2: replace 1 to 6 with activity names
final_Data$activity <- factor(final_Data$activity, levels = activity_Name[,1], labels = activity_Name[,2])

#-------------------------------------------------------------------------------
# 4. Appropriately labels the data set with descriptive variable names.

names(final_Data) <- gsub("\\()", "", names(final_Data))
names(final_Data) <- gsub("^t", "time", names(final_Data))
names(final_Data) <- gsub("^f", "frequence", names(final_Data))
names(final_Data) <- gsub("-mean", "Mean", names(final_Data))
names(final_Data) <- gsub("-std", "Std", names(final_Data))

#-------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
group_Data <- final_Data %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

write.table(group_Data, "./data/MeanData.txt", row.names = FALSE)