
##Load raw data
#load activity_labels.txt
#6 x 2
activity_labels <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
names(activity_labels) <- c("activity_id", "activity_name")
#load features.txt
#561 x 2
features <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
names(features) <- c("feature_id", "feature_name")
#load train data
#7532 x 1
subject_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
names(subject_train) <- c("subject_id")
#7532 x 561
X_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
#7532 x 1
y_train <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
names(y_train) <- c("activity")
#load test data
#2947 x 1
subject_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
names(subject_test) <- c("subject_id")
#2947 x 561
X_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
#2947 x 1
y_test <- read.table("./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
names(y_test) <- c("activity")

## Process data
#Step1
#Combine X_train and y_train data to all_train
all_train <- cbind(subject_train, X_train, y_train)
#combine X_test and y_test to all_test
all_test <- cbind(subject_test, X_test, y_test)
#combine all_train and all_test data
#combine train and test data
all_data <- rbind(X_train, X_test)

#Step 2
#change columns names as feature names
names(all_data) <- as.vector(features[,2])
#select column names contaiming std and mean
all_data <- all_data[, as.vector(grep("std|mean", colnames(all_data), 
                                      value=FALSE))]
#Add subject and y (activity_id) data columns to above filtered data
subject_data <- rbind(subject_train, subject_test)
y_data <- rbind(y_train, y_test)
all_data <- cbind(subject_data, all_data, y_data)    

#Step 3
#Replace activity numbers with descrpitive activity names
library(plyr)
all_data$activity <- mapvalues(all_data$activity, c(1,2,3,4,5,6), 
                    as.character(activity_labels$activity_name))

#Step 4 - Make column names more descriptive
#Remove () after functions like std, mean
colnames(all_data) <- gsub("\\(\\)", "", colnames(all_data))
colnames(all_data) <- gsub("-X", "-X-axis", colnames(all_data))
colnames(all_data) <- gsub("-Y", "-Y-axis", colnames(all_data))
colnames(all_data) <- gsub("-Z", "-Z-axis", colnames(all_data))

#Step 5 - Create indendent tidy dataset with average of each variable for 
#each activity and each subject
library(dplyr, warn.conflicts=FALSE)
tidy_data <- group_by(all_data, subject_id, activity) 
tidy_data <- summarise_each(tidy_data, funs(mean))

##Dump tidy data to file
write.table(tidy_data, file="tidy_data_project.txt", row.names=FALSE, col.names=TRUE,
            sep="\t", quote=TRUE)
