library(dplyr)

## Assigning all data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")


##Merge the training and the test data sets to create one data set.
xdata <- rbind(x_train, x_test)
ydata <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
merged_data <- cbind(subject, ydata, xdata)

##Extract only the measurements on the mean and standard deviation for each measurement.
tidy_data <- select(merged_data, subject, code, contains("mean"), contains("std"))

##Use descriptive activity names to name the activities in the data set.
tidy_data$code <- activities[tidy_data$code, 2]

##Appropriately label the data set with descriptive variable names.
names(tidy_data)[2] = "Activity"
names(tidy_data)<-gsub("^t", "Time", names(tidy_data))
names(tidy_data)<-gsub("^f", "Frequency", names(tidy_data))
names(tidy_data)<-gsub("angle", "Angle", names(tidy_data))
names(tidy_data)<-gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data)<-gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data)<-gsub("-freq()", "Frequency", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("gravity", "Gravity", names(tidy_data))
names(tidy_data)<-gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data)<-gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data)<-gsub("-mean()", "Mean", names(tidy_data), ignore.case = TRUE)
names(tidy_data)<-gsub("tBody", "TimeBody", names(tidy_data))
names(tidy_data)<-gsub("-std()", "STD", names(tidy_data), ignore.case = TRUE)

## Step 5: From the data set in step 4, create a second, independent tidy
##data set with the average of each variable for each activity and each subject.
TDS <- group_by(tidy_data,subject, Activity)
TIDY_DATA_SET <- summarise_all(T1, funs(mean))
write.table(TIDY_DATA_SET, "TIDY_DATA_SET.txt", row.name=FALSE)