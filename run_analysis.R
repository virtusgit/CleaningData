# this script assumes working directory is the 'UCI HAR Dataset'

# reading train data
tbl_X_train <- read.table("train/X_train.txt")
tbl_y_train <- read.table("train/y_train.txt")
tbl_subject_train <- read.table("train/subject_train.txt")

# reading test data
tbl_X_Test <- read.table("test/X_test.txt")
tbl_y_Test <- read.table("test/y_test.txt")
tbl_subject_test <- read.table("test/subject_test.txt")

# reading variables' names
tbl_features <- read.table("features.txt")
tbl_activities <- read.table("activity_labels.txt")

# 1.Merges the training and the test sets to create one data set.

# merge all test data, and all train data, and connect the two data frames
testfull <- cbind(tbl_X_Test,tbl_subject_test,tbl_y_Test)
trainfull <- cbind(tbl_X_train,tbl_subject_train,tbl_y_train)
df <- rbind(testfull,trainfull)

# clears memory removing all present variables except bigdf (needed for data),
# and tbl_activities and tbl_features (needed for labeling)
rm(list = setdiff(ls(), c("df","tbl_activities","tbl_features")))

#2.Extracts only the measurements on the mean and standard deviation for 
# each measurement.

# creating indices for mean() and std() variables
indmean <- grep("mean\\(\\)", tbl_features[,2], value = FALSE)
indstd <- grep("std\\(\\)", tbl_features[,2], value = FALSE)

# creating indices vector, for all measured mean and std variables indices
# including extra indices for 'subject number' (562) and 'activity' (563) 
indices <- append(indmean, c(indstd, dim(df)[2]-1, dim(df)[2]))

# subsetting on created index
df <- df[,indices]

# 3.Uses descriptive activity names to name the activities in the data set

# uses labels in tbl_activities to relabel last column of data frame
for(i in 1:6) {
      df[,dim(df)[2]] <- gsub(i, tbl_activities[,2][i], df[,dim(df)[2]]);
      }

# 4.Appropriately labels the data set with descriptive variable names.

# creates names vector
namesvect <- as.character(tbl_features[indices, 2])
namesvect[length(namesvect)-1] <- "subject"
namesvect[length(namesvect)] <- "activity"

# assigns names to df columns
names(df) <- namesvect

# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

library(dplyr)
by_sub_act <- df %>% group_by( subject, activity) %>% summarise_all(mean)

# create the tidydata.txt file to store the by_sub_act data frame
write.table(by_sub_act,"tidydata.txt",sep="\t",row.names=FALSE)