# 1. Load libraries
library(tidyr)
library(dplyr)


# 2. Read files into tables
 y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")
 x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
 
 x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
 y_test <- read.table("UCI HAR Dataset/test/Y_test.txt")
 
 subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
 subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
 
 activities <- read.table("UCI HAR Dataset/activity_labels.txt")
 features <- read.table("UCI HAR Dataset/features.txt")
 



 # 3. Re-labels the columns in tables 
 # To fullfill "Appropriately labels the data set with descriptive variable names."
 colnames(y_test) <- "activity.id"
 colnames(y_train) <- "activity.id"
 colnames(subject_train) <- "subject.id"
 colnames(subject_test) <- "subject.id"
 colnames(features) <- c("measure.id","measure.name")
 colnames(activities) <- c("activity.id","activity.name")
 
 colnames(x_test) <- features[,2]
 colnames(x_train) <- features[,2]



 # 4. Merges data tables
 # To fullfill "Uses descriptive activity names to name the activities in the data set"
 # Add activity name to y_test and y_train
 y_train_with_activity <- merge(y_train,activities, by="activity.id")
 y_test_with_activity  <- merge(y_test,activities,  by="activity.id")
 
 # add subject to right of y_dataset big dataset
 y_test_with_act_sub  <- cbind(y_test_with_activity,  subject_test)
 y_train_with_act_sub <- cbind(y_train_with_activity, subject_train)
 
 # Merge train and test
 total_x <- rbind(x_train,x_test)
 total_y <- rbind(y_train_with_act_sub,y_test_with_act_sub)
 
 


 # 5. Extracts measures related to mean or std
 # To fullfill "extract only measurement with mean and std"
 # I'm sure there is better way to do this but running out of time
 total_x_mean <- subset(total_x,
                          select = (grep("mean",colnames(total_x) ) 
                                   )
                          )
 total_x_std <- subset(total_x,
                        select = (grep("std",colnames(total_x) ) 
                        )
 ) 
 total_x_mean_std <- cbind(total_x_mean,total_x_std)


 # Add subject and activity to beggining of dataset
 total_x_y <- cbind(total_y,total_x_mean_std)
 # Convert subject.id to factor type
 total_x_y <- mutate(total_x_y,subject.id = factor(subject.id))
 


 
 # 6. Creates tidy data set
 # To fullfill "From the data set in step 4, creates a second, 
 # independent tidy data set with the average of each variable 
 # for each activity and each subject".

 # To create tidy dataset (narrow form)
 # 1. will drop activity id column
 # 2. make it narrow form using gather
 # 3. calculate avg per activity , subject and measure

 # Drop activity id column
 total_x_y[1] <- NULL
 
 # Convert to tbl_df to use dyplr package functions
 total_x_y_tbl <- tbl_df(total_x_y)


 tidy_x <- gather(total_x_y_tbl, measure.name, value, -(activity.name:subject.id))
 
 # calculate avg
 total_x_y_summ <- group_by(tidy_x,activity.name,subject.id,measure.name)
 final_set <- summarize(total_x_y_summ , mean(value))



 # 7. Exports file tidy_samsung_set.txt
 # Write file
 write.table(final_set,file="tidy_samsung_set.txt", row.names=FALSE)
 
 