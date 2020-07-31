# Function - The script 'run_analysis.R' perform the following.
# 1. Loads the data into memory vby reading the file from the given directory name
# 2. Merges the training and the test sets to create one data set.
# 3. Extracts only the measurements on the mean and standard deviation for each measurement.
# 4. Uses descriptive activity names to name the activities in the data set.
# 5. Appropriately labels the data set with descriptive variable names.
# 6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Expected Result:  
# 1. Result stored in a new text file based on the given name (or default = 'tidy_data.txt') 
#    in the same directory of where the script executed
#           
# Details on the tidy/clean data set table can be found at 'CodeBook.md' in the same location as this script
#
run_analysis <- function(directory="", fileTidy=""){
    #load necessary library
    library(data.table);
    library(dplyr);
    
    #setting the directory
    if(directory == ""){
        directory = paste(getwd(),"/Dataset/", sep ="");
    }
    if(fileTidy == ""){
        fileTidy = paste(getwd(),"/tidy_data.txt", sep="");
    }
    
    # 1. Loads the data into memory vby reading the file from the given directory name
    # Read feature list
    if(file.exists(paste(directory,"features.txt", sep = ""))){
        feature_list <- read.table(paste(directory,"features.txt", sep = ""));
    } else {
        stop(paste(paste(directory,"features.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    # Read the activity label
    if(file.exists(paste(directory,"activity_labels.txt", sep = ""))){
        activity_labels <- read.table(paste(directory,"activity_labels.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"activity_labels.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    # Read the train data
    if(file.exists(paste(directory,"train/subject_train.txt", sep = ""))){
        subject_train <- read.table(paste(directory,"train/subject_train.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"train/subject_train.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    if(file.exists(paste(directory,"train/X_train.txt", sep = ""))){
        x_train <- read.table(paste(directory,"train/X_train.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"train/X_train.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    if(file.exists(paste(directory,"train/Y_train.txt", sep = ""))){
        y_train <- read.table(paste(directory,"train/Y_train.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"train/Y_train.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    # Read the test data
    if(file.exists(paste(directory,"test/subject_test.txt", sep = ""))){
        subject_test <- read.table(paste(directory,"test/subject_test.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"test/subject_test.txt", sep = ""), " does not exist or inaccessible"));
    }
    if(file.exists(paste(directory,"test/X_test.txt", sep = ""))){
        x_test <- read.table(paste(directory,"test/X_test.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"test/X_test.txt", sep = ""), " does not exist or inaccessible"));
    }
    if(file.exists(paste(directory,"test/Y_test.txt", sep = ""))){
        y_test <- read.table(paste(directory,"test/Y_test.txt", sep = ""), header = FALSE);
    } else {
        stop(paste(paste(directory,"test/Y_test.txt", sep = ""), " does not exist or inaccessible"));
    }
    
    # 2. Merges the training and the test sets to create one data set.
    # merge the test and train data set
    merge_subject <- rbind(subject_train, subject_test);
    merge_x <- rbind(x_train, x_test);
    merge_y <- rbind(y_train, y_test);
    # name the columns
    colnames(merge_subject) <- "Subject";
    colnames(merge_x) <- t(feature_list[2]);
    colnames(merge_y) <- "Activity";
    # combine all data into one data set
    merge_data <- cbind(merge_x, merge_y, merge_subject);
    
    # 3. Extracts only the measurements on the mean and standard deviation for each measurement.
    # looking at the merge data set
    head(merge_data);
    dim(merge_data);
    # getting the subset of the merge data for mean and std
    mean_std_subset<-feature_list$V2[grep("mean\\(\\)|std\\(\\)", feature_list$V2)]
    # see the subset data
    head(mean_std_subset);
    # getting the actual data set that contains the value
    selected_names<-c(as.character(mean_std_subset), "Subject", "Activity" )
    mean_std_data_from_mergedata<-subset(merge_data,select=selected_names)
    # looking at the set again
    head(mean_std_data_from_mergedata);
    dim(mean_std_data_from_mergedata);
    
    # 4. Uses descriptive activity names to name the activities in the data set.
    mean_std_data_from_mergedata$Activity<-factor(mean_std_data_from_mergedata$Activity,labels=activity_labels[,2]);
    # looking at the set again after changes on the activity name
    head(mean_std_data_from_mergedata$Activity);
    
    # 5. Appropriately labels the data set with descriptive variable names.
    names(mean_std_data_from_mergedata)[2] = "Activity";
    names(mean_std_data_from_mergedata)<-gsub("Acc", "Accelerometer", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("Gyro", "Gyroscope", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("BodyBody", "Body", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("Mag", "Magnitude", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("^t", "Time", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("^f", "Frequency", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("tBody", "TimeBody", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("-mean()", "Mean", names(mean_std_data_from_mergedata), ignore.case = TRUE);
    names(mean_std_data_from_mergedata)<-gsub("-std()", "STD", names(mean_std_data_from_mergedata), ignore.case = TRUE);
    names(mean_std_data_from_mergedata)<-gsub("-freq()", "Frequency", names(mean_std_data_from_mergedata), ignore.case = TRUE);
    names(mean_std_data_from_mergedata)<-gsub("angle", "Angle", names(mean_std_data_from_mergedata));
    names(mean_std_data_from_mergedata)<-gsub("gravity", "Gravity", names(mean_std_data_from_mergedata));
    # view the data again
    names(mean_std_data_from_mergedata);
    
    # 6. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    final_tidy_data<-aggregate(. ~Subject + Activity, mean_std_data_from_mergedata, mean);
    final_tidy_data<-final_tidy_data[ order(final_tidy_data$Subject, final_tidy_data$Activity), ];
    # Write the result into a file
    write.table(final_tidy_data, file = fileTidy, row.name=FALSE, quote = FALSE, sep = '\t');
}


