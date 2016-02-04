## This R script collates and cleans Human Activity Data
## This link explains the data set
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## The data is divided into 2 data sets, Test Data Set and Training Data Set
## Each data set comprise of 561 time and frequency domain variables

## This script does the following as part of the assignment
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


run_analysis <- function() {
    library(dplyr)
    
    ## 0. Download and unzip data files.
    
    download.file(
        "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","getdata_projectfiles_UCI HAR Dataset.zip"
    )
    unzip("getdata_projectfiles_UCI HAR Dataset.zip")
    
    
    ## 1. Merge the training and the test sets to create one data set.
    
    ## Read Test Data
    testdata <- read.table("UCI HAR Dataset/test/X_test.txt")
    
    ## Read Training Data
    traindata <- read.table("UCI HAR Dataset/train/X_train.txt")
    
    ## Merge the training and the test data sets
    totdata <- rbind(testdata,traindata)
    
    ## 2. Extract only the measurements on the mean and standard deviation for each measurement.
    
    ## Create variable list
    features <- read.table("UCI HAR Dataset/features.txt")
    
    ## Create index array for mean() & std() type of variables
    meansstds <-
        c(grep("-mean()",features$V2, fixed = T),grep("-std()",features$V2, fixed = T))
    
    ## Extract only the mean and standard deviation variables from the merged data set
    meanstddata <- totdata[,meansstds]
    
    ## 3. Use descriptive activity names to name the activities in the data set
    
    ## Create activity list
    activities <- read.table("UCI HAR Dataset/activity_labels.txt")
    
    ## Create test activity list
    testy <- read.table("UCI HAR Dataset/test/y_test.txt")
    testactivities <- activities$V2[testy$V1]
    
    ## Create train activity list
    trainy <- read.table("UCI HAR Dataset/train/y_train.txt")
    trainactivities <- activities$V2[trainy$V1]
    
    
    ## 4. Appropriately label the data set with descriptive variable names.
    
    names(totdata) <- features$V2
    
    ## Create index array for mean() & std() variable names
    meanstdnames <-
        c(
            grep(
                "-mean()",features$V2, fixed = T, value = T
            ),grep("-std()",features$V2, fixed = T, value = T)
        )
    
    ## Clean up names, remove ()
    meanstdnames <- gsub("()","",meanstdnames, fixed = T)
    
    ## Label with descriptive variable names
    names(meanstddata) <- meanstdnames
    
    
    ## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    
    ## Create test subject list
    testsubject <- read.table("UCI HAR Dataset/test/subject_test.txt")
    
    ## Create train subject list
    trainsubject <-
        read.table("UCI HAR Dataset/train/subject_train.txt")
    
    ## Merge subject, activity, mean/std measurements
    testsa <- cbind(testsubject,testactivities)
    trainsa <- cbind(trainsubject,trainactivities)
    
    names(testsa) <- c("subject","activity")
    names(trainsa) <- c("subject","activity")
    
    subjectactivitymeanstd <- cbind(rbind(testsa,trainsa),meanstddata)
    
    ## Average mean/std measurements for subject / activity combination
    avgmeanstdforsubjectactivity <-
        summarise_each(group_by(subjectactivitymeanstd,subject,activity),funs(mean))
    
    ## Output Average mean/std measurements for subject / activity combination to csv file
    write.csv(avgmeanstdforsubjectactivity,"output.csv")
    
    avgmeanstdforsubjectactivity
}
