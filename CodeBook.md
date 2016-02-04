# Code Book for Getting and Cleaning Data Course Assignment

This document describes the code inside `run_analysis.R`.


## Objective
This R script downloads UCI Human Activity Recognition (UCI HAR) data. It processes this data and finally output Subject+Activity-wise average of mean and standard deviation of all measurements.


## Steps
* Download and Extract UCI HAR Data Sets
* Merge Test and Training Data Sets to create one Data Set
* Create a new data set comprising only mean and standard deviations of each measurement
* Convert activity code for each data row to activity description
* Appropriately label each variable in the means and standard deviations data set
* From this means and standard deviations data set, create a new data set that will have average of each variable for the Subject/Activity combination
* Outout this final data set to csv

### Merge Data
The training and test data has exactly same variables (561). Therefore, rbind is used to create the merged dataset.

### New data set with only mean and standard deviation variables
features.txt contains the names of the 561 variables. It is observed that mean values have text "mean()" in the variable name and standard deviation variables have "std()". This fact is used to create an index vector of these variables using grep. Using this index vector, new data set (meanstddata) is created from the total data set for only mean and standard deviation variables.

### Mapping Activity Names to Activity Data
activity_labels.txt contains mapping of the 6 activity codes (1:6) to activity names.

Actual activity associated with each measurement is stored in y_test.txt and y_train.txt for test and training data set resp.

All these 3 files are used to create list of activity names corresponding to each measurement data row.

### Label data set with appropriate feature description
The variable names are available in features.txt. As before, "mean()" and "std()" pattern is used to extract appropriate variable names and label the data set.

### Create final output, Subject+Activity wise averages
First complete dataset is created by combining subject, activity and mean/standrd deviations data sets.

Summarise_each function of dplyr package is used to take subject+activity-wise mean of all variables and store them in an a new data set.

This dataset is returned to the function and also output as csv file


## List of Variables
The final output will have following variables:

 1 subject              
 2 activity                  
 3 tBodyAcc-mean-X           
 4 tBodyAcc-mean-Y          
 5 tBodyAcc-mean-Z          
 6 tGravityAcc-mean-X      
 7 tGravityAcc-mean-Y       
 8 tGravityAcc-mean-Z       
 9 tBodyAccJerk-mean-X      
10 tBodyAccJerk-mean-Y      
11 tBodyAccJerk-mean-Z      
12 tBodyGyro-mean-X         
13 tBodyGyro-mean-Y         
14 tBodyGyro-mean-Z         
15 tBodyGyroJerk-mean-X     
16 tBodyGyroJerk-mean-Y     
17 tBodyGyroJerk-mean-Z     
18 tBodyAccMag-mean         
19 tGravityAccMag-mean      
20 tBodyAccJerkMag-mean     
21 tBodyGyroMag-mean        
22 tBodyGyroJerkMag-mean    
23 fBodyAcc-mean-X          
24 fBodyAcc-mean-Y          
25 fBodyAcc-mean-Z          
26 fBodyAccJerk-mean-X      
27 fBodyAccJerk-mean-Y      
28 fBodyAccJerk-mean-Z      
29 fBodyGyro-mean-X         
30 fBodyGyro-mean-Y         
31 fBodyGyro-mean-Z         
32 fBodyAccMag-mean         
33 fBodyBodyAccJerkMag-mean 
34 fBodyBodyGyroMag-mean    
35 fBodyBodyGyroJerkMag-mean
36 tBodyAcc-std-X           
37 tBodyAcc-std-Y          
38 tBodyAcc-std-Z           
39 tGravityAcc-std-X       
40 tGravityAcc-std-Y        
41 tGravityAcc-std-Z       
42 tBodyAccJerk-std-X       
43 tBodyAccJerk-std-Y       
44 tBodyAccJerk-std-Z       
45 tBodyGyro-std-X          
46 tBodyGyro-std-Y          
47 tBodyGyro-std-Z          
48 tBodyGyroJerk-std-X      
49 tBodyGyroJerk-std-Y      
50 tBodyGyroJerk-std-Z     
51 tBodyAccMag-std          
52 tGravityAccMag-std       
53 tBodyAccJerkMag-std      
54 tBodyGyroMag-std         
55 tBodyGyroJerkMag-std     
56 fBodyAcc-std-X           
57 fBodyAcc-std-Y           
58 fBodyAcc-std-Z           
59 fBodyAccJerk-std-X       
60 fBodyAccJerk-std-Y       
61 fBodyAccJerk-std-Z       
62 fBodyGyro-std-X          
63 fBodyGyro-std-Y          
64 fBodyGyro-std-Z          
65 fBodyAccMag-std          
66 fBodyBodyAccJerkMag-std  
67 fBodyBodyGyroMag-std     
68 fBodyBodyGyroJerkMag-std 
