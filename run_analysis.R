################################################################################
# Assignment Description, Coursera Getting and Cleaning Data - Course Progject
################################################################################
# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for 
#    each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# The data is located at
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

################################################################################
# My approach
################################################################################
# 1. Get the Data 
# 1.0. Dowload files
# 1.1. Read into R
# 2. Explore the Data
# 2.0. Understand the structure of the data file(s)
# 2.1. check for N/A values (blanks)
# 2.2. verify unique variable names
# 2.3. review profile (specifically are there any outliers)
# 3. Shape the Data  (split - apply - combine)
# 4. Analyze the Data

################################################################################
# 1. Get the Data 
################################################################################

# 1.0. Dowload files
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destfile <- "UCI_HAR.zip"
download.file(url, destfile, method='curl')
# **unzip(destfile)** creates the folder "UCI HAR Dataset", extracts the archived files 
# and writes them to the newly created folder.
unzip(destfile)
# **list.files("UCI HAR Dataset")** shows which files have been extracted
files <- list.files("UCI HAR Dataset")
# result: 
## [1] "activity_labels.txt" "features_info.txt"   "features.txt"        "README.txt"         
## [5] "test"                "train"              
# note that "test" and "train" are folders
testFiles <- list.files("UCI HAR Dataset/test")
# testFiles includes "subject_test.txt", "X_test.txt", "y_test.txt"
# and also the folder "Inertial Signals"
testInertialSignalsFiles <- list.files("UCI HAR Dataset/test/Inertial Signals")
# testInertialSignalsFiles includes "body_acc_x_test.txt"  "body_acc_y_test.txt"  
# "body_acc_z_test.txt" "body_gyro_x_test.txt" "body_gyro_y_test.txt" 
# "body_gyro_z_test.txt" "total_acc_x_test.txt" "total_acc_y_test.txt" 
# "total_acc_z_test.txt"

# 1.1. Read into R
trn_j <- read.table("UCI HAR Dataset/train/subject_train.txt")
tst_j <- read.table("UCI HAR Dataset/test/subject_test.txt")
trn <- read.table("UCI HAR Dataset/train/X_train.txt")
tst <- read.table("UCI HAR Dataset/test/X_test.txt")
trn_b <- read.table("UCI HAR Dataset/train/y_train.txt")
tst_b <- read.table("UCI HAR Dataset/test/y_test.txt")
act_b <- read.table("UCI HAR Dataset/activity_labels.txt")
feat <- read.table("UCI HAR Dataset/features.txt")


################################################################################
# 2. Explore the Data
################################################################################

# 2.0. Understand the structure of the data file(s)
# trn[] and tst[] should have similar structures
if(length(trn_j) == length(tst_j))  exp_length_j = 1 
if(length(trn) == length(tst))  exp_length = 1 
if(length(trn_b) == length(tst_b))  exp_length_b = 1 
if(!exp_length_j || !exp_length || !exp_length_b) warning("Input files are not similar.")

# 2.1. check for N/A values (blanks)
if(length(trn_j[is.na(trn_j[ , 1])]) != 0) warning("The file subject_train.txt included blanks")
if(length(tst_j[is.na(tst_j[ , 1])]) != 0) warning("The file subject_test.txt included blanks")
if(length(trn[is.na(trn[ , 1])]) != 0) warning("The file X_train.txt included blanks")
if(length(tst[is.na(tst[ , 1])]) != 0) warning("The file X_train.txt included blanks")
if(length(trn_b[is.na(trn_b[ , 1])]) != 0) warning("The file y_train.txt included blanks")
if(length(tst_b[is.na(tst_b[ , 1])]) != 0) warning("The file y_train.txt included blanks")

# 2.2. verify unique variable names
trn_names <- names(trn)
if(length(trn_names) > length(unique(trn_names))) warning("There are duplicate variable names")
tst_names <- names(tst)
if(length(tst_names) > length(unique(tst_names))) warning("There are duplicate variable names")
compare_names <- trn_names != tst_names
if(length(compare_names[FALSE]) != 0) warning("The column names are not the same in the data sets")
if(length(act_b[,2]) > length(unique(act_b[,2]))) 
  {
    activities <- as.data.frame(paste(act_b[,1], act_b[,2], sep='_'))
  } else { 
    activities <- as.data.frame(act_b[,2])
  }
if(length(feat[,2]) > length(unique(feat[,2]))) 
  {
    var_names <- as.data.frame(paste(feat[,2], feat[,1], sep='_'))
  } else { 
    var_names <- as.data.frame(feat[,2])
  }
names(var_names) <- c("Variable" )

# 2.3. review profile (specifically are there any outliers)
outliers <- function(x) {
  has_outliers = 0
  qnt_x <- quantile(x)
  iqr_x <- IQR(x)
  lower <- qnt_x["25%"] - (iqr_x*1.5)
  upper <- qnt_x["75%"] + (iqr_x*1.5)
  if((qnt_x["0%"] < lower) || (qnt_x["100%"] > upper )) has_outliers = 1
  has_outliers
}
# **outlier_list** will be a useful tool later if there are issues with the analysis
outlier_list <- apply(trn, 2, outliers)


################################################################################
# 3. Shape the Data  (split - apply - combine)
################################################################################

# Combine datasets
trn_tst_j <- rbind(trn_j, tst_j)
  names(trn_tst_j) <- c("Subject")
trn_tst_b <-rbind(trn_b, tst_b)
  names(trn_tst_b) <- c("Activity")
trn_tst <- rbind(trn, tst)
  names(trn_tst) <- var_names$Variable

# Extract only the measurements on the mean and standard deviation
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
trn_tst_mean <- select(trn_tst, contains("[Mm]ean|str"))
trn_tst_sdev <- select(trn_tst, contains("[Ss][Tt][Dd]|str"))
## create a single data frame with these two
trn_tst_msr <- cbind(trn_tst_mean, trn_tst_sdev)
## capture names for use later.
msr_names <- names(trn_tst_msr)
# Create One Dataset
trn_tst_all <-cbind(trn_tst_j, trn_tst_b, trn_tst_msr)

# Free Memory
rm(trn_j, tst_j, trn, tst, trn_b, tst_b, act_b, feat)
rm(trn_tst_j, trn_tst_b, trn_tst_mean, trn_tst_sdev)


################################################################################
# 4. Analyze the Data
################################################################################
getMeans <- function(Subject, Activity) {
  is_relevant <- trn_tst_all[,Subject==Subject & Activity==Activity]
  output <- as.data.frame(apply(trn_tst_all[is_relevant], 2, mean))
  output[1,] <- Subject
  if(Activity==1) output[2,] <- 'WALKING'
  if(Activity==2) output[2,] <- 'WALKING_UPSTAIRS'
  if(Activity==3) output[2,] <- 'WALKING_DOWNSTAIRS'
  if(Activity==4) output[2,] <- 'SITTING'
  if(Activity==5) output[2,] <- 'STANDING'
  if(Activity==6) output[2,] <- 'LAYING'  
  names(output) <- c(paste('Subject:', Subject, '_Activity:', output[2,], sep=''))
  output
}
rs_names <- c('Subject', 'Activity', msr_names)
resultSet <- data.frame(row.names=rs_names)
# for(Subject in 1:30) {
#   for(Activity in 1:6) {
#      resultSet <- cbind(resultSet, getMeans(Subject, Activity))
#   }
# }
# write.csv(resultSet, 'resultSet.csv')
