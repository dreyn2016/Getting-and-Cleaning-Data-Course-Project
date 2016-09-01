# Getting-and-Cleaning-Data-Course-Project

# create path to dataset

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# download file to local directory
download.file(fileUrl, destfile = "C:\\Users\\BLD\\Documents\\Dataset.zip")

# extract contents of .zip file
unzip <- unzip("C:\\Users\\BLD\\Documents\\Dataset.zip")

# read in files contained within UCI HAR Dataset folder: X_train.txt, X_test.txt, Y_train.txt, Y_test.txt, features.txt, subject_train.txt, subject_test.txt

train <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\train\\X_train.txt", quote = "", na.strings = "NA")

test <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\test\\X_test.txt", quote = "", na.strings = "NA")

trainLabels <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\train\\Y_train.txt")

testLabels <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\test\\Y_test.txt")

features <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\features.txt")

subjectTrain <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\train\\subject_train.txt")

subjectTest <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\test\\subject_test.txt")

# held off on reading in inertial signals files per the discussion at https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment

# create character vector of variable names based on second column of features

variableNames <- as.character(features$V2)

# set column names of test and train data frames equal to variableNames vector created above

colnames(test) <- variableNames

colnames(train) <- variableNames

# column bind testLabels (activities) data frame to test data frame

testwActivity <- cbind(testLabels, test)

# column bind subjectTest (subjects) data frame to testwActivity data frame (from previous step)

testFinal <- cbind(subjectTest, testwActivity)

# column bind trainLabels (activities) data frame to train data frame

trainwActivity <- cbind(trainLabels, train)

# column bind subjectTrain (subjects) data frame to trainwActivity data frame (from previous step)

trainFinal <- cbind(subjectTrain, trainwActivity)

# 1. Merges the training (trainFinal) and the test (testFinal) sets to create one data set

merge <- rbind(testFinal, trainFinal)

# set name of first column equal to "subject" and second column equal to "activity"

colnames(merge)[1] <- "subject"

colnames(merge)[2] <- "activity"

# ensure column names are syntactically valid and unique prior to extracting measurements on the mean and standard deviation

validColumnNames <- make.names(names=names(merge), unique=TRUE, allow_ = TRUE)

names(merge) <- validColumnNames

# load dplyr package in order to select measurements on the mean and standard deviation

library(dplyr)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement

mergeSelect <- select(merge, subject, activity, contains(".mean..."), contains(".std..."), contains(".mean.."), contains(".std.."))

# 3. Uses descriptive activity names to name the activities in the data set

mergeSelect$activity[mergeSelect$activity == 1] <- "walking"

mergeSelect$activity[mergeSelect$activity == 2] <- "walking_upstairs"

mergeSelect$activity[mergeSelect$activity == 3] <- "walking_downstairs"

mergeSelect$activity[mergeSelect$activity == 4] <- "sitting"

mergeSelect$activity[mergeSelect$activity == 5] <- "standing"

mergeSelect$activity[mergeSelect$activity == 6] <- "laying"

# after ensuring column names are valid and unique in step 44 above, invalid characters were translated to "."

# remove "." characters and set column names to lower case as advised in the "Editing Text Variables" lecture

df <- mergeSelect %>% setNames(tolower(gsub("\\.", "", names(.))))

# load Hmisc package in order to label the data set with descriptive variable names

library("Hmisc")

# 4. Appropriately labels the data set with descriptive variable names
# Note: R wouldn't allow me to label all 68 variables in one shot so had to do so iteratively (i.e. varLabels, varLabels2, varLabels3, varLabels4, varLabels5, varLabels6)

varLabels = c(tbodyaccmeanx="acceleration of the body on the x axis (time domain) of the phone (mean)", tbodyaccmeany="acceleration of the body on the y axis (time domain) of the phone (mean)", tbodyaccmeanz="acceleration of the body on the z axis (time domain) of the phone (mean)", tbodyaccstdx="acceleration of the body on the x axis (time domain) of the phone (std)", tbodyaccstdy="acceleration of the body on the y axis (time domain) of the phone (std)", tbodyaccstdz="acceleration of the body on the z axis (time domain) of the phone (std)", tgravityaccmeanx="gravitational acceleration of the body on the x axis (time domain) of the phone (mean)", tgravityaccmeany="gravitational acceleration of the body on the y axis (time domain) of the phone (mean)", tgravityaccmeanz="gravitational acceleration of the body on the z axis (time domain) of the phone (mean)", tgravityaccstdx="gravitational acceleration of the body on the x axis (time domain) of the phone (std)")

varLabels2 = c(tgravityaccstdy="gravitational acceleration of the body on the y axis (time domain) of the phone (std)", tgravityaccstdz="gravitational acceleration of the body on the z axis (time domain) of the phone (std)", tbodyaccjerkmeanx="jerk of the body on the x axis (time domain) of the phone (mean)", tbodyaccjerkmeany="jerk of the body on the y axis (time domain) of the phone (mean)", tbodyaccjerkmeanz="jerk of the body on the z axis (time domain) of the phone (mean)", tbodyaccjerkstdx="jerk of the body on the x axis (time domain) of the phone (std)", tbodyaccjerkstdy="jerk of the body on the y axis (time domain) of the phone (std)", tbodyaccjerkstdz="jerk of the body on the z axis (time domain) of the phone (std)", tbodygyromeanx="angular velocity of the body on the x axis (time domain) of the phone (mean)", tbodygyromeany="angular velocity of the body on the y axis (time domain) of the phone (mean)", tbodygyromeanz="angular velocity of the body on the z axis (time domain) of the phone (mean)")

varLabels3 = c(tbodygyrostdx="angular velocity of the body on the x axis (time domain) of the phone (std)", tbodygyrostdy="angular velocity of the body on the y axis (time domain) of the phone (std)", tbodygyrostdz="angular velocity of the body on the z axis (time domain) of the phone (std)", tbodygyrojerkmeanx="angular velocity jerk of the body on the x axis (time domain) of the phone (mean)", tbodygyrojerkmeany="angular velocity jerk of the body on the y axis (time domain) of the phone (mean)", tbodygyrojerkmeanz="angular velocity jerk of the body on the z axis (time domain) of the phone (mean)", tbodygyrojerkstdx="angular velocity jerk of the body on the x axis (time domain) of the phone (std)", tbodygyrojerkstdy="angular velocity jerk of the body on the y axis (time domain) of the phone (std)", tbodygyrojerkstdz="angular velocity jerk of the body on the z axis (time domain) of the phone (std)", tbodyaccmagmean="magnitude of body acceleration across signals (time domain) of the phone (mean)", tbodyaccmagstd="magnitude of body acceleration across signals (time domain) of the phone (std)")

varLabels4 = c(tgravityaccmagmean="magnitude of gravitational acceleration across signals (time domain) of the phone (mean)", tgravityaccmagstd="magnitude of gravitational acceleration across signals (time domain) of the phone (sd)", tbodyaccjerkmagmean="magnitude of jerk of the body across signals (time domain) of the phone (mean)", tbodyaccjerkmagstd="magnitude of jerk of the body across signals (time domain) of the phone (std)", tbodygyromagmean="magnitude of angular velocity across signals (time domain) of the phone (mean)", tbodygyromagstd="magnitude of angular velocity across signals (time domain) of the phone (std)", tbodygyrojerkmagmean="magnitude of angular velocity jerk of the body across signals (time domain) of the phone (mean)", tbodygyrojerkmagstd="magnitude of angular velocity jerk of the body across signals (time domain) of the phone (std)", fbodyaccmeanx="acceleration of the body on the x axis (frequency domain) of the phone (mean)")

varLabels5 = c(fbodyaccmeany="acceleration of the body on the y axis (frequency domain) of the phone (mean)", fbodyaccmeanz="acceleration of the body on the z axis (frequency domain) of the phone (mean)", fbodyaccstdx="acceleration of the body on the x axis (frequency domain) of the phone (std)", fbodyaccstdy="acceleration of the body on the y axis (frequency domain) of the phone (std)", fbodyaccstdz="acceleration of the body on the z axis (frequency domain) of the phone (std)", fbodyaccjerkmeanx="jerk of the body on the x axis (frequency domain) of the phone (mean)", fbodyaccjerkmeany="jerk of the body on the y axis (frequency domain) of the phone (mean)", fbodyaccjerkmeanz="jerk of the body on the z axis (frequency domain) of the phone (mean)", fbodyaccjerkstdx="jerk of the body on the x axis (frequency domain) of the phone (std)", fbodyaccjerkstdy="jerk of the body on the y axis (frequency domain) of the phone (std)", fbodyaccjerkstdz="jerk of the body on the z axis (frequency domain) of the phone (std)")

varLabels6 = c(fbodygyromeanx="angular velocity of the body on the x axis (frequency domain) of the phone (mean)", fbodygyromeany="angular velocity of the body on the y axis (frequency domain) of the phone (mean)", fbodygyromeanz="angular velocity of the body on the z axis (frequency domain) of the phone (mean)", fbodygyrostdx="angular velocity of the body on the x axis (frequency domain) of the phone (std)", fbodygyrostdy="angular velocity of the body on the y axis (frequency domain) of the phone (std)", fbodygyrostdz="angular velocity of the body on the z axis (frequency domain) of the phone (std)", fbodyaccmagmean="magnitude of body acceleration across signals (frequency domain) of the phone (mean)", fbodyaccmagstd="magnitude of body acceleration across signals (frequency domain) of the phone (sd)", fbodybodyaccjerkmagmean="magnitude of jerk of the body across signals (frequency domain) of the phone (mean)", fbodybodyaccjerkmagstd="magnitude of jerk of the body across signals (frequency domain) of the phone (std)", fbodybodygyromagmean="magnitude of angular velocity across signals (frequency domain) of the phone (mean)", fbodybodygyromagstd="magnitude of angular velocity across signals (frequency domain) of the phone (std)", fbodybodygyrojerkmagmean="magnitude of angular velocity jerk of the body across signals (frequency domain) of the phone (mean)", fbodybodygyrojerkmagstd="magnitude of angular velocity jerk of the body across signals (frequency domain) of the phone (std)")

# combine varLabels - varLabels6 together 

varLabels7 = c(varLabels, varLabels2, varLabels3, varLabels4, varLabels5, varLabels6)

varLabels8 = c(subject="subject", activity="activity", varLabels7)

# use a function to apply the descriptive labels above to the data set

label(df) = lapply(names(varLabels8), 
                     function(x) label(df[,x]) = varLabels8[x])

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

dfTidy <- df %>% group_by(subject, activity) %>% summarize_each(funs(mean))

# This data set is tidy for the following reasons:

# 1. Each row represents an observation (1 of 30 subjects)

# 2. Each row represents one treatment (1 of 6 activities) for each subject

# 3. Each column represents a variable (representing means and standard deviations)

# use function from step 128 above to re-apply the descriptive labels as they disappeared in prior step

label(dfTidy) = lapply(names(varLabels8), 
                              function(x) label(dfTidy[,x]) = varLabels8[x])

# write tidy data set to directory

write.table(dfTidy, file = "tidy.txt", row.name=FALSE)
