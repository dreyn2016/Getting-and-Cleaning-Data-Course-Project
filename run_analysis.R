# create path to dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# download file to local directory
download.file(fileUrl, destfile = "C:\\Users\\BLD\\Documents\\Dataset.zip")

# extract contents of .zip file
unzip <- unzip("C:\\Users\\BLD\\Documents\\Dataset.zip")

# read in files contained within UCI HAR Dataset folder: X_train.txt, X_test.txt, Y_train.txt, Y_test.txt, features.txt, subject_train.txt, subject_test.txt
# held off on reading in inertial signals files per the discussion at https://thoughtfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-the-assignment,
train <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\train\\X_train.txt", quote = "", na.strings = "NA")
test <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\test\\X_test.txt", quote = "", na.strings = "NA")
trainLabels <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\train\\Y_train.txt")
testLabels <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\test\\Y_test.txt")
features <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\features.txt")
variableNames <- as.character(features$V2)
subjectTrain <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\train\\subject_train.txt")
subjectTest <- read.table("C:\\Users\\BLD\\Documents\\UCI HAR Dataset\\test\\subject_test.txt")

# setting column names of test and train data frames equal to variableNames vector
colnames(test) <- variableNames
colnames(train) <- variableNames

# column binding testLabels (activities) data frame to test data frame
testwActivity <- cbind(testLabels, test)

# column binding subjectTest (subjects) data frame to testwActivity data frame (from previous step)
testFinal <- cbind(subjectTest, testwActivity)

# column binding trainLabels (activities) data frame to train data frame
trainwActivity <- cbind(trainLabels, train)

# column binding subjectTrain (subjects) data frame to trainwActivity data frame (from previous step)
trainFinal <- cbind(subjectTrain, trainwActivity)

# merging the training (trainFinal) and test (testFinal) sets to create one data set (merge)
merge <- rbind(testFinal, trainFinal)

# setting name of first column equal to "subject" and second column equal to "activity"
colnames(merge)[1] <- "subject"
colnames(merge)[2] <- "activity"

# ensuring column names are syntactically valid and unique prior to extracting measurements on the mean and standard deviation
validColumnNames <- make.names(names=names(merge), unique=TRUE, allow_ = TRUE)
names(merge) <- validColumnNames

# loading dplyr package in order to select measurements on the mean and standard deviation
library(dplyr)

# selecting only the measurements on the mean and standard deviation
mergeSelect <- select(merge, subject, activity, contains(".mean..."), contains(".std..."), contains(".mean.."), contains(".std.."))

# using descriptive activity names as opposed to numerics
mergeSelect$activity[mergeSelect$activity == 1] <- "walking"
mergeSelect$activity[mergeSelect$activity == 2] <- "walking_upstairs"
mergeSelect$activity[mergeSelect$activity == 3] <- "walking_downstairs"
mergeSelect$activity[mergeSelect$activity == 4] <- "sitting"
mergeSelect$activity[mergeSelect$activity == 5] <- "standing"
mergeSelect$activity[mergeSelect$activity == 6] <- "laying"

# after ensuring column names are valid and unique in step 44 above, invalid characters were translated to "."
# removing "." characters and setting column names to lower case as advised in the "Editing Text Variables" lecture
df <- mergeSelect %>% setNames(tolower(gsub("\\.", "", names(.))))

# loading Hmisc package in order to label the data set with descriptive variable names
library("Hmisc")

# assigning descriptive labels to each of the 68 variables in the data set
varLabels = c(tbodyaccmeanx="acceleration of the body on the x axis (time domain) of the phone (mean)", tbodyaccmeany="acceleration of the body on the y axis (time domain) of the phone (mean)", tbodyaccmeanz="acceleration of the body on the z axis (time domain) of the phone (mean)", tbodyaccstdx="acceleration of the body on the x axis (time domain) of the phone (std)", tbodyaccstdy="acceleration of the body on the y axis (time domain) of the phone (std)", tbodyaccstdz="acceleration of the body on the z axis (time domain) of the phone (std)", tgravityaccmeanx="gravitational acceleration of the body on the x axis (time domain) of the phone (mean)", tgravityaccmeany="gravitational acceleration of the body on the y axis (time domain) of the phone (mean)", tgravityaccmeanz="gravitational acceleration of the body on the z axis (time domain) of the phone (mean)", tgravityaccstdx="gravitational acceleration of the body on the x axis (time domain) of the phone (std)")
varLabels2 = c(tgravityaccstdy="gravitational acceleration of the body on the y axis (time domain) of the phone (std)", tgravityaccstdz="gravitational acceleration of the body on the z axis (time domain) of the phone (std)", tbodyaccjerkmeanx="jerk of the body on the x axis (time domain) of the phone (mean)", tbodyaccjerkmeany="jerk of the body on the y axis (time domain) of the phone (mean)", tbodyaccjerkmeanz="jerk of the body on the z axis (time domain) of the phone (mean)", tbodyaccjerkstdx="jerk of the body on the x axis (time domain) of the phone (std)", tbodyaccjerkstdy="jerk of the body on the y axis (time domain) of the phone (std)", tbodyaccjerkstdz="jerk of the body on the z axis (time domain) of the phone (std)", tbodygyromeanx="angular velocity of the body on the x axis (time domain) of the phone (mean)", tbodygyromeany="angular velocity of the body on the y axis (time domain) of the phone (mean)", tbodygyromeanz="angular velocity of the body on the z axis (time domain) of the phone (mean)")
varLabels3 = c(tbodygyrostdx="angular velocity of the body on the x axis (time domain) of the phone (std)", tbodygyrostdy="angular velocity of the body on the y axis (time domain) of the phone (std)", tbodygyrostdz="angular velocity of the body on the z axis (time domain) of the phone (std)", tbodygyrojerkmeanx="angular velocity jerk of the body on the x axis (time domain) of the phone (mean)", tbodygyrojerkmeany="angular velocity jerk of the body on the y axis (time domain) of the phone (mean)", tbodygyrojerkmeanz="angular velocity jerk of the body on the z axis (time domain) of the phone (mean)", tbodygyrojerkstdx="angular velocity jerk of the body on the x axis (time domain) of the phone (std)", tbodygyrojerkstdy="angular velocity jerk of the body on the y axis (time domain) of the phone (std)", tbodygyrojerkstdz="angular velocity jerk of the body on the z axis (time domain) of the phone (std)", tbodyaccmagmean="magnitude of body acceleration across signals (time domain) of the phone (mean)", tbodyaccmagstd="magnitude of body acceleration across signals (time domain) of the phone (std)")
varLabels4 = c(tgravityaccmagmean="magnitude of gravitational acceleration across signals (time domain) of the phone (mean)", tgravityaccmagstd="magnitude of gravitational acceleration across signals (time domain) of the phone (sd)", tbodyaccjerkmagmean="magnitude of jerk of the body across signals (time domain) of the phone (mean)", tbodyaccjerkmagstd="magnitude of jerk of the body across signals (time domain) of the phone (std)", tbodygyromagmean="magnitude of angular velocity across signals (time domain) of the phone (mean)", tbodygyromagstd="magnitude of angular velocity across signals (time domain) of the phone (std)", tbodygyrojerkmagmean="magnitude of angular velocity jerk of the body across signals (time domain) of the phone (mean)", tbodygyrojerkmagstd="magnitude of angular velocity jerk of the body across signals (time domain) of the phone (std)", fbodyaccmeanx="acceleration of the body on the x axis (frequency domain) of the phone (mean)")
varLabels5 = c(fbodyaccmeany="acceleration of the body on the y axis (frequency domain) of the phone (mean)", fbodyaccmeanz="acceleration of the body on the z axis (frequency domain) of the phone (mean)", fbodyaccstdx="acceleration of the body on the x axis (frequency domain) of the phone (std)", fbodyaccstdy="acceleration of the body on the y axis (frequency domain) of the phone (std)", fbodyaccstdz="acceleration of the body on the z axis (frequency domain) of the phone (std)", fbodyaccjerkmeanx="jerk of the body on the x axis (frequency domain) of the phone (mean)", fbodyaccjerkmeany="jerk of the body on the y axis (frequency domain) of the phone (mean)", fbodyaccjerkmeanz="jerk of the body on the z axis (frequency domain) of the phone (mean)", fbodyaccjerkstdx="jerk of the body on the x axis (frequency domain) of the phone (std)", fbodyaccjerkstdy="jerk of the body on the y axis (frequency domain) of the phone (std)", fbodyaccjerkstdz="jerk of the body on the z axis (frequency domain) of the phone (std)")
varLabels6 = c(fbodygyromeanx="angular velocity of the body on the x axis (frequency domain) of the phone (mean)", fbodygyromeany="angular velocity of the body on the y axis (frequency domain) of the phone (mean)", fbodygyromeanz="angular velocity of the body on the z axis (frequency domain) of the phone (mean)", fbodygyrostdx="angular velocity of the body on the x axis (frequency domain) of the phone (std)", fbodygyrostdy="angular velocity of the body on the y axis (frequency domain) of the phone (std)", fbodygyrostdz="angular velocity of the body on the z axis (frequency domain) of the phone (std)", fbodyaccmagmean="magnitude of body acceleration across signals (frequency domain) of the phone (mean)", fbodyaccmagstd="magnitude of body acceleration across signals (frequency domain) of the phone (sd)", fbodybodyaccjerkmagmean="magnitude of jerk of the body across signals (frequency domain) of the phone (mean)", fbodybodyaccjerkmagstd="magnitude of jerk of the body across signals (frequency domain) of the phone (std)", fbodybodygyromagmean="magnitude of angular velocity across signals (frequency domain) of the phone (mean)", fbodybodygyromagstd="magnitude of angular velocity across signals (frequency domain) of the phone (std)", fbodybodygyrojerkmagmean="magnitude of angular velocity jerk of the body across signals (frequency domain) of the phone (mean)", fbodybodygyrojerkmagstd="magnitude of angular velocity jerk of the body across signals (frequency domain) of the phone (std)")
varLabels7 = c(varLabels, varLabels2, varLabels3, varLabels4, varLabels5, varLabels6)
varLabels8 = c(subject="subject", activity="activity", varLabels7)

# using a function to apply the descriptive labels above to the data set
label(df) = lapply(names(varLabels8), 
                     function(x) label(df[,x]) = varLabels8[x])

# creating a tidy data set with the average of each variable for each activity and each subject
dfTidy <- df %>% group_by(subject, activity) %>% summarize_each(funs(mean))

# using function from step 79 above to re-apply the descriptive labels as they disappeared in prior step
label(dfTidy) = lapply(names(varLabels8), 
                              function(x) label(dfTidy[,x]) = varLabels8[x])
write.table(dfTidy, file = "tidy.txt", row.name=FALSE)
