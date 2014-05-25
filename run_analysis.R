#Load Packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

#Set Patn 
path <- getwd()
path

# Download the file. Put it in the data folder

# Create a data directory if it does not exist, go there
if(!file.exists("data")) dir.create("data")
setwd("data")
#setwd("c:/Getting_and_cleaning_data/data")

# Download and unzip the data file, remove the temporary zip file
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,"Dataset.zip")
unzip("Dataset.zip")
file.remove("Dataset.zip")

#List files
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive=TRUE)

#Read all the data files
#Read Subject files
SubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
SubjectTest <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

#Read Activity files
ActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
ActivityTest <- fread(file.path(pathIn, "test" , "Y_test.txt" ))

#Read Data files
fileToDataTable <- function (f) {
df <- read.table(f)
dt <- data.table(df)
}
Train <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
Test <- fileToDataTable(file.path(pathIn, "test" , "X_test.txt" ))

#1. Merge the training and the test sets to create one data set.

#Concatenate the data tables (merge the rows).
Subject <- rbind(SubjectTrain, SubjectTest)
setnames(Subject, "V1", "subject")
Activity <- rbind(ActivityTrain, ActivityTest)
setnames(Activity, "V1", "activityNum")
dt <- rbind(Train, Test)

# Merge the columns.
Subject <- cbind(Subject, Activity)
dt <- cbind(Subject, dt)

#Set key
setkey(dt, subject, activityNum)

#2. Extracts only the measurements on the mean and standard deviation for each measurement

Features <- fread(file.path(pathIn, "features.txt"))
setnames(Features, names(Features), c("featureNum", "featureName"))

#Subset only measurements for the mean and standard deviation.
Features <- Features[grepl("mean\\(\\)|std\\(\\)", featureName)]

#Convert the column numbers to a vector of variable names matching columns in dt
Features$featureCode <- Features[, paste0("V", featureNum)]

head(Features)

Features$featureCode
 
#Subset these variables using variable names.
select <- c(key(dt), Features$featureCode)
dt <- dt[, select, with=FALSE]

#3. Use descriptive activity names to name the activities in the data set

#Read activity_labels.txt file. This will be used to add descriptive names to the activities
ActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(ActivityNames, names(ActivityNames), c("activityNum", "activityName"))

#4. Appropriately label the data set with descriptive activity names

#Merge activity labels.
dt <- merge(dt, ActivityNames, by="activityNum", all.x=TRUE)

#Add activityName as a key.
setkey(dt, subject, activityNum, activityName)

#Melt the data table to reshape it from a short and wide format to a tall and narrow format
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
#Merge activity name.
dt <- merge(dt, Features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

#Create a new variable, activity that is equivalent to activityName as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class.
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#Seperate features from featureName using the helper function grepthis.
grepthis <- function (regex) {
   grepl(regex, dt$feature)
 }
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables.
R1 <- nrow(dt[, .N, by=c("feature")])
R2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
R1 == R2

#Create a tidy data set
#Create a data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
Tidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

#Dataset structure, List the key variables in the data table, Show a few rows of the dataset, Summary of variables
# str(Tidy)
# key(Tidy)
# Tidy
# summary(Tidy)

#List all possible combinations of features
Tidy[, .N, by=c(names(Tidy)[grep("^feat", names(Tidy))])]

#Save data table objects to a tab-delimited text file called 'HumanActivityPhones.txt'
f <- file.path(path, "HumanActivityPhones.txt")
write.table(Tidy, f, quote=FALSE, sep="\t", row.names=FALSE)