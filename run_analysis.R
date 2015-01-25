#' 
#' 1. Merges the training and the test sets to create one data set.
#' 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#' 3. Uses descriptive activity names to name the activities in the data set
#' 4. Appropriately labels the data set with descriptive variable names. 
#' 5. From the data set in step 4, creates a second, independent tidy data set 
#'    with the average of each variable for each activity and each subject.

packages <- c("dplyr", "tidyr")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

DEBUG <- FALSE
DOWNLOAD <- FALSE
SAVEDIR <- getwd()
DATADIR <- file.path(SAVEDIR, "UCI HAR Dataset")
# file download
if (DOWNLOAD) {
f <- "activity.zip"
if(!file.exists(SAVEDIR)){dir.create(SAVEDIR)}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, file.path(SAVEDIR, f), method="curl")
cat(paste0("file download complete - remember to unzip file ",f))
readline(prompt="Press [enter] to continue")
}


# read in the data for test /training sets/ labels
if (!file.exists(DATADIR)) {
	cat(paste0(DATADIR," doesn't exist -- did you unzip the file ?"))
	stopifnot(file.exists(DATADIR))	
}
	
# read test
test <- read.table(file.path(DATADIR, "test", "X_test.txt") , colClasses="numeric")		
if (DEBUG) head(test)
testLabel <- read.table(file.path(DATADIR, "test", "Y_test.txt"))
if (DEBUG) head(testLabel)
testSubject <- read.table(file.path(DATADIR, "test", "subject_test.txt"))		
if (DEBUG) head(testSubject)

# read training
training <- read.table(file.path(DATADIR, "train", "X_train.txt"), colClasses="numeric") 
if (DEBUG) head(training)
trainingLabel <- read.table(file.path(DATADIR, "train", "Y_train.txt"))
if (DEBUG) head(trainingLabel)
trainingSubject <- read.table(file.path(DATADIR, "train", "subject_train.txt"))
if (DEBUG) head(trainingSubject)

activityLabel <- read.table(file.path(DATADIR, "activity_labels.txt"))
if (DEBUG) head(activityLabel)
# clean up the the labels by removing excessive brackets and underscores
activityLabel <- sub("_", " ", tolower(activityLabel$V2))

features <- read.table(file.path(DATADIR, "features.txt"))
if (DEBUG) head(features)
features <- gsub("\\()", "", features$V2)


# relabel the names of columns 
names(test) <- features; names(training) <- features
names(testLabel) <- "activity"; names(trainingLabel) <- "activity"
names(testSubject) <- "participant"; names(trainingSubject) <- "participant"

# create a DF & bind the training data to the bottom of the test data
# Source: local data frame [2,947 x 81]
DF1 <- tbl_df(bind_cols(testSubject,testLabel, test[,grep("mean|std", colnames(test))]))
# Source: local data frame [7,352 x 81]
DF2 <- tbl_df(bind_cols(trainingSubject,trainingLabel, training[,grep("mean|std", colnames(training))]))

# final merge
# Source: local data frame [10,299 x 81]
DF <-  rbind_list(DF1, DF2)
rm("DF1")
rm("DF2")

# fix the activity labels
DF <- mutate(DF, activity = factor(activity, labels = activityLabel))

# check for NAs
#colSums(is.na(DF))
if (DEBUG) all(colSums(is.na(DF))==0)

#  DF with the average of each variable for each activity & subject
TIDY <-DF %>% group_by(activity, participant) %>% summarise_each(funs(mean))

# save results
output <- "activitydata.txt"
write.table(file = "activitydata.txt", x = TIDY, row.names = FALSE)
cat(paste0(" ALL DONE, results wriiten to ", output))

str(TIDY)


