## 1. Merges the training and the test sets to create one data set.
## Read the Train files into R.
features = read.table("features.txt", header = FALSE)
activityType = read.table("activity_labels.txt", header = FALSE)
subjectTrain = read.table("./train/subject_train.txt", header = FALSE)
xTrain = read.table("./train/X_train.txt", header = FALSE)
yTrain = read.table("./train/Y_train.txt", header = FALSE)

## Set the column names
colnames(activityType) = c('activityID', 'activityType')
colnames(subjectTrain) = "subjectID"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityID"

## Combine the Train files into one dataset.
trainData = cbind(yTrain, subjectTrain, xTrain)

## Read the Test files into R.
yTest = read.table("./test/y_test.txt", header = FALSE)
subjectTest = read.table("./test/subject_test.txt", header = FALSE)
xTest = read.table("./test/X_test.txt", header = FALSE)

## Set the column names.
colnames(subjectTest) = "subjectID"
colnames(xTest) = features[,2]
colnames(yTest) = "activityID"

## Combine the Test data into one dataset.
testData = cbind(yTest, subjectTest, xTest)

## Combine the Train and Test data into one complete dataset.
finalData = rbind(trainData, testData)

## Create a vector containing all column names.
colnames = colnames(finalData)

## 2. Extract only the measurements on the mean and standard deviation for each measurement. 
## Create a logical vector identifying all variable with Standard Deviation or 
## Mean, along with the activityID and subjectID columns.
mean_sd = grepl("mean", colnames)|grepl("std", colnames)|grepl("ID", colnames)

## Reset finalData to include just the variables that return as FALSE.
finalData = finalData[mean_sd == TRUE]

## 3. Use descriptive activity names to name the activities in the data set
## Merge the activityID table.
finalData = merge(finalData,activityType,by='activityID',all.x=TRUE)

## Update the column names vector.
colnames = colnames(finalData)

## 4. Appropriately label the data set with descriptive activity names. 
## Clean up the variable names.
for (i in 1:length(colnames)) 
{
  colnames[i] = gsub("\\()","",colnames[i])
  colnames[i] = gsub("-std$","sd",colnames[i])
  colnames[i] = gsub("-mean","Mean",colnames[i])
  colnames[i] = gsub("^(t)","time",colnames[i])
  colnames[i] = gsub("^(f)","freq",colnames[i])
  colnames[i] = gsub("([Gg]ravity)","Grav",colnames[i])
  colnames[i] = gsub("([Bb]ody)","",colnames[i])
  colnames[i] = gsub("[Gg]yro","Gyro",colnames[i])
}

## From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject.
finalData.dt <- data.table(finalData)
tidy <- finalData.dt[, lapply(.SD, mean), by = 'subjectID,activityID']
write.table(tidy, file = "tidy.txt", row.names = FALSE)
