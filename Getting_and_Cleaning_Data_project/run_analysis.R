# Step 1 - Merge the training and the test sets to create one data set.

#set working directory
setwd("~/UCI HAR Dataset")

# Read in the data from files
features     = read.table('./features.txt',header=FALSE)
activityType = read.table('./activity_labels.txt',header=FALSE)
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain       = read.table('./train/x_train.txt',header=FALSE)
yTrain       = read.table('./train/y_train.txt',header=FALSE)

colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2]
colnames(yTrain)        = "activityId"

# final training - merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);

# Reading the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE)
xTest       = read.table('./test/x_test.txt',header=FALSE)
yTest       = read.table('./test/y_test.txt',header=FALSE)

# Assigning column names to the test data
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

testData = cbind(yTest,subjectTest,xTest);

finalData = rbind(trainingData,testData);

colNames  = colnames(finalData);

# Step 2 - Extract only the measurements on the mean and standard deviation for each measurement. 

logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

finalData = finalData[logicalVector==TRUE];

# Step 3 - Use descriptive activity names to name the activities in the data set

finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);

colNames  = colnames(finalData); 

# Step 4 - Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) = colNames;

# Step 5 - Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

write.table(tidyData, './tidyData2.txt',row.names=F,sep='\t')

