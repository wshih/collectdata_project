#Course Project
#This script combines the test and train datasets, and merges subject IDs and activities labels.
#Only the mean and standard deviation measurements are kept, and these values are
#averaged for each individual activity and per subject  

#read variable measurement names and collect vector of indices for all column names containing 
#"mean" (but not "meanFreq") and containing "std"
features<-read.table("features.txt")
mean_features<-setdiff(grep("mean", features[,2]), grep("meanFreq", features[,2]))
select_features<-sort(c(mean_features, grep("std", features[,2]))) #vector of indices
#valid_features <- make.names(names=features[,2], unique=TRUE, allow_ = TRUE) #forcing unique column names with valid characters


#get training data
setwd("./train")
xtrain<-read.table("X_train.txt")
train_activitiesid<- read.table("y_train.txt")
train_subjectid<-read.table("subject_train.txt")

#get test data
setwd("../test")
xtest<-read.table("X_test.txt")
test_activitiesid<-read.table("y_test.txt")
test_subjectid<-read.table("subject_test.txt")


#merge test and training data
merged<-rbind(xtrain, xtest)
merge_subjectid<-rbind(train_subjectid, test_subjectid)
merge_activitiesid<-rbind(train_activitiesid, test_activitiesid)

#label activities with descriptive activities names in second column
for(i in 1:nrow(merge_activitiesid)){
  if(merge_activitiesid[i,1]==1) merge_activitiesid[i,2]<- "WALKING" 
  if(merge_activitiesid[i,1]==2) merge_activitiesid[i,2]<- "WALKING_UPSTAIRS" 
  if(merge_activitiesid[i,1]==3) merge_activitiesid[i,2]<- "WALKING_DOWNSTAIRS" 
  if(merge_activitiesid[i,1]==4) merge_activitiesid[i,2]<- "SITTING" 
  if(merge_activitiesid[i,1]==5) merge_activitiesid[i,2]<- "STANDING" 
  if(merge_activitiesid[i,1]==6) merge_activitiesid[i,2]<- "LAYING" 
}


#Extracts only the selected variable measurements and variable names 
#containing the mean and standard deviation for each measurement
filtered<-select(merged, select_features)
select_names<-slice(features, select_features) #filter only the selected feature names

#label the data set with different, descriptive variable names 
select_names[,3]<-sub("\\(\\)", "", select_names[,2]) #remove parenthesis from names
select_names[,3]<-sub("BodyBody", "Body", select_names[,3]) #remove duplicate "Body" in labels 
select_names[,3]<-sub("Acc", "Accelerometer", select_names[,3])
select_names[,3]<-sub("Mag", "Magnitude", select_names[,3])
select_names[,3]<-sub("Gyro", "Gyroscope", select_names[,3])
colnames(filtered)<-select_names[,3]

#create new dataset with subject ids and activity ids
data1<- cbind(merge_activitiesid[,2], filtered)
newdata<-cbind(merge_subjectid, data1)
colnames(newdata)[1]<-"Subject_ID"
colnames(newdata)[2]<-"Activity"

#calculate mean for each variable for each subject and activity
data_groups<-newdata %>% 
  group_by(Subject_ID, Activity) %>%
  summarise_each(funs(mean))

write.table(data_groups, file="Activity_summary.txt", row.names=FALSE)
