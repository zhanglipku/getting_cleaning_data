library(dplyr)
#assuming the working directory has been set to "UCI HAR Dataset"
#loading data
features<-read.table("features.txt")
act_labels<-read.table("activity_labels.txt")
test_X<-read.table(".\\test\\X_test.txt")
test_Y<-read.table(".\\test\\Y_test.txt")
test_subject<-read.table(".\\test\\subject_test.txt")
train_X<-read.table(".\\train\\X_train.txt")
train_Y<-read.table(".\\train\\Y_train.txt")
train_subject<-read.table(".\\train\\subject_train.txt")

#rename columns
colnames(test_X)<-features[,2]
colnames(train_X)<-features[,2]
colnames(test_subject)<-"subject_ID"
colnames(train_subject)<-"subject_ID"
colnames(test_Y)<-"activity_ID"
colnames(train_Y)<-"activity_ID"

#combine in the "test" and the "train" tables, then merge these two
test<-cbind(test_subject, test_Y, test_X)
train<-cbind(train_subject,train_Y, train_X)
merged_data<-rbind(test, train)

#only mean or standard deviation, as well as subject ID and activity ID
index<-grep("std|mean",names(merged_data))
selected<-cbind(merged_data[,1:2],merged_data[,index])

#replace activity ID with descriptive activity names
n=1
while(n<=10299){
    selected[n,2]<-as.character(act_labels[selected[n,2],2])
    n<-n+1
}
selected<-rename(selected, activity=activity_ID) # column "activity_ID" is no longer a numeric vector but represents names of each activity, it should be changeed accordingly

#label variables with more descriptive names
colnames(selected)<-gsub("^f","frequency_", names(selected)) #f stands for frequency_
colnames(selected)<-gsub("^t","time_", names(selected)) #t stands for time_
colnames(selected)<-gsub("[mM]ag","_magnitude", names(selected)) #mag or Mag stands for magnitude
colnames(selected)<-gsub("-","_", names(selected)) #replace dashes with underscore so that it becomes more readable

#generate a second, independent dataset
final<-selected%>%group_by(subject_ID, activity)%>%summarize_each(funs(mean))  #grouping the dataset first by subject_

#write the new dataset to a local
write.table(final, "final.txt", row.names=FALSE)

 
