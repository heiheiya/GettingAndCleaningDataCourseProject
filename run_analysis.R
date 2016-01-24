#read data from files
train_x<-read.table("./UCI HAR Dataset/train/X_train.txt")
train_y<-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

test_x<-read.table("./UCI HAR Dataset/test/X_test.txt")
test_y<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
features<-read.table("./UCI HAR Dataset/features.txt")

#combine train_x dataset and test_x dataset to a new dataset
newData_x<-rbind(train_x,test_x)

#use features to name the newData_x columns
colnames(newData_x) <- c(as.character(features[,2]))

#find mean and std,learn from slides 4-1 Editing Text Variables 
mean<-grep("mean()",colnames(newData_x),fixed=TRUE)

sd<-grep("std()",colnames(newData_x),fixed=TRUE)

mean_sd<-newData_x[,c(mean,sd)]

#combine train_y dataset and test_y dataset to a new dataset
newData_y<-rbind(train_y,test_y)

newActivity<-cbind(newData_y,mean_sd)

#name the Activity
colnames(newActivity)[1] <- "Activity"

#convert to character
activity_labels[,2]<-as.character(activity_labels[,2])

activity_len <- length(newActivity[,1])
for(i in 1:activity_len){
  newActivity[i,1]<-activity_labels[newActivity[i,1],2]
}

#combine subject dataset
newSubject<-rbind(subject_train,subject_test)

newSubAct<-cbind(newSubject,newActivity)

colnames(newSubAct)[1] <- "Subject"

tidyData <- aggregate( newSubAct[,3] ~ Subject+Activity, data = newSubAct, FUN= "mean" )

newSubAct_col <- ncol(newSubAct)
for(i in 4:newSubAct_col){
  tidyData[,i] <- aggregate( newSubAct[,i] ~ Subject+Activity, data = newSubAct, FUN= "mean" )[,3]
}

colnames(tidyData)[3:ncol(tidyData)] <- colnames(mean_sd)

write.table(tidyData, file = "tidyData.txt",row.name=FALSE)