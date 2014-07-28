run_analysis <- function(directory ="./data/UCI-HAR-Dataset") {
  
  ## We use RStudio tools :
  ## Here are the steps of this program :
  ##
  ## Step 1 - Merges the training and the test sets to create one data set.
  ## Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
  ## Step 3 - Uses descriptive activity names to name the activities in the data set
  ## Step 4 - Appropriately labels the data set with descriptive variable names. 
  ## Step 5 - Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
  
  ##
  ## argument directory is an helper
  ## in this version , we assume that the user download by hand the zip file
  ## and uncompress in the directory passed in argument
  ## in this way files are under directory/train/... and
  ## directory/test...
  ## in a future version we can download the file and open it in zip format
  ##
  ##
  
  ## Step 1  Merges the training and the test sets to create one data set.
  
  tmp = paste(directory, "/train/X_train.txt", sep = "")
  
  if (file.exists(tmp)) {
    
    print("OK for train files")
    
    X_train<-read.csv(tmp, header=FALSE,sep="",encoding="UTF-8")
    print(dim(X_train))
    
    tmp = paste(directory, "/train/y_train.txt", sep = "")
    
    y_train<-read.csv(tmp,stringsAsFactors=TRUE, header=FALSE,sep="",encoding="UTF-8")
  ## concatenate the activity from y_train
  
   X_train<-cbind(X_train,y_train)
  
  tmp = paste(directory, "/train/subject_train.txt", sep = "")
  
  subject_train<-read.csv(tmp,stringsAsFactors=TRUE, header=FALSE,sep="",encoding="UTF-8")
  ## concatenate the clien from subject_test
  X_train<-cbind(X_train, subject_train)
  
  }
  else {
    
    print("train files not found !!")
  }
  
  tmp = paste(directory, "/test/X_test.txt", sep = "")
  
  if (file.exists(tmp)) {
    
    print("OK for test files")
    
    X_test<-read.csv(tmp, header=FALSE,sep="",encoding="UTF-8")
    print(dim(X_test))
    
    tmp = paste(directory, "/test/y_test.txt", sep = "")
    
    y_test<-read.csv(tmp,stringsAsFactors=TRUE, header=FALSE,sep="",encoding="UTF-8")
    ## concatenate the activity from y_test
    
    X_test<-cbind(X_test,y_test)
    
    tmp = paste(directory, "/test/subject_test.txt", sep = "")
    
    subject_test<-read.csv(tmp,stringsAsFactors=TRUE, header=FALSE,sep="",encoding="UTF-8")
    ## concatenate the clien from subject_test
    X_test<-cbind(X_test, subject_test)
    
  }
  else {
    
    print("test files not found !!")
  }
  print(dim(X_train))
  print(dim(X_test))
  merge1<-rbind(X_train,X_test)
  print(dim(merge1))
  ## At this step we have two last column with the activity and client number
  
  ## End of Step 1, Data set named merge1
  
  ## Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  ## this is a short option, we are not sure of the result
  ## too long to check one by one !
  tmp = paste(directory, "/features.txt", sep = "")
  
  features<-read.csv(tmp, stringsAsFactor=FALSE, header=FALSE,sep="",encoding="UTF-8")
  
  ## part of Step4 : names of the columns
  names(merge1)<-features$V2
  colnames(merge1)[563]<-"client_nb"
  colnames(merge1)[562]<-"activity"
  
  ## select mean and std : 79 values
  new_features<-grep("*-mean|-std*", features[,2])
  ## less values with this 66
  selected <- grep("mean\\(\\)|std\\(\\)", features$V2, value = TRUE)
  print(new_features)
  print(selected)
  
  features<-features[new_features,]
  .
  ## got a warning ... value activity KO
  print(features)
  
  ##
  ## this syntax gives also the order of the columns
  merge2<-merge1[,c("client_nb", "activity", selected)]
  ## ok in shell KO in prog.
  dim(merge2)
  head(merge2)
  
  
  
  ## Step 3 - - Uses descriptive activity names to name the activities in the data set
  
  activity_labels<-read.csv("./data/UCI-HAR-Dataset/activity_labels.txt",stringsAsFactors=TRUE, header=FALSE,sep="",encoding="UTF-8")
  
  merge2$activity<-factor(merge2$activity,levels = activity_labels$V1,labels = activity_labels$V2)
  
  ## Step 4  - done above
  
  ## Step 5 - several solutions ... tapply, ddply
  library(plyr)
  tidy_ds <- ddply(merge2, c("client_nb", "activity"), function(x) colMeans(x[selected]))
  
 
  ## Write the tidy data set to a file
  ## to be done : write in another directory
  write.table(tidy_ds, "tidy_data_set.txt", row.names = FALSE)
  
}
