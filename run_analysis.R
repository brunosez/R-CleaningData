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
    
  }
  else {
    
    print("test files not found !!")
  }
  
  merge1<-rbind(X_train,X_test)
  print(dim(merge1))
  ## At this step we have last column with the activity
  
  ## End of Step 1, Data set named merge1
  
  ## Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
  
  ## this is a short option, we are not sure of the result
  ## too long to check one by one !
  tmp = paste(directory, "/features.txt", sep = "")
  
  features<-read.csv(tmp, header=FALSE,sep="",encoding="UTF-8")
  
  new_features<-grep("*-mean|-std*", features[,2])
  print(new_features)
  features<-features[new_features,]
  features<-rbind(features,c("562","activity"))
  ## got a warning ... value activity KO
  print(features)
  merge2<-merge1[,features[,1]]
  ## ok in shell KO in prog.
  dim(merge2)
  head(merge2)
  

  
  ## Step 3
  
  activity_labels<-read.csv("./data/UCI-HAR-Dataset/activity_labels.txt",stringsAsFactors=TRUE, header=FALSE,sep="",encoding="UTF-8")
  
  merge2$activity<-factor(merge2$activity,levels = activity_labels$V1,labels = activity_labels$V2)
  
  
  
  ## write in a merged directory
  
  ## write.table
  
  
}
