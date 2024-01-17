      #Preparation work

        library(dplyr)

        filename <- "getdata-projectfiles-UCI HAR Dataset.zip"

      #check if file exists
      
        if (!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")){
          fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
          download.file(fileURL, filename, method = "curl")
        }
      #check if folder exists
        
        if(!file.exists("UCI HAR Dataset"))
           unzip(filename)
        
      #read text files and assign data frame
        features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
        activities <-read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code","activity"))
        x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
        y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code" )
        subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
        x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
        y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
        subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
        
      #combine data from XY and merg to one table
        X <- rbind(x_test, x_train)
        Y <- rbind(y_test, y_train)
        subject <- rbind(subject_test, subject_train)
        mergedata <- cbind(subject, Y, X)
      
      #extract subject, code, mean and standard deviation column
        tidy <- mergedata %>% select(subject, code, contains("mean"), contains ("std"))
       
      #name the code as descriptive activity name
        tidy$code <- activities [tidydata$code, 2]
      
      #set the approprate name for variables
        names(tidy)[2] = "activity"
        names(tidy)<-gsub("Acc", "Accelerometer", names(tidy))        
        names(tidy) <-gsub("Gyro", "Gyroscope", names(tidy))
        names(tidy) <-gsub("BodyBody", "Body", names(tidy))
        names(tidy) <-gsub("^t", "Time", names(tidy))
        names(tidy) <-gsub("^f", "Frequency", names(tidy))
        names(tidy) <-gsub("tbody", "TimeBody", names(tidy))
        names(tidy) <-gsub("^f", "Frequency", names(tidy))
        names(tidy) <-gsub("angle", "Angle", names(tidy))
        names(tidy) <-gsub("tBody", "TimeBody", names(tidy))
        names(tidy) <-gsub("gravity", "Gravity", names(tidy))
        
      #mean grouped by subject and activity
        final <- tidy %>% group_by(subject, activity) %>% summarise_all(funs(mean))
      
      #Create a table for final data
        write.table(final, "FinalData.txt", row.name=FALSE)
        