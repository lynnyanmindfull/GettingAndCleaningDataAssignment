#set working directory
setwd("F:/Lynn/Data")

#download the file and put the file in the data folder
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

#unzip the file
unzip(zipfile = "./data/Dataset.zip",exdir="./data")

#unzipped files are in the folder UCI HAR Dataset. Get the list of the files
path_rf <- file.path("./data","UCI HAR Dataset")
files <- list.files(path_rf,recursive = TRUE)
files

#read data from the Activity files into the variables
dataActivityTest <- read.table(file.path(path_rf,"test","Y_test.txt"), header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf,"train","Y_train.txt"), header = FALSE)

#read data from the Subject files into the variables
dataSubjectTest <- read.table(file.path(path_rf,"test","Subject_test.txt"), header = FALSE)
dataSubjectTrain <- read.table(file.path(path_rf,"train","Subject_train.txt"), header = FALSE)

#read data from the Features files into the variables
dataFeaturesTest <- read.table(file.path(path_rf,"test","X_test.txt"), header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf,"train","X_train.txt"), header = FALSE)

#check the properties of those above variables
str(dataActivityTest)
str(dataActivityTrain)
str(dataSubjectTrain)
str(dataSubjectTest)
str(dataFeaturesTest)
str(dataFeaturesTrain)

##1. Merges the training and the test sets to create one data set
#Concatenate the data tables by rows
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

#set names to variables
names(dataSubject)<-c("subject")
names(dataActivity)<- c("activity")
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"),head=FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

#Merge columns to get the dataframe data for all data
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)

##2. Extracts only the measurement on the mean and standard deviation for each measure
#subset name of features by measurements on the mean and standard deviation
subdataFeaturesNames<-dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

#subset the data frame Data by seleted names of Features
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
Data<-subset(Data,select=selectedNames)

#check the structures of the data frame Data
str(Data)

##3. Uses descriptive activity names to name the activities in the data set
#read descriptive activity names from "activity_lables.txt"
activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"),header = FALSE)

#facotize Variable activity in the data frame Data using descriptive names and check
Data$activity<-factor(Data$activity);
Data$activity<- factor(Data$activity,labels=as.character(activityLabels$V2))
head(Data$activity,30)

##4. Appropriately labels the data set with descriptive variable names
#In the former part, variables activity and subject and names of the activities have been labelled 
#using descriptive names.In this part, Names of Feteatures will labelled using descriptive variable
#names.

#- prefix t  is replaced by  time
#- Acc is replaced by Accelerometer
#- Gyro is replaced by Gyroscope
#- prefix f is replaced by frequency
#- Mag is replaced by Magnitude
#- BodyBody is replaced by Body

names(Data)<-gsub("^t", "time", names(Data))
names(Data)<-gsub("^f", "frequency", names(Data))
names(Data)<-gsub("Acc", "Accelerometer", names(Data))
names(Data)<-gsub("Gyro", "Gyroscope", names(Data))
names(Data)<-gsub("Mag", "Magnitude", names(Data))
names(Data)<-gsub("BodyBody", "Body", names(Data))

#check
names(Data)

##5. Creates a second,independent tidy data set and ouput it
#In this part,a tidy data set will be created with the average 
#of each variable for each activity and each subject 

library(plyr);
Data2<-aggregate(. ~subject + activity, Data, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)