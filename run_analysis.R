setwd("./91 Tutorial/JH Coursera 03/WK04/UCI HAR Dataset/Getting-and-Cleaning-Data-Course-Project")
library(dplyr)
library(tidyr)
library(readr)
## read label
activitylabel <- read.table("../activity_labels.txt")
featureslabel <- read.table("../features.txt") %>% filter(grepl("mean\\(\\)|std\\(\\)", V2))
trainlabel <- read.table("../train/y_train.txt")
testlabel <- read.table("../test/y_test.txt")
## read subject
trainsubject <- read.table("../train/subject_train.txt")
testsubject <- read.table("../test/subject_test.txt")
## read data, add y label (acitivity), subject, row number and set (train or test) info
train <- read.table("../train/X_train.txt")
train <- train %>% mutate(activitynum = trainlabel$V1, subject = trainsubject$V1, row = as.numeric(rownames(train)), set ="train") 
test <- read.table("../test/X_test.txt") 
test <- test %>% mutate(activitynum = testlabel$V1, subject = testsubject$V1, row = as.numeric(rownames(test)), set ="test") 

## change features label to descriptive one
featureslabel$V2 <- sub("^t", "Time ", featureslabel$V2)
featureslabel$V2 <- sub("^f", "Frequecy ", featureslabel$V2)
featureslabel1 <- filter(featureslabel, grepl("mean()", V2))
featureslabel1$V2 <- paste("Mean of", featureslabel1$V2)
featureslabel2 <- filter(featureslabel, grepl("std()", V2))
featureslabel2$V2 <- paste("Standard Deviation of", featureslabel2$V2)
featureslabel <- rbind(featureslabel1, featureslabel2)
featureslabel$V2 <- gsub("Body", "Body ", featureslabel$V2)
featureslabel$V2 <- sub("Gravity", "Gravity ", featureslabel$V2)
featureslabel$V2 <- sub("Acc", "Acceleration ", featureslabel$V2)
featureslabel$V2 <- sub("Gyro", "Angular velocity ", featureslabel$V2)
featureslabel$V2 <- sub("Jerk", "Jerk ", featureslabel$V2)
featureslabel$V2 <- sub("Mag", "Magnitude ", featureslabel$V2)
featureslabel$V2 <- sub("-X", "X axis", featureslabel$V2)
featureslabel$V2 <- sub("-Y", "Y axis", featureslabel$V2)
featureslabel$V2 <- sub("-Z", "Z axis", featureslabel$V2)
featureslabel$V2 <- sub("\\-mean\\(\\)", "", featureslabel$V2)
featureslabel$V2 <- sub("\\-std\\(\\)", "", featureslabel$V2)


## Join train and test data, merge activity label to data
phonedata <- bind_rows(train, test) %>% merge(activitylabel, by.x = "activitynum", by.y = "V1") %>% select(-activitynum) %>% 
          ##transpose data and split variable name of measure
          gather(measure, read, V1:V561) %>% mutate(measure = parse_number(measure)) %>% 
          ##merge featureslabel
          merge(featureslabel, by.x = "measure", by.y = "V1") %>%
          select(-measure, activity = V2.y) %>%
          ##Transpose measure to columns as variables
          spread(V2, read) %>% select(-row)
## To create separate with the average of each variable for each activity and each subject.
phonedata_summary <- phonedata %>% group_by(subject, activity) %>% 
        summarise_at(vars("Mean of Frequecy Body Acceleration Jerk X axis": "Standard Deviation of Time Gravity Acceleration Z axis"), mean)
## Write data set to upload
write_csv(phonedata, "../phonedata.csv")
write_csv(phonedata_summary, "../phonedata summary.csv")