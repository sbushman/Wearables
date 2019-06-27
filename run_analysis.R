#This R Script follows directions given in the JHU Course, "Getting and Cleaning Data"

#Load Packages:
library('dplyr')
library('tidyr')
library('stringr')
library('purrr')
library('utils')

#Read the test files in
testX <- read.table('test/X_test.txt', header = FALSE)
testy <- read.table('test/y_test.txt', header = FALSE)
testsubject <- read.table('test/subject_test.txt', header = FALSE)

#Read the train files in
trainX <- read.table('train/X_train.txt', header = FALSE)
trainy <- read.table('train/y_train.txt', header = FALSE)
trainsubject <- read.table('train/subject_train.txt', header = FALSE)

#Read descriptor files in
features <- read.table('features.txt', as.is = TRUE)
activities <- read.table('activity_labels.txt')
colnames(activities) <- c("Activity", "ActDescription")
activities <- activities %>% top_n(6)
activities$Activity <- as.character(activities$Activity)
activities$Activity <- as.factor(activities$Activity)

#1  Merges the training and the test sets to create one data set.
togethertest <- cbind(testy, testsubject, testX)
togethertrain <- cbind(trainy, trainsubject, trainX)
together <- rbind(togethertest, togethertrain)
colnames(together) <- c('Activity', 'Subject', make.unique(features[,2], '_'))

#2.  Extracts only the measurements on the mean and standard deviation for each measurement.
#4.  (Out of Order, makes more sense to put it here than after #3)  Appropriately labels the data set with descriptive variable names.

selectfeatures <- features %>% 
  filter(str_detect(V2, pattern = 'mean|std')) %>%
  select(V2) %>%
  rename(features = V2)
together$Activity <- as.factor(together$Activity)

selected <- together %>% 
  select('Activity', 'Subject', selectfeatures[,1])  

#3. Uses descriptive activity names to name the activities in the data set

dataset1 <- left_join(selected, activities, by = 'Activity')
dataset1 <- dataset1 %>%
  select(ActDescription, everything()) %>% select(-Activity)


#5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

dataset2 <- dataset1 %>% group_by(ActDescription, Subject) %>% summarise_all(mean)
