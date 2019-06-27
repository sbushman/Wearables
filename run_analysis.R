#This R Script follows directions given in the JHU Course, "Getting and Cleaning Data"

#Load Packages:
library('dplyr')
library('tidyr')
library('stringr')
library('purrr')
library('utils')

#buncha variables, some of which I used in the deliverable, some not

length <- c(2947, 7352)
folders <- c('test', 'train')
files <- c('subject_', 'X_', 'y_')
subfiles <- c('body_acc_x_test.txt',
              'body_acc_y_test.txt',
              'body_acc_z_test.txt',
              'body_gyro_x_test.txt',
              'body_gyro_y_test.txt',
              'body_gyro_z_test.txt',
              'total_acc_x_test.txt',
              'total_acc_y_test.txt',
              'total_acc_z_test.txt'
              )
rpath <- "Wearables/UCI_HAR_Dataset/"

practicetest <- read.table('test/X_test.txt', header = FALSE)


#1.  Read files in from data set

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
colnames(activities) <- c("activityId", "activityLabel")

#2.  Merge them, first by columns, then by rows
togethertest <- cbind(testy, testsubject, testX)
togethertrain <- cbind(trainy, trainsubject, trainX)
together <- rbind(togethertest, togethertrain)
colnames(together) <- c('Activity', 'Subject', make.unique(features[,2], '_'))

#3.  Extract the set of mean and std features described in the 'features' dataset, then keep only the appropriate columns
selectfeatures <- features %>% 
  filter(str_detect(V2, pattern = 'mean|std')) %>%
  select(V2) %>%
  rename(features = V2)
filtered <- together %>% select('Activity', 'Subject', selectfeatures[,1])



#Coerce to character
together$V1 <- as.character(together$V1)
#Turn it into one big character vector
together <- unlist(together)
#Break the giant elements of the vector into lots of individual character vectors, at spaces.
split <- lapply(together, strsplit, split = ' ')
#unlist level two to vectors with lots of entries
unlisted <- lapply(split, unlist)
#make them numeric vectors
numero <- lapply(unlisted, as.numeric)
#Get rid of NAs, miraculously after this there are 561 entries on each row!
clean <- lapply(numero, na.exclude)
#coerce to dataframe so I can use tidyverse
fixed <- as.data.frame(clean)


####This is the path to the first dataset
#Calcuate mean and sd of each row (note I am still row-wise at this point)
calculated <- fixed %>% mutate(mean = mean(1:10299), sd = sd(1:10299)) %>% select(mean, sd)
#I think I will leave this on "narrow" rather than transposing to be wide--but I could change it 
#if I wanted to

#need to bring in and clean the features dataset
features <- read.csv(paste0(rpath, 'features.txt'))
features <- features %>% rename(features = X1.tBodyAcc.mean...X)
features$features <- as.character(features$features)

#Some values are truncated and spill over to another line.  This step omits most of the lopped off values
features <- features %>% filter(str_length(features) > 2)

#Fixing final entries in features
featurefix <- unlist(features)[554:567]
featurefix <- as.data.frame(matrix(featurefix,ncol = 2,byrow = T))
featurefix <- featurefix %>% mutate(features = paste0(V1, V2)) %>% select(features)

#Removing these last entries from features so I can replace them with the fixed values
features <- features %>% top_n(553)

#ALSO, the first entry is missing, so I'll create a new df with one entry

filler <- data.frame(features = 'unknown')

#Now, rbind a fixed features dataframe
features <- rbind(filler, features, featurefix)

#Now cbind entry 
dataset1 <- cbind(features, calculated)


####This is the path to the second dataset

#First read in complementary datasets
subjectTest <- read.csv(header = FALSE)
subjectTest <- subjectTest %>% rename(subject = V1)

subjectTrain <- read.csv(paste0(rpath, folders[2], '/', files[1], folders[2], '.txt'), header = FALSE)
subjectTrain <- subjectTrain %>% rename(subject = V1)

Y_Test <- read.csv(paste0(rpath, folders[1], '/', files[3], folders[1], '.txt'), header = FALSE)
Y_Test <- Y_Test %>% rename(Activity = V1)

Y_Train <- read.csv(paste0(rpath, folders[2], '/', files[3], folders[2], '.txt'), header = FALSE)
Y_Train <- Y_Train %>% rename(Activity = V1)


#Ideally I would build loops that accomplish these steps, but this manual approach also fulfills the assignment.  Also, in the end, it doesn't appear I need to do this, but I suspect it would be useful for other analyses.


body_acc_x_test <- read.csv('test/Inertial Signals/body_acc_x_test.txt', header = FALSE)
body_acc_x_test <- body_acc_x_test %>% rename(body_acc_x = V1)

body_acc_y_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/body_acc_y_test.txt', header = FALSE)
body_acc_y_test <- body_acc_y_test %>% rename(body_acc_y = V1)

body_acc_z_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/body_acc_z_test.txt', header = FALSE)
body_acc_z_test <- body_acc_z_test %>% rename(body_acc_z = V1)

body_gyro_x_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/body_gyro_x_test.txt', header = FALSE)
body_gyro_x_test <- body_gyro_x_test %>% rename(body_gyro_x = V1)

body_gyro_y_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/body_gyro_y_test.txt', header = FALSE)
body_gyro_y_test <- body_gyro_y_test %>% rename(body_gyro_y = V1)

body_gyro_z_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/body_gyro_z_test.txt', header = FALSE)
body_gyro_z_test <- body_gyro_z_test %>% rename(body_gyro_z = V1)

total_acc_x_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/total_acc_x_test.txt', header = FALSE)
total_acc_x_test <- total_acc_x_test %>% rename(total_acc_x = V1)

total_acc_y_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/total_acc_y_test.txt', header = FALSE)
total_acc_y_test <- total_acc_y_test %>% rename(total_acc_y = V1)

total_acc_z_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/total_acc_z_test.txt', header = FALSE)
total_acc_z_test <- total_acc_z_test %>% rename(total_acc_z = V1)

#Now bind the test data together, note I'm using only two of the datasets read in.
subject_test <- cbind(subjectTest, Y_Test)


#Now read in all the train data (Don't have to do this for this project, but might for some other analysis in future)
body_acc_x_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/body_acc_x_train.txt', header = FALSE)
body_acc_x_train <- body_acc_x_train %>% rename(body_acc_x = V1)

body_acc_y_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/body_acc_y_train.txt', header = FALSE)
body_acc_y_train <- body_acc_y_train %>% rename(body_acc_y = V1)

body_acc_z_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/body_acc_z_train.txt', header = FALSE)
body_acc_z_train <- body_acc_z_train %>% rename(body_acc_z = V1)

body_gyro_x_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/body_gyro_x_train.txt', header = FALSE)
body_gyro_x_train <- body_gyro_x_train %>% rename(body_gyro_x = V1)

body_gyro_y_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/body_gyro_y_train.txt', header = FALSE)
body_gyro_y_train <- body_gyro_y_train %>% rename(body_gyro_y = V1)

body_gyro_z_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/body_gyro_z_train.txt', header = FALSE)
body_gyro_z_train <- body_gyro_z_train %>% rename(body_gyro_z = V1)

total_acc_x_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/total_acc_x_train.txt', header = FALSE)
total_acc_x_train <- total_acc_x_train %>% rename(total_acc_x = V1)

total_acc_y_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/total_acc_y_train.txt', header = FALSE)
total_acc_y_train <- total_acc_y_train %>% rename(total_acc_y = V1)

total_acc_z_train <- read.csv('Wearables/UCI_HAR_Dataset/train/Inertial Signals/total_acc_z_train.txt', header = FALSE)
total_acc_z_train <- total_acc_z_train %>% rename(total_acc_z = V1)

#Now bind all train data, note, using only two data sets read in.
subject_train <- cbind(subjectTrain, Y_Train)

#Now combine Train and Test tables
Combined <- rbind(subject_test, subject_train)

#Now I will transpose "Fixed" described above, which appropriately reshapes the stuff from part one
datadetail <- t(fixed)
#Now change it back to a dataframe
datadetail <- as.data.frame(datadetail)

#Make features names unique
features$features <- as.character(features$features)

uniquefeatures <- make.unique(features$features, sep = "_")

colnames(datadetail) <- uniquefeatures

#Now combine the data

almost <- cbind(Combined, datadetail)

#Allright, now some dplyr magic
almost <- almost %>% group_by(subject, Activity) %>% summarize_all(mean)
almost <- as.data.frame(almost)
almost$Activity <- as.numeric(almost$Activity)

#Also, I will join descriptive activity, using the activities label dataset
labels <- read.csv(paste0(rpath, 'activity_labels.txt'), header = FALSE)
#omit the last line which seems like a mistake
labels <- labels %>% top_n(6)
#make two columns from one
labels$V1 <- as.character(labels$V1)
labels <- separate(labels, col = V1, into = c('Activity', 'Description'), sep = ' ')
labels$Activity <- as.numeric(labels$Activity)

#Now join in order to deliver descriptive Activity
dataset2 <- left_join(almost, labels, by = 'Activity')

write.csv(dataset2, 'Wearables/dataset2.txt')


