#This R Script follows directions given in the JHU Course, "Getting and Cleaning Data"

#Load Packages:
library('dplyr')

##readin the two datasets to be merged

#This one has long incomprehensible values, 2946 observations
X_test <- read.csv('Wearables/UCI_HAR_Dataset/test/X_test.txt')

y_test <- read.csv('Wearables/UCI_HAR_Dataset/test/y_test.txt')

body_acc_x_test <- read.csv('Wearables/UCI_HAR_Dataset/test/Inertial Signals/body_acc_x_test.txt')