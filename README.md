# Getting-and-Cleaning-Data-Project

1. Reading of Data
Data are read from the files:

Data: X_test and X_train (data) Activity Data: y_test and y_train (activity_data) Subject Data: subject_test and subject_train (subject_data) Labels: labels Features: features

2. Extract of Mean and Standard Deviation Data
Vector of mean and sd created from column 2 of features using grep (msd) data reduced to only mean and sd by application of msd

3 and 4.
data names cleaned using gsub Subject and Activity labels added to subject_data and activity_data

5. Grouping and Tidy Data
Use of melt from reshape2 to group data and add activity labels Write tidied data to "tidied_averag_data.txt"

A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
