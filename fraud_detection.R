library(dplyr)
library(ggplot2)
library(caret)

### Datenanalyse und erste Erkenntnisse ###

# Read data
data.full <- read.csv("Google Drive/My Drive/Files/FH Technikum/1 - Aktuell/Solution Engineering - R/Fraud.csv")
# Show first rows of data
head(data.full)
# Show isFraud distribution in percentage
table(data.full$isFraud)/nrow(data)*100
# Show isFlaggedFraud distribution in percentage
table(data.full$isFlaggedFraud)/nrow(data)*100

### Datenbereinigung ###

# Create subset of data by removing "nameOrig", "nameDest", "isFlaggedFraud columns
data <- select(data.full, -nameOrig, -nameDest, -isFlaggedFraud)
head(data)

# Check data types
str(data)
# Convert data types
data$type <- as.factor(data$type)
data$isFraud <- as.factor(data$isFraud)
#data$isFlaggedFraud <- as.factor(data$isFlaggedFraud)

# Check number of missing values
sum(is.na(data))

# -- Es sind keine fehlenden Werte vorhanden.

### Explorative Datenanalyse ###

# Show summary statistics
summary(data)

# Plot histogram for all numeric variables
numeric_columns <- colnames(select_if(data, is.numeric)) #select numeric column names

par(mfrow=c(3,2))
for (col_name in numeric_columns) {
  hist(data[[col_name]], main = col_name)
}
par(mfrow=c(1,1))

# -- Es ist zu sehen, dass eine starke rechtsschiefe Verteilung bei den numerischen Variablen (außer step) vorliegt. 

# Plot barplot for all categorical variables
categorical_columns <- colnames(select_if(data, is.factor)) #select categorical column names

par(mfrow=c(2,1))
for (col_name in categorical_columns) {
  barplot(table(data[[col_name]]), main = col_name)
}
par(mfrow=c(1,1))

# -- Es ist zu sehen, dass die Verteilung der Kategorien bei den kategorischen Variablen ziemlich ungleich ist, besonders bei isFraud

# Plot correlation matrix
correlation_matrix <- cor(data[numeric_columns])
corrplot::corrplot.mixed(correlation_matrix, order = 'AOE')

# -- Es ist zu sehen, dass eine perfekte Korrelation zwischen "oldbalanceOrg" und "newbalanceOrg" vorliegt sowie eine fast 
# -- perfekte Korrelation zwischen "oldbalanceDest" und "newbalanceDest" vorliegt.

# Remove highly correlated columns (newbalanceOrig, newbalanceDest)
data <- select(data, -newbalanceOrig, -newbalanceDest)
# Update numeric columns
numeric_columns <- colnames(select_if(data, is.numeric)) #select numeric column names


# Histograms by fraud status
par(mfrow=c(4,2))
for (col_name in numeric_columns) {
  hist(data[[col_name]][data$isFraud == 0], main = paste(col_name, "isFraud = 0"))
  hist(data[[col_name]][data$isFraud == 1], main = paste(col_name, "isFraud = 1"))
}
par(mfrow=c(1,1))

# Boxplots by fraud status
par(mfrow=c(1,4))
for (col_name in numeric_columns) {
  boxplot(data[[col_name]][data$isFraud == 1], main = paste(col_name, "isFraud = 1"))
}
par(mfrow=c(1,1))

# Barplot by type and fraud status
ggplot(data, aes(x = type, fill = isFraud)) + geom_bar(position = "dodge")


### Modellierung ###
# Split data into training and test set
# Create a column for the interaction between type and isFraud
data$type_fraud <- interaction(data$type, data$isFraud)

# Stratified allocation based on the combined column
splitIndex <- createDataPartition(data$type_fraud, p = 0.75, list = FALSE, times = 1)

trainData <- data[splitIndex, ]
testData <- data[-splitIndex, ]

# Remove the combined column
trainData$type_fraud <- NULL
testData$type_fraud <- NULL

# Check if the distribution of isFraud is similar in the training and test set
table(trainData$isFraud)/nrow(trainData)*100
table(testData$isFraud)/nrow(testData)*100
# Check if the distribution of type is similar in the training and test set
table(trainData$type)/nrow(trainData)*100
table(testData$type)/nrow(testData)*100

# Baseline Model



