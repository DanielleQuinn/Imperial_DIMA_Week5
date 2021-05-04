# ---- Prepare Your R Workspace ----
# Please note that using this online R session you do not
# need to set your working directory!

# Load packages
library(readxl)
library(clue)
library(ggplot2)

# Import training data
train <- read_excel('airline_train.xlsx')

# Import test data
test <- read_excel('airline_test.xlsx')

# Make a copy of the training data
train_copy <- train

# Make a copy of the test data
test_copy <- test

# ---- Data Preparation ----
# 1. Drop customer_satisfaction from the training data
train <- train[, -which(names(train) == "customer_satisfaction")]

# 2. Convert all non-numeric variables in both train and test to numeric
# Examine the structure of the training data
str(train)

# Convert variables in the training data to numeric
train$gender <- as.numeric(as.factor(train$gender))
train$customer_type <- as.numeric(as.factor(train$customer_type))
train$travel_type <- as.numeric(as.factor(train$travel_type))
train$class <- as.numeric(as.factor(train$class))

# Examine the new structure of the training data
str(train)

# Examine the structure of the test data
str(test)

# Convert variables in the test data to numeric
test$gender <- as.numeric(as.factor(test$gender))
test$customer_type <- as.numeric(as.factor(test$customer_type))
test$travel_type <- as.numeric(as.factor(test$travel_type))
test$class <- as.numeric(as.factor(test$class))

# Examine the new structure of the test data
str(test)

# 3. Check to see if there are any missing values in train or test; replace the missing values with 0
# How many missing values are in the training data?
sum(is.na(train))

# Which column do they occur in?
colnames(train)[colSums(is.na(train)) > 0]

# Replace them with 0
train$arrival_delay_minutes[is.na(train$arrival_delay_minutes)] <- 0

# How many missing values are in the test data?
sum(is.na(test))

# Which column do they occur in?
colnames(test)[colSums(is.na(test)) > 0]

# Replace them with 0
test$arrival_delay_minutes[is.na(test$arrival_delay_minutes)] <- 0

# 4. Drop the variable called gate_location from the train and test data
# Drop gate_location from the training data
train <- train[, -which(names(train) == "gate_location")]

# Drop gate_location from the test data
test <- test[, -which(names(test) == "gate_location")]

# 5. Scale the data in train and test
# Scale the training data
train <- scale(train)

# Scale the test data
test <- scale(test)

# ---- Algorithm Training ----
# Create a k means model using the cleaned training data
# Specify random number set
set.seed(20)

# Train k means algorithm
clustermodel <- kmeans(train, centers = 2, nstart = 25)

# ---- Training Results ----
# 1. Add a column to train_copy containing the cluster of each customer, as extracted from the clustermodel results
train_copy$cluster <- as.factor(clustermodel$cluster)

# ---- Data Visualization ----
# Generate figure 1: gender vs cluster
ggplot(train_copy) + 
  geom_bar(aes(x = gender, fill = cluster)) +
  theme_bw(14) +
  scale_fill_manual(values = c("darkorange2", "blue4"), name = "Cluster") +
  xlab("Gender") + 
  ylab("Frequency")
  
# Generate figure 2: baggage handling rating vs cluster
ggplot(train_copy) + 
  geom_bar(aes(x = baggage_handling, fill = cluster)) +
  theme_bw(14) +
  scale_fill_manual(values = c("darkorange2", "blue4"), name = "Cluster") +
  xlab("Baggage Handling Rating") + 
  ylab("Frequency")
  
# Generate figure 3: customer loyalty vs cluster
ggplot(train_copy) + 
  geom_bar(aes(x = customer_type, fill = cluster)) +
  theme_bw(14) +
  scale_fill_manual(values = c("darkorange2", "blue4"), name = "Cluster") +
  xlab("Customer Type") + 
  ylab("Frequency")
  
# Generate figure 4: inflight service rating vs cluster
ggplot(train_copy[train_copy$inflight_service > 0,]) + 
  geom_bar(aes(x = inflight_service, fill = cluster)) +
  theme_bw(14) +
  scale_fill_manual(values = c("darkorange2", "blue4"), name = "Cluster") +
  xlab("Inflight Service Rating") + 
  ylab("Frequency")

# Compare clusters to customer satisfaction
table(train_copy$cluster, train_copy$customer_satisfaction)

# ---- Algorithm Application ----
# Make predictions about cluster using the test data
clustertest <- cl_predict(clustermodel, test)

# Add predictions to test_copy
test_copy$cluster <- clustertest

# ---- Assessment of Results ----
# Import actual data
actual <- read_excel('airline_actual.xlsx')

# Compare predicted clusters to actual customer satisfaction
table(test_copy$cluster, actual$customer_satisfaction)
