# Load necessary libraries
library(tidyverse)
library(caret)
library(cluster)
library(factoextra)
library(ggplot2)

# Load the diabetes dataset
data <- read.csv("diabetes.csv")

# Data Processing
data <- data %>%
  mutate_if(is.character, as.factor) %>%
  na.omit()

# Statistical Analysis
summary(data)
correlation_matrix <- cor(data %>% select_if(is.numeric))
print(correlation_matrix)

# Visualization
# Histogram of Age
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Scatter plot of Glucose vs. Insulin
ggplot(data, aes(x = Glucose, y = Insulin)) +
  geom_point(color = "red") +
  labs(title = "Glucose vs. Insulin", x = "Glucose", y = "Insulin")

# Regression Analysis
set.seed(123)
trainIndex <- createDataPartition(data$Outcome, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- data[ trainIndex,]
testData  <- data[-trainIndex,]

model <- train(Outcome ~ ., data = trainData, method = "glm", family = "binomial")
summary(model)

# Predict on test data
predictions <- predict(model, newdata = testData)
confusionMatrix(predictions, testData$Outcome)

# Clustering
# Scale the data
scaled_data <- scale(data %>% select_if(is.numeric))

# K-means clustering
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
fviz_cluster(kmeans_result, data = scaled_data)

# Hierarchical clustering
dist_matrix <- dist(scaled_data)
hclust_result <- hclust(dist_matrix, method = "ward.D2")
fviz_dend(hclust_result, k = 3, rect = TRUE)
