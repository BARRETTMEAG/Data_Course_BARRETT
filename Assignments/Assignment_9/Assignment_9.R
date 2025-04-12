#Assignment_9

library(tidyverse) 
library(caret)     
library(ggplot2)    
library(dplyr)  
library(corrplot)
library(pROC)
library(MASS)

school <- read.csv("Data/GradSchool_Admissions.csv")

# Check for missing values
school <- na.omit(school)
sum(is.na(school))

# Convert the 'admit' column to factor (since it's binary)
school$admit <- as.factor(school$admit)

# Check data types
str(school)

school <- school %>%
  mutate(
    rank = factor(rank),
    admit = factor(admit, labels = c("Failure", "Admitted"))
)


# Summary statistics
summary(school)

# Admission rate by rank
school %>%
  group_by(rank) %>%
  summarise(admit_rate = mean(admit == "Admitted"))

# GRE vs GPA, colored by admit
school %>% 
  ggplot(aes(x = gre, y = gpa, color = admit)) +
  geom_point(alpha = 0.6) +
  labs(title = "GRE vs GPA by Admission Status")

# Admission rate by rank (bar plot)
school %>% 
  ggplot(aes(x = rank, fill = admit)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Admission Rate by Rank")

# Fit a logistic regression model
model <- glm(admit ~ gre + gpa + rank, data = school, family = "binomial")
summary(model)

# Get predicted probabilities
school$prob <- predict(model, type = "response")

# Confusion matrix (using 0.5 cutoff)
school$predicted <- ifelse(school$prob > 0.5, "Admitted", "Failed")
table(school$admit, school$predicted)

# Accuracy
mean(school$admit == school$predicted)


# Histogram of GRE scores
school %>% 
  ggplot(aes(x = gre)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~admit)+
  labs(title = "Distribution of GRE Scores", x = "GRE Score", y = "Frequency")

# Histogram of GPA scores
school %>% 
  ggplot(aes(x = gpa)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  facet_wrap(~admit) +
  labs(title = "Distribution of GPA", x = "GPA", y = "Frequency")

# Boxplot of GPA by Admission Status
school %>% 
  ggplot(aes(x = admit, y = gpa)) +
  geom_boxplot() +
  facet_wrap(~admit)+
  labs(title = "GPA by Admission Status", x = "Admission Status", y = "GPA")

# Boxplot of Rank by Admission Status
school %>% 
  ggplot(aes(x = admit, y = rank)) +
  geom_boxplot() +
  facet_wrap(~admit)+
  labs(title = "Rank by Admission Status", x = "Admission Status", y = "Rank")

cor_matrix <- school %>%
  mutate(rank = as.numeric(rank)) %>%
  dplyr::select(gre, gpa, rank) %>%
  cor()

print(cor_matrix)


# Split the school data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(school$admit, p = 0.8, list = FALSE)
trainData <- school[trainIndex, ]
testData <- school[-trainIndex, ]

# Build the model on the training data
model_train <- glm(admit ~ gre + gpa + rank, data = trainData, family = binomial)

# Predict on the test set
predictions <- predict(model_train, testData, type = "response")

# Convert predictions to binary values
predicted_class <- ifelse(predictions > 0.5, 1, 0)

# Make sure predicted and actual have the same factor levels
predicted_class <- factor(predicted_class, levels = c("Failure", "Admitted"))
actual_class <- factor(testData$admit, levels = c("Failure", "Admitted"))

# Then run confusion matrix
confusionMatrix(predicted_class, actual_class)


# Then run confusion matrix
confusionMatrix(predicted_class, actual_class)


# ROC and AUC
roc_curve <- roc(testData$admit, predictions)

# Plot the ROC curve
plot(roc_curve, col = "blue", main = "ROC Curve for Admission Model")

# AUC
auc_value <- auc(roc_curve)
auc_value