# Load necessary libraries
library(nnet)
library(pROC)
library(PRROC)

# Create a data frame with your data
data <- data.frame(
  ID = c(133, 134, 135, 136, 137, 138, 139, 140, 141, 142),
  State = c("Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh", "Madhya Pradesh",
            "Madhya Pradesh", "Madhya Pradesh", "Maharashtra", "Maharashtra", "Maharashtra"),
  Date = c("30-04-2020", "31-05-2020", "30-06-2020", "31-07-2020", "31-08-2020", "30-09-2020", "31-10-2020",
           "31-01-2020", "29-02-2020", "31-03-2020"),
  Gender = c("M", "M", "M", "M", "M", "M", "M", "M", "M", "M"),
  Value1 = c(12.36, 21.98, 6.48, 5.08, 4.70, 3.91, 3.13, 4.95, 4.69, 5.79),
  Target = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
)

# Convert categorical variables to factors
data$State <- as.factor(data$State)
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")
data$Gender <- ifelse(data$Gender == "M", 1, 0)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Create and train the logistic regression model
model <- glm(Target ~ ID + Value1 + State + Gender, data = train_data, family = binomial)

# Make predictions
predictions <- predict(model, test_data, type = "response")

# Calculate accuracy
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(predicted_classes == test_data$Target)
print(paste("Accuracy:", accuracy))

# Create ROC curve using pROC package
roc_obj <- roc(test_data$Target, predictions)
plot(roc_obj, main = "ROC Curve")

# Create precision-recall curve using PRROC package
pr_obj <- pr.curve(test_data$Target, predictions, curve = TRUE)  # Specify curve = TRUE
plot(pr_obj, main = "Precision-Recall Curve")

# Calculate and print F1 score
f1_score <- function(true_labels, predicted_labels) {
  tp <- sum(true_labels == 1 & predicted_labels == 1)
  precision <- tp / sum(predicted_labels == 1)
  recall <- tp / sum(true_labels == 1)
  f1 <- 2 * (precision * recall) / (precision + recall)
  return(f1)
}

f1 <- f1_score(test_data$Target, predicted_classes)
print(paste("F1 Score:", f1))

# Create confusion matrix
confusion_matrix <- table(Actual = test_data$Target, Predicted = predicted_classes)
print("Confusion Matrix:")
print(confusion_matrix)

# Calculate AUC-PR
pr_obj <- pr.curve(test_data$Target, predictions, curve = TRUE)  # Specify curve = TRUE
auc_pr <- pr_obj$auc.integral
print(paste("AUC-PR:", auc_pr))