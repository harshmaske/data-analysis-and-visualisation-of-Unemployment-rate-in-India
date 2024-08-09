
library(ggplot2)
library(randomForest)


data <- read.csv("/Users/harshvmx/Desktop/unemployment.csv")

rf_model <- randomForest(EstimatedUnemploymentRate ~ ., data = data)

data$RF_Predictions <- predict(rf_model, data)

ggplot(data = data, aes(x = factor(Region))) +
  geom_bar(aes(y = EstimatedUnemploymentRate), stat = "identity", fill = "blue" +
  geom_bar(aes(y = RF_Predictions), stat = "identity", fill = "red") +
  labs(title = "Unemployment Rate in India with Random Forest Predictions",
       x = "Region", y = "Estimated Unemployment Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  