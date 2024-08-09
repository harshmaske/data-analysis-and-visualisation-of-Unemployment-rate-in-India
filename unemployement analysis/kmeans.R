library(ggplot2)
library(stats)

data <- read.csv("/Users/harshvmx/Desktop/unemployment.csv")

kmeans_model <- kmeans(data[, c("EstimatedUnemploymentRate")], centers = 3)

data$Cluster <- as.factor(kmeans_model$cluster)

data$Color <- ifelse(data$EstimatedUnemploymentRate > 25, "Exceeding 25",
                     ifelse(data$EstimatedUnemploymentRate >= 10, "Between 10 and 25", "Below 10"))

ggplot(data = data, aes(x = Region, y = EstimatedUnemploymentRate, color = Color)) +
  geom_point() +
  scale_color_manual(values = c("green", "blue", "red"), 
                     labels = c("Below 10", "Between 10 and 25", "Exceeding 25")) +
  labs(title = "Unemployment Rate Clustering in India Using K-means Clustering",
       x = "Region", y = "Estimated Unemployment Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
