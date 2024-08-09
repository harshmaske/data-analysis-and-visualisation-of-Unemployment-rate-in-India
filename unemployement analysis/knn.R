library(ggplot2)
library(class)

data <- read.csv("/Users/harshvmx/Desktop/unemployment.csv")

threshold <- 25

k <- 3 
data$Category <- ifelse(data$EstimatedUnemploymentRate > 25, "Exceeding 25",
                        ifelse(data$EstimatedUnemploymentRate >= 10, "Between 10 and 25", "Below 10"))

ggplot(data, aes(x = Region, y = EstimatedUnemploymentRate, color = Category)) +
  geom_point() +
  scale_color_manual(values = c("green", "blue", "red"), 
                     labels = c("Below 10", "Between 10 and 25", "Exceeding 25")) +
  labs(title = "Scatter Plot of Estimated Unemployment Rate",
       x = "Region", y = "Estimated Unemployment Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
