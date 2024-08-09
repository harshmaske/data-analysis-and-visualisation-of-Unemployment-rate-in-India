library(ggplot2)
data <- read.csv("/Users/harshvmx/Desktop/unemployment.csv")

model <- lm(EstimatedUnemploymentRate ~ Region, data = data)

data$Color <- ifelse(data$EstimatedUnemploymentRate > 25, "Exceeding 25",
                     ifelse(data$EstimatedUnemploymentRate >= 10, "Between 10 and 25", "Below 10"))


ggplot(data, aes(x = Region, y = EstimatedUnemploymentRate, color = Color)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = c("green", "blue", "red"), 
                     labels = c("Below 10", "Between 10 and 25", "Exceeding 25")) +
  labs(title = "Unemployement Rate In india Using linear regression",
       x = "Region", y = "Estimated Unemployment Rate") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
