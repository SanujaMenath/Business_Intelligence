# Module: CIS6008 – Analytics and Business Intelligence
# Task a: Statistical Analysis of Hotel Revenue
# Context: Sri Lanka Tourism Sector

# Load required libraries
library(ggplot2)
library(corrplot)

# 1. Import Dataset
data <- read.csv("D://ICBT//Sem 2//CIS 6008-Analytics and Business Intelligence//Analytics_and_Business_Intelligence//data//HOTELS_2025.csv")

# Structure and descriptive statistics
str(data)
summary(data)

# Convert HotelQualityRank to a factor
data$HotelQualityRank <- as.factor(data$HotelQualityRank)

# Verify the change
str(data$HotelQualityRank)


par(mfrow=c(1,2))
# Histogram
hist(data$Revenue, main="Histogram of Revenue", xlab="Revenue", col="lightblue", probability=TRUE)
lines(density(data$Revenue), col="red", lwd=2)

# Q-Q Plot
qqnorm(data$Revenue, main="Q-Q Plot of Revenue")
qqline(data$Revenue, col="red")

boxplot(
  data$Revenue,
  main = "Boxplot of Hotel Revenue",
  ylab = "Revenue"
)

# 3. Hypothesis Testing

# H0: Data is normally distributed
# H1: Data is NOT normally distributed
shapiro.test(data$Revenue)
shapiro.test(data$ADR)
shapiro.test(data$OccupancyRate)
shapiro.test(data$MarketingSpend)
# accept the Null Hypothesis, because data is normally distributed(p-value > 0.05)


# 4. correlation analysis

# exclude 'HotelQualityRank' because it is categorical
numeric_vars <- data[, c("Revenue", "RoomsAvailable", 
                         "OccupancyRate", "ADR", "MarketingSpend", 
                         "StaffCount", "GuestSatisfactionScore", 
                         "LoyaltyMembers")]

# Calculate the Correlation Matrix
cor_matrix <- cor(numeric_vars)
revenue_cor <- sort(cor_matrix[, "Revenue"], decreasing = TRUE)
print(revenue_cor)

# Visualize the correlation
library(corrplot)
corrplot(cor_matrix, method="number", type="upper", 
         tl.col="black", tl.cex=0.8, number.cex=0.8, 
         title="Correlation with Revenue", mar=c(0,0,1,0))

# 4. Regression Modelling

# Simple Linear Regression
# Formula: Revenue = Intercept + (Slope * RoomsAvailable)
simple_model <- lm(Revenue ~ RoomsAvailable, data = data)

# 2. View Summary Statistics
summary(simple_model)

# 3. Best Fit Line
# Enhanced Scatter Plot with Regression Line using ggplot2
ggplot(data, aes(x=RoomsAvailable, y=Revenue)) +
  geom_point(color="blue", alpha=0.6) +     # The dots
  geom_smooth(method="lm", color="red", se=TRUE) +  # The regression line with confidence interval
  labs(title="Regression: Revenue vs Rooms Available",
       x="Rooms Available", y="Revenue ($)") +
  theme_minimal()

# Multiple Linear Regression
multi_model <- lm(Revenue ~ RoomsAvailable + ADR + OccupancyRate + MarketingSpend + StaffCount, data = data)

summary(multi_model)
# split screen into 4 to visualize all graphs
par(mfrow=c(2,2))
plot(multi_model) 
# reset to visualize 1
par(mfrow=c(1,1))

# 5. Prediction
# Create a Hypothetical Hotel scenario
new_hotel_scenario <- data.frame(
  RoomsAvailable = 200,
  OccupancyRate = 0.75,
  ADR = 150,
  MarketingSpend = 20000,
  StaffCount = 40
)

# Predict Revenue using multi_model
predicted_revenue <- predict(multi_model, newdata = new_hotel_scenario)

# Result
print(paste("Predicted Monthly Revenue: $", round(predicted_revenue, 2)))
