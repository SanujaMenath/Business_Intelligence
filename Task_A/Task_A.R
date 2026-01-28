#load data
data <- read.csv("D://ICBT//Sem 2//CIS 6008-Analytics and Business Intelligence//Analytics_and_Business_Intelligence//data//HOTELS_2025.csv")
head(data)
str(data)

missing_pct <- colMeans(is.na(data)) * 100
missing_pct

sum(duplicated(data))

summary(data)

#Visualize distribution of Revenue
hist(
  data$Revenue,
  main = "Distribution of Hotel Revenue",
  xlab = "Revenue",
  col = "lightblue"
)

#Box plot for Revenue
boxplot(
  data$Revenue,
  main = "Boxplot of Hotel Revenue",
  ylab = "Revenue"
)



# H0: Data is normally distributed
# H1: Data is NOT normally distributed
shapiro.test(data$Revenue)

qqnorm(data$Revenue)
qqline(data$Revenue, col = "red")

#Normality tests for independent variables
shapiro.test(data$OccupancyRate)
shapiro.test(data$ADR)
shapiro.test(data$MarketingSpend)
shapiro.test(data$GuestSatisfactionScore)



#Correlation Testing
colnames(data)

library(tidyverse)

numeric_data <- data %>%
  select(
    Revenue,
    RoomsAvailable,
    OccupancyRate,
    ADR,
    MarketingSpend,
    StaffCount,
    GuestSatisfactionScore,
    LoyaltyMembers
  )
cor_matrix <- cor(numeric_data, method = "pearson")
cor_matrix

cor.test(data$Revenue, data$OccupancyRate)
cor.test(data$Revenue, data$ADR)
cor.test(data$Revenue, data$MarketingSpend)
cor.test(data$Revenue, data$GuestSatisfactionScore)
cor.test(data$Revenue, data$LoyaltyMembers)



#Visualizing using a Scatter Plot
library(ggplot2)
ggplot(data, aes(x=OccupancyRate, y=Revenue)) +
  geom_point() +
  geom_smooth(method="lm", col="red") +
  labs(title="Impact of Occupancy on Revenue", x="Occupancy Rate (%)", y="Revenue (LKR)")

selected_vars <- data %>%
  select(
    Revenue,
    OccupancyRate,
    ADR,
    MarketingSpend,
    GuestSatisfactionScore,
    LoyaltyMembers
  )

#linear regression with OccupancyRate
model_occ <- lm(Revenue ~ OccupancyRate, data = data)
summary(model_occ)

plot(
  data$OccupancyRate,
  data$Revenue,
  main = "Revenue vs Occupancy Rate",
  xlab = "Occupancy Rate",
  ylab = "Revenue"
)

abline(model_occ, col = "red")

#linear regression with ADR
model_adr <- lm(Revenue ~ ADR, data = data)
summary(model_adr)

#linear regression with MarketingSpend
model_marketing <- lm(Revenue ~ MarketingSpend, data = data)
summary(model_marketing)

#linear regression with GuestSatisfactionScore
model_satisfaction <- lm(Revenue ~ GuestSatisfactionScore, data = data)
summary(model_satisfaction)

#Multiple Linear Regression and model building
multi_model <- lm(
  Revenue ~ OccupancyRate + ADR + MarketingSpend + GuestSatisfactionScore + LoyaltyMembers,
  data = data
)

summary(multi_model)

library(car)
vif(multi_model)

plot(multi_model$fitted.values, resid(multi_model),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(resid(multi_model))
qqline


hist(resid(multi_model),
     main = "Distribution of Residuals",
     xlab = "Residuals")



final_model <- multi_model
summary(final_model)



