# Module: CIS6008 - Analytics and Business Intelligence
# Task a: Statistical Analysis of Hotel Revenue for Sri Lanka Tourism Sector

# Load Required Libraries
# Tidyverse for data manipulation and ggplot2 for visualizations
# Car for advanced regression diagnostics (VIF)
library(tidyverse)
library(car)

# Import Data
data <- read.csv("D://ICBT//Sem 2//CIS 6008-Analytics and Business Intelligence//Analytics_and_Business_Intelligence//data//HOTELS_2025.csv")

# Check data types and descriptive statistics
str(data)
summary(data)

#  Normality Testing based on Hypothesis
# H0: The distribution of Revenue is normal.
# H1: The distribution of Revenue is NOT normal.

shapiro_test <- shapiro.test(data$Revenue)
print(shapiro_test)

# Based on the p-value < 0.05, we can reject H0. 
# The data is non-normal, therefore, we must use non-parametric correlation.

#  Correlation Analysis (Non-Parametric)
# H0: No significant association exists between Revenue and independent variables.
# H1: A significant association exists.
# Using 'spearman' method due to non-normality of the dependent variable.

numeric_data <- data %>% 
  select(Revenue, OccupancyRate, ADR, MarketingSpend, 
         GuestSatisfactionScore, LoyaltyMembers)

cor_matrix <- cor(numeric_data, method = "spearman")
print("Spearman Correlation Matrix:")
print(cor_matrix)

# --- 5. Professional Visualizations (Graphical Simulations) ---
#  - "Scatter plot graphical simulations supported with findings."

# Figure 1: Revenue vs Occupancy Rate
ggplot(data, aes(x = OccupancyRate, y = Revenue)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(title = "Figure 1: Relationship between Occupancy Rate and Revenue",
       subtitle = "Linear regression trend with 95% confidence interval",
       x = "Occupancy Rate (%)",
       y = "Revenue (LKR)") +
  theme_minimal()

# Figure 2: Revenue vs Marketing Spend (Example of multiple simulations)
ggplot(data, aes(x = MarketingSpend, y = Revenue)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(title = "Figure 2: Impact of Marketing Spend on Revenue",
       x = "Marketing Spend (LKR)",
       y = "Revenue (LKR)") +
  theme_minimal()

# --- 6. Regression Modeling (Full Scale) ---
#  - Requires "full scale of regression analysis (simple and multiple)."

# A. Simple Linear Regression (Baseline)
model_simple <- lm(Revenue ~ OccupancyRate, data = data)
summary(model_simple)

# B. Multiple Linear Regression (Evidence-Based Model)
# H0: Coefficients are equal to zero (no effect).
# H1: At least one predictor has a significant effect on Revenue.
multi_model <- lm(Revenue ~ OccupancyRate + ADR + MarketingSpend + 
                    GuestSatisfactionScore + LoyaltyMembers, data = data)

# --- 7. Model Refinement & Diagnostics ---
# Check for Multicollinearity using Variance Inflation Factor (VIF)
# VIF > 5-10 indicates problematic correlation between predictors.
vif_results <- vif(multi_model)
print("VIF Results:")
print(vif_results)

# Final Model Summary
summary(multi_model)

# --- 8. Residual Diagnostics (Methodological Rigor) ---
# [cite: 40] - "ensuring methodological rigor."
# This checks if the model's assumptions hold true.

par(mfrow = c(2, 2)) 
plot(multi_model)
par(mfrow = c(1, 1))

# --- 9. Final Mathematical Model ---
# Print coefficients for writing the formula in the report.
print("Model Coefficients for Formula Calculation:")
print(coef(multi_model))
