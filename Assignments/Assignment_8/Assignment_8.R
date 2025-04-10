#Assignment_8

setwd("~/Desktop/Classes/UVU/BIOL3100/Data_Course_BARRETT")

library(tidyverse)
library(modelr)
library(broom)
library(ggplot2)
library(mgcv)
library(easystats)

#loads the “Data/mushroom_growth.csv” data set
growth <-read.csv('Data/mushroom_growth.csv')
View(growth)

# Quick look
glimpse(growth)

# Plot relationships between GrowthRate and predictors
# 1. GrowthRate vs Light
growth %>% 
  ggplot(aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ggtitle("GrowthRate vs Light")

# 2. GrowthRate vs Temperature
growth %>% 
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ggtitle("GrowthRate vs Temperature")

# 3. GrowthRate vs Humidity
growth %>% 
  ggplot(aes(x = Humidity, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ggtitle("GrowthRate vs Humidity")

# 4. GrowthRate vs Nitrogen
growth %>% 
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  ggtitle("GrowthRate vs Nitrogen")

#5 GrowthRate vs Species
growth %>%   
  ggplot(aes(x=Species, y=GrowthRate)) +
  geom_boxplot() + theme_minimal()+
  ggtitle("GrowthRate vs Species (categorical)")


# Defining multiple models to explain GrowthRate
# Model 1: Simple linear model with Temperature as predictor
mod1 <- lm(GrowthRate ~ Temperature, data = growth)

# Model 2: Multiple linear model with Temperature, Light, and Nitrogen as predictors
mod2 <- lm(GrowthRate ~ Temperature + Light + Nitrogen, data = growth)

# Model 3: Multiple linear model with Temperature, Light, and Humidity as predictors
mod3 <- lm(GrowthRate ~ Temperature + Light + Humidity, data = growth)

# Model 4: Interaction model between Temperature and Nitrogen
mod4 <- lm(GrowthRate ~ Temperature * Nitrogen, data = growth)

# Calculate Mean Squared Error (MSE) for each model
mse_mod1 <- mean(mod1$residuals^2)
mse_mod2 <- mean(mod2$residuals^2)
mse_mod3 <- mean(mod3$residuals^2)
mse_mod4 <- mean(mod4$residuals^2)

# Display MSE values for comparison
mse_values <- data.frame(
  Model = c("mod1", "mod2", "mod3", "mod4"),
  MSE = c(mse_mod1, mse_mod2, mse_mod3, mse_mod4)
)
print(mse_values)

# Select the best model based on MSE (lower MSE is better)
best_model <- mod3 

# Convert 'Humidity' to a factor (categorical variable)
growth$Humidity <- factor(growth$Humidity)

# Convert 'Light' to a factor if needed (representing categories like Low, High)
growth$Light <- factor(growth$Light, levels = c(0, 10, 20))

# Now check the structure again
str(growth)

new_data <- data.frame(
  Temperature = c(30, 35, 40),
  Light = c(10, 10, 20),  
  Nitrogen = c(10, 15, 20),
  Humidity = factor(c("Low", "High", "Low"), levels = c("Low","High"))  
)

# Now make predictions using the best model
predictions <- predict(best_model, newdata = new_data)

# Combine predictions with the new data for visualization
hyp_predictions <- data.frame(
  Temperature = new_data$Temperature,
  Light = new_data$Light,
  Nitrogen = new_data$Nitrogen,
  Humidity = new_data$Humidity,
  PredictedGrowthRate = predictions
)

# Print out the predictions
print(hyp_predictions)

# Plot real vs predicted values (real data + predictions)
growth %>% 
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_point(aes(color = "Real")) +
  geom_point(data = hyp_predictions, aes(x = Temperature, y = PredictedGrowthRate, color = "Predicted")) +
  theme_minimal() +
  ggtitle("Real vs Predicted GrowthRate")

mod_non_linear <- lm(GrowthRate ~ factor(Temperature), data = growth)
summary(mod_non_linear)

# Load non-linear_relationship.csv
non_linear_data <- read.csv("Data/non_linear_relationship.csv")

# Visualize the curve
non_linear_data %>%    
  ggplot(aes(x = predictor, y = response)) +
  geom_point() +
  theme_minimal() +
  ggtitle("Predictor vs Response")

# Fit a polynomial model to capture non-linearity
mod_poly <- lm(response ~ poly(predictor, 2), data = non_linear_data)
summary(mod_poly)

# Visualize fit poly
non_linear_data %>%  
  ggplot(aes(x = predictor, y = response)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = FALSE) +
  theme_minimal()

# Fit a GAM model (automatic smoothness)
mod_gam <- gam(response ~ s(predictor), data = non_linear_data)
summary(mod_gam)

# Visualize fit
non_linear_data %>% 
  ggplot(aes(x = predictor, y = response)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x), color = "red", se = FALSE) +
  theme_minimal()

# Visualize fitted curve
non_linear_data %>%  
  ggplot(aes(x = predictor, y = response)) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "blue", se = FALSE) +
  stat_smooth(method = "gam", formula = y ~ s(x), color = "red", se = FALSE) +
  theme_minimal() +
  ggtitle("Polynomial (blue) vs GAM (red)")

# Compare model performance
AIC(mod_poly, mod_gam)
