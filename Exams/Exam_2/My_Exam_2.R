### Exam_2 ####
## Meaghan Barrett #

setwd('/Users/meaghanbarrett/Desktop/Classes/UVU/BIOL3100/Data_Course_BARRETT/Exams/Exam_2')

library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(broom)
library(tidyverse)
library(janitor)
library(easystats)
library(caret)

# 1. Read in the unicef data ####
unicef_data <- read.csv("unicef-u5mr.csv")

# 2. Tidy the data ####
unicef_tidy <- unicef_data %>%
  gather(key = "Year", value = "U5MR", -CountryName, -Continent, -Region) %>%
  mutate(Year = as.integer(gsub("U5MR.", "", Year)),
         U5MR = as.numeric(U5MR))

# 3. U5MR over time, faceted by continent ####
BARRETT_Plot_1<-unicef_tidy %>% 
  ggplot(aes(x = Year, y = U5MR, group = CountryName)) + 
  geom_line() + 
  facet_wrap(~ Continent, scales = "free_y") + 
  labs(y = "U5MR", x = "Year") + 
  theme(
    legend.position = "none",
    axis.title = element_text(size = 14), 
    axis.text = element_text(size = 12),   
    strip.text = element_text(size = 12)   
  ) + 
  coord_cartesian(ylim = c(0, 400)) +
  theme_bw()

# 4. Save this plot as LASTNAME_Plot_1.png (5 pts) ####
ggsave("BARRETT_Plot_1.png", plot = BARRETT_Plot_1, width = 10, height = 6)


#5. Create another plot that shows the mean U5MR for all ####
#the countries within a given continent at each year (20 pts) ####
mean_u5mr <- unicef_tidy %>%
  group_by(Year, Continent) %>%
  summarise(Mean_U5MR = mean(U5MR, na.rm = TRUE))

#Graph
BARRETT_Plot_2<-mean_u5mr %>%
  ggplot(aes(x = Year, y = Mean_U5MR, color = Continent)) + 
  geom_line(size = 2) + 
  labs(y = "Mean_U5MR", x = "Year") + 
  theme_bw()

# 6. Save this plot as LASTNAME_Plot_2.png ####
ggsave("BARRETT_Plot_2.png", plot = BARRETT_Plot_2, bg = "white", width = 10, height = 8, dpi = 300)

# 7. Create the three models ####
# Model 1: Year only
# Model 2: Year and Continent
# Model 3: Year, Continent, and their interaction
mod1 <- lm(U5MR ~ Year, data = unicef_tidy)
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

summary(mod1)
summary(mod2)
summary(mod3)

# 8. Compare the models ####
AIC(mod1, mod2, mod3)

mod1_pred <- predict(mod1)
mod2_pred <- predict(mod2)
mod3_pred <- predict(mod3)

#predict
unicef_tidy$mod1_pred <- predict(mod1, newdata = unicef_tidy)
unicef_tidy$mod2_pred <- predict(mod2, newdata = unicef_tidy)
unicef_tidy$mod3_pred <- predict(mod3, newdata = unicef_tidy)

## rename
unicef_long <- unicef_tidy %>%
  gather(key = "model", value = "predicted_u5mr", mod1_pred, 
         mod2_pred, mod3_pred)%>%
  mutate(model = recode(model, 
                        "mod1_pred" = "mod1", 
                        "mod2_pred" = "mod2", 
                        "mod3_pred" = "mod3"))

# 9. Plot the 3 models’ predictions like so: (10 pts)####
unicef_long %>% 
  ggplot(aes(x = Year, y = predicted_u5mr, color = Continent)) + 
  geom_line(size = 1) + 
  facet_wrap(~ model, scales = "fixed") + 
  labs(title = "Model Predictions", 
       y = "Predicted U5MR", 
       x = "Year") + 
  theme_bw() + 
  theme_minimal() + 
  theme(legend.position = "right", 
        strip.background = element_rect(color = "black", fill = "lightgray", size = 1), 
        strip.text = element_text(size = 12), 
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        legend.background = element_blank(),  
        legend.key = element_rect(fill = "lightgrey",  color = "lightgrey"),    
        legend.box.background = element_blank(),   
        scale_y_continuous(limits = c(-40, 280), 
                           breaks = seq(0, 200, by = 100))) 


# 10. BONUS - Predict U5MR for Ecuador in 2020 ####
# Read the data
ecuador <- read.csv('fusion_GLOBAL_DATAFLOW_UNICEF_1.0_ECU.CME_MRY0T4..csv')

str(ecuador)

# Clean the data
ecuador <- ecuador %>%
  select(where(~ !all(is.na(.)))) %>%  
  rename( 
    dataflow = DATAFLOW, 
    geographic_area = `REF_AREA.Geographic.area`, 
    indicator = `INDICATOR.Indicator`,
    sex = `SEX.Sex`, 
    year = `TIME_PERIOD.Time.period`, 
    observation_value = `OBS_VALUE.Observation.Value`, 
    unit_of_measure = `UNIT_MEASURE.Unit.of.measure`, 
    observation_status = `OBS_STATUS.Observation.Status`, 
    lower_bound = `LOWER_BOUND.Lower.Bound`, 
    upper_bound = `UPPER_BOUND.Upper.Bound`, 
    data_source = `DATA_SOURCE.Data.Source`, 
    age = `AGE.Current.age`
  ) %>%
  filter(year >= 1995)
View(ecuador)

# clean the column names
colnames(ecuador)
ecuador <- clean_names(ecuador)
ecuador <- ecuador %>%
  mutate(across(where(is.character), ~ gsub("^[^:]*:", "", .)))

# Filter for the relevant columns
ecuador_u5mr <- ecuador %>%
  filter(!is.na(observation_value)) %>%
  select(year, observation_value)

# linear regression model predicting 'observation_value' (U5MR)
lm_model <- lm(observation_value ~ year, data = ecuador_u5mr)

# predictions for the year 2020
predicted_u5mr <- predict(lm_model, newdata = data.frame(year = 2020))

# real value for 2020 is 13
real_u5mr_2020 <- 13

# difference between the model's prediction and the real value
prediction_difference <- abs(predicted_u5mr - real_u5mr_2020)

# results
cat("Model Prediction for U5MR in 2020:", predicted_u5mr, "\n")
cat("Real U5MR in 2020:", real_u5mr_2020, "\n")
cat("Difference between predicted value and real U5MR:", prediction_difference, "\n")
