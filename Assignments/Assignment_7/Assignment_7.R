#Assignment_7 Code####

getwd(/Users/meaghanbarrett/Desktop/Classes/UVU/BIOL3100/Data_Course_BARRETT/Assignments/Assignment_7)

# Import necessary libraries
library(tidyverse)

# Import csv file
data <- read.csv("Utah_Religions_by_County.csv")

# View data
View(data)

# Check data structure
str(data)

# Tidy the data: gather specifics
tidy_data <- data %>%
  gather(key = "Religion", value = "Proportion", -County, -Pop_2010) %>%
  arrange(County, Religion)

# View dataset
View(tidy_data)

# Graphing counties religious groups proportion
tidy_data %>% 
  ggplot(aes(x = County, y = Proportion, fill = Religion)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "County Religious Groups Proportion", 
       x = "County", 
       y = "Proportion") 

# Graph county distribution
data %>% 
  ggplot(aes(x = County, y = Pop_2010)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Population of Counties", x = "County", y = "Population")

# Graph population and LDS proportion
data %>% 
  ggplot(aes(x = Pop_2010, y = LDS)) +
  geom_point() +
  geom_smooth(method = "lm", col = "orange") +
  labs(title = "LDS Proportion vs Population", x = "Population", y = "Proportion LDS")

# Correlation coefficient calculation
cor(data$Pop_2010, data$LDS)

# Graph LDS and non-religious population
ggplot(data, aes(x = LDS, y = Non.Religious)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "LDS vs Non-religious Proportion", x = "Proportion LDS", y = "Proportion Non-religious")

# Correlation coefficient calculation
cor(data$LDS, data$Non.Religious)

#### QUESTIONS ####
#“Does population of a county correlate with the proportion of any specific religious group in that county?”
## Answer: Yes, LDS [1] -0.002895282

ggplot(data, aes(x = Pop_2010, y = LDS)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "orange") + 
  labs(title = "LDS Proportion vs Population",
       x = "Population", y = "Proportion of LDS")

# Calculate population and LDS proportion
correlation_LDS <- cor(data$Pop_2010, data$LDS)
correlation_LDS

#“Does proportion of any specific religion in a given county correlate with the proportion of non-religious 
#people?”  
## Answer: -0.8697708

ggplot(data, aes(x = LDS, y = Non.Religious)) +
  geom_point() + 
  geom_smooth(method = "lm", col = "blue") + 
  labs(title = "LDS vs Non-religious Proportion",
       x = "Proportion of LDS", y = "Proportion of Non-religious")

# Calculate LDS and proportion of non-religious
correlation_LDS_non_religious <- cor(data$LDS, data$Non.Religious)
correlation_LDS_non_religious

