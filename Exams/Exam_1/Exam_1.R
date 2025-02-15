####Exam_1####
#I. Read the cleaned_covid_data.csv file into an R data frame. (20 pts)####

read.csv('Data/cleaned_covid_data.csv')
covid_data <-read.csv('Data/cleaned_covid_data.csv')

View(covid_data)
head(covid_data)

#II. Subset the data set to just show states that begin with “A” and save this as an object called A_states. (20 pts)####
#Use the tidyverse suite of packages
#Selecting rows where the state starts with “A” is tricky (you can use the grepl() function or just a vector of those states if you prefer)
library(tidyverse)

covid_data %>% 
  filter(grepl("^A", Province_State))

A_states <- covid_data %>% 
  filter(grepl("^A", Province_State))

head(A_states)
View(A_states)

#III. Create a plot of that subset showing Deaths over time, with a separate facet for each state. (20 pts)####
#Create a scatterplot
#Add loess curves WITHOUT standard error shading
#Keep scales “free” in each facet

A_states %>%
  ggplot(aes(x = as.Date(Last_Update), y = Deaths)) +
  geom_point() + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") + 
  labs(title = "COVID-19 Deaths for 'A' States",
       x = "Dates", y = "Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#IV. (Back to the full dataset) Find the “peak” of Case_Fatality_Ratio for ####
#each state and save this as a new data frame object called state_max_fatality_rate. (20 pts)####
#I’m looking for a new data frame with 2 columns: “Province_State” & “Maximum_Fatality_Ratio”
#Arrange the new data frame in descending order by Maximum_Fatality_Ratio
#This might take a few steps. Be careful about how you deal with missing values!
  
state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%
  summarise(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

head(state_max_fatality_rate)
View(state_max_fatality_rate)

#V. Use that new data frame from task IV to create another plot. (20 pts)####
#X-axis is Province_State & Y-axis is Maximum_Fatality_Ratio
#bar plot
#x-axis arranged in descending order, just like the data frame (make it a factor to accomplish this)
#X-axis labels turned to 90 deg to be readable
#Even with this partial data set (not current), you should be able to see that (within these dates), different states had very different fatality ratios.

#This chart has the words connected with a _
state_max_fatality_rate %>%
  ggplot(aes(x = reorder(Province_State, -Maximum_Fatality_Ratio), y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  labs(title = "States Maximum Fatality Ratio",
       x = "Province_State", y = "Maximum_Fatality_Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#without _ connecting words
state_max_fatality_rate %>%
  ggplot(aes(x = reorder(Province_State, -Maximum_Fatality_Ratio), 
             y = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  labs(title = "States Maximum Fatality Ratio",
       x = "Province State", y = "Maximum Fatality Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#VI. (BONUS 10 pts) Using the FULL data set, plot cumulative deaths for the entire US over time####
#You’ll need to read ahead a bit and use the dplyr package functions group_by() and summarize() to accomplish this.
library(dplyr)

us_cumulative_deaths <- covid_data %>%
  group_by(Last_Update) %>%
  summarise(Cumulative_Deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(Last_Update)

us_cumulative_deaths %>%
  ggplot(aes(x = as.Date(Last_Update), y = Cumulative_Deaths)) +
  geom_line() +
  labs(title = "USA: Cumulative COVID-19 Deaths",
       x = "Dates", y = "Cumulative Deaths") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
