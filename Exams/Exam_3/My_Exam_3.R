#Exam 3
# Meaghan Barrett

# Load required packages
library(tidyverse)
library(easystats)
library(tidyr)
library(knitr)
library(broom)
library(purrr)


# 1. Load and clean FacultySalaries_1995.csv file and Re-create the graph below ####
faculty_data <- read.csv("FacultySalaries_1995.csv")

# Create the Rank column based on salary averages
faculty_data_clean <- faculty_data %>%
  gather(key = "Rank", value = "Salary", 
         AvgAssistProfSalary, AvgAssocProfSalary, AvgFullProfSalary) %>%
  mutate(Rank = recode(Rank,
                       "AvgAssistProfSalary" = "Assistant",
                       "AvgAssocProfSalary" = "Associate",
                       "AvgFullProfSalary" = "Full")) %>%
  filter(!is.na(Salary))  

# Convert Rank to a factor
faculty_data_clean$Rank <- factor(faculty_data_clean$Rank, 
                                  levels = c("Assistant", "Associate", "Full"))

# Check the cleaned data
View(faculty_data_clean)

# filtering
faculty_data_clean_filtered <- faculty_data_clean %>%
  filter(Tier %in% c("I", "IIA", "IIB"))

# Create the facet_wrap boxplot
faculty_data_clean_filtered %>% 
  ggplot(aes(x = Rank, y = Salary, fill = Rank)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(200, 1000)) + 
  labs(x = "Rank", 
       y = "Salary") +
  facet_wrap(~Tier, scales = "free_y") +  
  theme_minimal()+
  scale_x_discrete(labels = c("Assistant" = "Assist", 
                              "Associate" = "Assoc", 
                              "Full" = "Full")) +
  scale_fill_discrete(labels = c("Assistant" = "Assist", 
                                 "Associate" = "Assoc", 
                                 "Full" = "Full")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. Build an ANOVA model and display the summary output in your report.####

# Fit the ANOVA model
anova_model <- aov(Salary ~ State + Tier + Rank, data = faculty_data_clean_filtered)

# Display the summary of the ANOVA model
summary(anova_model)

# 3. The rest of the test uses another data set. The “Juniper_Oils.csv” data. ####
# Get it loaded and take a look. Then tidy it! (show the code used for tidying in your report)####

juniper_data <- read.csv("Juniper_Oils.csv")

names(juniper_data)

names(juniper_data) <- gsub("\\.", "-", names(juniper_data))
names(juniper_data) <- gsub("compound-1", "compound 1", names(juniper_data))
names(juniper_data) <- gsub("compound-2", "compound 2", names(juniper_data))

View(juniper_data)

# Tidy the data by pivoting the chemical columns into long format
tidy_data <- juniper_data %>%
  pivot_longer(cols = c("alpha-pinene", "para-cymene", "alpha-terpineol", "cedr-9-ene", 
                        "alpha-cedrene", "beta-cedrene", "cis-thujopsene", "alpha-himachalene", 
                        "beta-chamigrene", "cuparene", "compound 1", "alpha-chamigrene", "widdrol", 
                        "cedrol", "beta-acorenol", "alpha-acorenol", "gamma-eudesmol", "beta-eudesmol", 
                        "alpha-eudesmol", "cedr-8-en-13-ol", "cedr-8-en-15-ol", "compound 2", "thujopsenal"),
               names_to = "ChemicalID", values_to = "Concentration")


# Check the result
View(juniper_tidy)

# 4. Make me a graph of the following: ####
tidy_data %>% 
  ggplot(aes(x = YearsSinceBurn, y = Concentration)) +
  geom_smooth(aes(group = ChemicalID)) +
  facet_wrap(~ChemicalID, scales = "free") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(5, 20, by = 5)) + 
  theme_minimal() +
  labs(x = "YearsSinceBurn", 
       y = "Concentration")+
  scale_y_continuous(breaks = function(x) {  # Define custom breaks for specific chemicals
    if (any(grepl("compound", unique(tidy_data$ChemicalID)))) {
      return(seq(0, 40, by = 10)) 
    }  elif {
      return(seq(0, 30, by = 10))
    } elif {
      return(seq(0, 20, by = 5))   
    } elif {
      return(seq(0, 10, by = 2.5))
    } 
  })


# 5. Use a generalized linear model to find which chemicals show ####
# concentrations that are significantly (significant, as in P < 0.05) affected by “Years Since Burn”.###

# Step 1: Run GLM for each chemical separately
lm_results <- tidy_data %>%
  group_by(ChemicalID) %>%
  nest() %>%
  mutate(
    model = map(data, ~ glm(Concentration ~ YearsSinceBurn, data = .x)),
    tidy_output = map(model, ~ broom::tidy(.x)) 
  ) %>%
  select(ChemicalID, tidy_output) %>%
  unnest(tidy_output)

# Step 2: Filter to only significant terms (p < 0.05)
significant_terms <- lm_results %>%
  filter(p.value < 0.05)

# Step 3: View the results
print(significant_terms)
