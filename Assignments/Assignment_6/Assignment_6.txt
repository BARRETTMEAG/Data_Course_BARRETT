#Assignment_6

library(tidyverse)
library(gganimate)

dat <- read_csv("Data/BioLog_Plate_Data.csv")
View(dat)

#combining Hr
tidy_dat <- dat %>%
  pivot_longer(cols = starts_with("Hr_"),  
               names_to = "Time",
               values_to = "Absorbance") %>%
  mutate(Time = as.numeric(str_extract(Time, "\\d+")))

#Separating Sample_ID into Soil or Water
tidy_dat <- tidy_dat %>%
  mutate(Sample_Type = ifelse(str_detect(`Sample ID`, "Soil"), "Soil", "Water"))

View(tidy_dat)

#Filtering 0.1
tidy_dat_filtered <- tidy_dat %>%
  filter(Dilution == 0.1)

View(tidy_dat_filtered)

#Creating Graphs for substrates
tidy_dat_filtered %>%
  ggplot(aes(x = Time, y = Absorbance, color = Sample_Type)) +
  geom_smooth(method = "loess", se = FALSE) +  
  labs(title = "Just Dilution 0.1", x = "Time", y = "Absorbance", color = 'Type') +
  theme_minimal() +
  facet_wrap(~ Substrate) +
  scale_x_continuous(limits = c(0, 150), breaks = c(50, 100, 150)) +
  theme(legend.position = "right")+
  theme(
    legend.position = "right",
    strip.text = element_text(size = 6),  
    plot.title = element_text(size = 8)
  )


#changing name for appearance sake
itaconic_data <- tidy_dat %>%
  rename(Sample_ID = `Sample ID`)

#filtering Itaconic acid
itaconic_data %>%
  filter(Substrate == 'Itaconic Acid') %>%
  group_by(Sample_ID, Dilution,Time) %>%
  summarize(Mean_Absorbance = mean(Absorbance, na.rm = TRUE))

View(itaconic_data)

#filtering further Dilutions 0.001, 0.01, 0.1
itaconic_data_filtered <- itaconic_data %>%
  filter(Substrate == "Itaconic Acid", Dilution %in% c(0.001, 0.01, 0.1))


View(itaconic_data_filtered)

#filtering 0.001 from itaconic data
dat_0_001 <- itaconic_data_filtered %>%
  filter(Dilution == 0.001) 
View(dat_0_001)

#filtering 0.01 from itaconic data
dat_0_01 <- itaconic_data_filtered %>%
  filter(Dilution == 0.01) 

#filtering 0.1 from itaconic data
dat_0_1 <- itaconic_data_filtered %>%
  filter(Dilution == 0.1) 

#combining the data
dat_combined <- bind_rows(
  dat_0_001 %>% mutate(Dilution = "0.001"),
  dat_0_01 %>% mutate(Dilution = "0.01"),
  dat_0_1 %>% mutate(Dilution = "0.1")
)

#animating the data
anim_plot <- dat_combined %>%
  ggplot(aes(x = Time, y = Absorbance, color = Sample_ID)) +
  geom_line() + 
  labs(
    #title = "Itaconic Acid Absorbance",
    x = "Time",
    y = "Mean_absorbance",
    color = "Sample ID") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 2.5)) +
  scale_x_continuous(limits = c(0, 150), breaks = c(50, 100, 150)) +
  facet_wrap(~ Dilution, scales = "free_y") +  
  transition_reveal(Time) + 
  ease_aes('linear') +
  theme(
    legend.position = "right", 
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    axis.title.x = element_text(size = 10), 
    axis.title.y = element_text(size = 10)
  )
anim_plot

