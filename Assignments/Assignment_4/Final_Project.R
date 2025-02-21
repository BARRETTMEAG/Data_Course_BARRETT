#Final_Project
library(tidyverse)

gator <- read.csv("Assignments/Assignment_4/fatal_alligator_attacks_US.csv")
View(gator)

gator %>% 
  group_by(Date) %>% 
  ggplot(aes(x = Date, y = Age)) +
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

g_wolves <- read.csv("Assignments/Assignment_4/global_wolves.csv")
View(g_wolves)

g_wolves %>% 
  group_by(Type.of.attack) %>% 
  ggplot(aes(x = Type.of.attack)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shark_2 <- read.csv("Assignments/Assignment_4/shark_attacks.csv")
View(shark_2)

shark_2 %>% 
  group_by(Area) %>% 
  ggplot(aes(x = Type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

shark_1 <- read.csv("Assignments/Assignment_4/Sharks/attacks.csv")
View(shark_1)

shark_1 %>% 
  group_by(Country) %>% 
  ggplot(aes(x = Type)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#shark_1.5 <- read.csv("Assignments/Assignment_4/Sharks/list_coor_australia.csv")
#View(shark_1.5)
