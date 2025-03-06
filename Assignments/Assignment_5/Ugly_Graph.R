# Ugly Graph Contest ####

library(gapminder)
library(ggimage)
library(tidyverse)
library(gganimate)

## take a look of gapminder data, make a graph, and save to local
View(gapminder)

gap <- gapminder
dim(gapminder)

#Animated
gap %>% 
  group_by(country) %>% 
  ggplot(aes(x = country,
             y = lifeExp))+
  geom_image(
    data = tibble(country = "Haiti", lifeExp = 35),
    aes(image = "cat_party.png"),
    size = 0.9,
    alpha = 0.1) +  
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Gapminder: Country Life Expectancy")+
  theme(plot.title = element_text(hjust = 0.5))+
  transition_reveal(lifeExp)

anim_save("Ugly_Graph.gif")

#Just Picture
gap %>% 
  group_by(country) %>% 
  ggplot(aes(x = country,
             y = lifeExp))+
  geom_image(
    data = tibble(country = "Haiti", lifeExp = 50),
    aes(image = "cat_party.png"),
    size = 1.0,
    alpha = 0.1) +  
  geom_point()+
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Gapminder: Country Life Expectancy")+
  theme(plot.title = element_text(hjust = 0.5))
