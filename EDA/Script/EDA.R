
#### Libraries ####
library(tidyverse)


# Load the data 
marine_engine_data <- read_csv("Data/marine_engine_data.csv")

# Convert strings into factors
marine_data <- marine_engine_data %>% 
  select(-engine_id) %>%
  mutate(across(where(is.character), as.factor))

## Numerical distributions 
marine_data %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value")%>%
  ggplot(aes(x = Value)) +
  geom_boxplot() +
  facet_wrap(~Feature,scale = "free_x")+
  theme_minimal()+
  labs(
    title = "Boxplot",
    annotate(geom = "text",x = 1,y = 2),caption = "Many outliers in exhaust_temp and fuel_consumption_per_hour")+
  theme(
    title = element_text(size = 15))

## Categorical Features Distribution 
marine_data %>%
  select_if(is.factor) %>%
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value")%>%
  mutate(
    Value = fct_infreq(Value),
    Value_colored = ifelse(Value == "Oil Leakage", "lightgrey", "red")) %>% 
  ggplot(aes(x = Value,fill = Value_colored))+
  geom_bar()+
  scale_fill_manual(values = c("darkred","lightgrey"))+
  facet_wrap(~Feature,scale = "free")+
  coord_flip()+
  theme_minimal()+
  labs(
    title = "Features Bar Plot",
    y = "Count",
    x = " ",
    caption = "Most common failure : Oil Leakage")+
  theme(
    legend.position = " ",
    title = element_text(size = 15),
    strip.text.x = element_text(size = 10),
    strip.text = element_text(size = 10)
  )
  
  







