
#### Libraries ####
library(tidyverse)
library(rstatix)
library(corrplot)

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

## Correlation matrix
dev.new(width = 10, height = 10)  
corrplot(cor(marine_data %>% select_if(is.numeric)), 
         method = "color", 
         order = "hclust", 
         tl.srt = 45, 
         outline = TRUE, 
         addCoef.col = "black")


#### Hypothesis Testing
# Sig level = 0.05 

## Kruskal-Wallis Test (Non-Parametric ANOVA)

# Check is there a dif between oil distribution facet by failure mode
kruskal.test(oil_pressure ~ failure_mode,data = marine_data) # p-value = 0.2831
 # Check the effect size 
kruskal_effsize(data = marine_data,formula = oil_pressure ~ failure_mode) # effsize = 0.000155


# Check is there a dif between running_period distribution facet by failure mode 
kruskal.test(running_period ~ failure_mode,data = marine_data) # p-value = 0.8317
# Check the effect size 
kruskal_effsize(data = marine_data,formula = running_period ~ failure_mode) # effsize =-0.000409


# Check if rpm distribution dif between failure modes 
kruskal.test(rpm ~ failure_mode,data = marine_data) # p-value = 0.192
# Check the effect size 
kruskal_effsize(data = marine_data,formula = rpm ~ failure_mode) # effsize = 0.000334


# Check if fuel_consumption dif between failure modes 
kruskal.test(fuel_consumption ~ failure_mode,data = marine_data) # p-value = 0.8757
# Check the effect size 
kruskal_effsize(data = marine_data,formula = fuel_consumption ~ failure_mode) # effsize = -0.000445


# Check if the exhaust_temp dif between failure modes 
kruskal.test(exhaust_temp ~ failure_mode,data = marine_data) # p-value = 0.4094
# Check the effect size 
kruskal_effsize(data = marine_data,exhaust_temp ~ failure_mode) # effsize = -0.0000217


# Check if the engine_temp is dif between failure modes
kruskal.test(engine_temp ~ failure_mode,data = marine_data)  # p-value = 0.2566
# Check the effect size 
kruskal_effsize(data = marine_data,engine_temp ~ failure_mode) # effsize = 0.000201


# Check if vibration level dist is dif between failure modes
kruskal.test(vibration_level ~ failure_mode ,data = marine_data) # p-value = 0.2233
# Check the effect size 
kruskal_effsize(data = marine_data,vibration_level ~ failure_mode) # effsize =0.000266


## Kolmogorov-Smirnov (KS) Test

# Oil Pressure dif for Oil_Leakage and No Failure
ks.test(
  marine_data$oil_pressure[marine_data$failure_mode == "Oil Leakage"],
  marine_data$oil_pressure[marine_data$failure_mode == "No Failure"]
  ) # p-value = 0.3119 

# 
marine_data %>%
  filter(failure_mode == c("No Failure","Oil Leakage"))%>%
ggplot(aes(x = oil_pressure,color = failure_mode))+
  stat_ecdf()









