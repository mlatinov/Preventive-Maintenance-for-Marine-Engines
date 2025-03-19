
#### Libraries ####
library(tidyverse)
library(rstatix)
library(corrplot)
library(randomForest)

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

# Plot the oil pressure by failure type 
marine_data%>%
  group_by(failure_mode)%>%
  summarise(
    median_oil_p = median(oil_pressure))%>%
ggplot(aes(x = median_oil_p, y = fct_reorder(failure_mode, median_oil_p, .fun = sum), fill = failure_mode)) +
   geom_col() +
   scale_fill_viridis_d(option = "plasma")+
    theme_minimal()+
    labs(
        Fill = "Failure Mode",
       title = "Failure Types and Oil Pressure",
        caption = "Kruskal Test : p = 0.2831, Effect size = 0.000155",
        x = "Oil Pressure",
        y = "Failure Type")+
    theme(
        plot.caption = element_text(hjust = 0.1,vjust = 152),
        title = element_text(size = 13),
        legend.position = "")

# Kolmogorov-Smirnov (KS) Test

# Oil Pressure dif for Oil_Leakage and No Failure
ks.test(
  marine_data$oil_pressure[marine_data$failure_mode == "Oil Leakage"],
  marine_data$oil_pressure[marine_data$failure_mode == "No Failure"]
) # p-value = 0.3119 

# Wilcoxon rank sum test 
wilcox.test(marine_data$oil_pressure[marine_data$failure_mode == "Oil Leakage"],
            marine_data$oil_pressure[marine_data$failure_mode == "No Failure"]
            ) # p-value = 0.171

# Check is there a dif between running_period distribution facet by failure mode 
kruskal.test(running_period ~ failure_mode,data = marine_data) # p-value = 0.8317
# Check the effect size 
kruskal_effsize(data = marine_data,formula = running_period ~ failure_mode) # effsize =-0.000409

# Plot 
marine_data %>%
  group_by(failure_mode)%>%
  summarise(
    median_run = median(running_period))%>%
ggplot(aes(x = median_run,y = fct_reorder(failure_mode,median_run,.fun = sum),fill = failure_mode))+
  geom_col()+
  scale_fill_viridis_d(option = "plasma")+
  theme_minimal()


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

## Chi-Square test

# Failure modes and maintenance status
chisq_test(table(marine_data$failure_mode,marine_data$maintenance_status)) # ns 

# Failure modes and engine type 
chisq_test(table(marine_data$failure_mode,marine_data$engine_type)) # ns 

# Failure modes and fuel_type
chisq_test(table(marine_data$failure_mode,marine_data$fuel_type)) # ns

# Failure modes and 
chisq_test(table(marine_data$failure_mode,marine_data$manufacturer)) # ns

# Simple model for Feature Importance
ranger_imp <- randomForest(
  formula = failure_mode ~ .,
  data = marine_data,
  importance = TRUE
  )
dev.new(width = 10, height = 10)  
varImpPlot(ranger_imp,main = "Basic Random Forest Importance",sort = TRUE)

#### Collapse the levels of failure_mode to failure or Not and Try again

marine_data_b <- marine_data %>%
  mutate(
    failure_mode_b = as.factor(ifelse(test = failure_mode == "No Failure" ,"No Failure" ,"Failure"))
    )


## Stat Sig Function 
stats <- function(df, target, col){
  
  # Check if col is numeric and target is a factor
  if (is.numeric(df[[col]]) && is.factor(df[[target]])) {
    
    if (length(levels(df[[target]])) >= 3) {
      
      # Kruskal-Wallis Test
      kruskal_test <- kruskal.test(df[[col]] ~ df[[target]], data = df)
      
      # Kruskal-Wallis Effect size 
      kruskal_effect <- kruskal_effsize(df[[col]] ~ df[[target]], data = df)
      
      # Return the results
      return(list(
        kruskal_test = kruskal_test,
        kruskal_effect = kruskal_effect
      ))
      
    } else {
      
      # Wilcoxon rank-sum test for two levels
      wilcox_test <- wilcox.test(df[[col]] ~ df[[target]], data = df)
      
      # Return the result
      return(list(
        wilcox_test = wilcox_test
      ))
    }
    
  }
  
  # Check if both col and target are factors
  if (is.factor(df[[col]]) && is.factor(df[[target]])) {
    
    # Chi-Square test 
    chisq_test <- chisq.test(table(df[[col]], df[[target]]))
    
    # Return the result
    return(list(
      chisq_test = chisq_test
    ))
    
  }
  # If the conditions are not met 
  return("Conditions not met for statistical test.")
}

# Test failure mode and engine load 
stats(df = marine_data_b,target ="failure_mode_b",col = "engine_load") 
# Wilcoxon rank sum test p-value = 0.04963 *

# Plot Failure mode and engine load
ggplot(data = marine_data_b,aes(x = engine_load,fill = failure_mode_b))+
  geom_boxplot()+
  coord_flip()+
  scale_fill_viridis_d(option = "plasma")+
  theme_minimal()+
  labs(
    title = "Failure and Engine Load",
    x = "Engine Load",
    subtitle = "Wilcoxon rank sum test p-value = 0.04963 *",
    fill = "Failure Mode")+
  theme(
    title = element_text(size = 13,face = "bold"))

stats(df = marine_data_b,target = "failure_mode_b",col = "fuel_consumption")
# Wilcoxon rank sum test  p-value = 0.7972

stats(df = marine_data_b,target = "failure_mode_b",col = "running_period")
# Wilcoxon rank sum test  p-value = 0.4496

stats(df = marine_data_b,target = "failure_mode_b",col = "manufacturer")
# Pearson's Chi-squared test  p-value = 0.9646

stats(df= marine_data_b,target = "failure_mode_b",col =  "engine_temp" )
# Wilcoxon rank sum test p-value = 0.07643


# Test failure mode and Vibration Level
stats(df = marine_data_b,target = "failure_mode_b",col = "vibration_level")
# Wilcoxon rank sum test p-value = 0.04349 *

# Plot Failure mode and Vibration Level 
ggplot(data = marine_data_b,aes(x = vibration_level,fill = failure_mode_b))+
  geom_boxplot()+
  coord_flip()+
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()+
  labs(
    title = "Failure mode and Vibration Level",
    x = "Vibration Level",
    subtitle = " Wilcoxon rank sum test p-value = 0.04349 *",
    fill = "Failure Mode")+
  theme(
    title = element_text(size = 12,face = "bold")
  )

stats(df = marine_data_b,target = "failure_mode_b", col = "coolant_temp")
# Wilcoxon rank sum test p-value = 0.0626

stats(df= marine_data_b,target = "failure_mode_b",col = "fuel_consumption_per_hour")
# Wilcoxon rank sum test  p-value = 0.3721

stats(df = marine_data_b,target = "failure_mode_b",col = "engine_type")
# Pearson's Chi-squared test p-value = 0.824

stats(df = marine_data_b,target = "failure_mode_b",col = "oil_pressure")
# Wilcoxon rank sum test p-value = 0.06175

stats(df = marine_data_b,target = "failure_mode_b",col = "rpm")
# Wilcoxon rank sum test p-value = 0.6468

stats(df = marine_data_b,target = "failure_mode_b", col = "exhaust_temp")
# Wilcoxon rank sum test  p-value = 0.2747

stats(df = marine_data_b,target = "failure_mode_b",col = "maintenance_status")
# Pearson's Chi-squared test   p-value = 0.6815

stats(df = marine_data_b,target = "failure_mode_b",col = "fuel_type")
# Pearson's Chi-squared test   p-value = 0.5548

