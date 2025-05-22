library(tidyverse)
library(openxlsx)

lt_synthetic <- read.xlsx("Documents/git/Lithuania/KurkLT/lithuania_mobility_synthetic_60_classed_v2 (1).xlsx")

test <- lt_synthetic %>% 
  select(County, Municipality, Class, PM10_excess = PM10.amount.in.excess.of.EU.standards, bike_satisfaction = `General.satisfaction.with.bike.parking.(%).or.other.bike.infrastructure `, 
         pt_satisfaction = `General.satisfaction.with.public.transportation  `)

bike_gg <- test %>% 
  ggplot(aes(x = bike_satisfaction)) + 
  geom_density() + 
  xlim(0,8000)+ 
  labs(y="Density", x="Bike Infrastructure and Safety Satisfaction") + 
  theme_linedraw()
bike_gg

pm10_gg <- test %>% 
  ggplot(aes(x = PM10_excess)) + 
  geom_density() + 
  xlim(0,80) + 
  labs(y="Density", x="PM10 in Excess of EU Standards") + 
  theme_linedraw() #+ 
 # theme(axis.text.y = element_blank())
pm10_gg

pt_gg <- test %>% 
  ggplot(aes(x = pt_satisfaction)) + 
  geom_density() + 
  xlim(0,8000)+ 
  labs(y="Density", x="Satisfaction with Public Transport") + 
  theme_linedraw()
pt_gg

min_pm10 <- min(test$PM10_excess)
max_pm10 <- max(test$PM10_excess)
min_max_pm10 <- test %>% 
  mutate(standardized_score = 100*((max(PM10_excess)-PM10_excess)/(max(PM10_excess)-min(PM10_excess))))

standardized_pm10_gg <- min_max_pm10 %>% 
  ggplot(aes(x = standardized_score)) + 
  geom_density()
standardized_pm10_gg

min_max_bike <- test %>% 
  mutate(standardized_score = 100*((bike_satisfaction - min(bike_satisfaction))/(max(bike_satisfaction) - min(bike_satisfaction))))

standardized_bike_gg <- min_max_bike %>% 
  ggplot(aes(x = standardized_score)) + 
  geom_density()
standardized_bike_gg

######################################## 
# let's try z scores 
########################################
zscores <- test %>% 
  mutate(across(
    where(is.numeric) & !any_of(c("Municipality", "Class")),
    ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
    .names = "{.col}_z"
  ))

zscore_pm10 <- zscores %>% 
  ggplot(aes(x = PM10_excess_z)) + 
  geom_density()
zscore_pm10

quantile(min_max_bike$standardized_score, probs = seq(0,1,.1))
quantile(min_max_pm10$standardized_score, probs = seq(0,1,.1))





