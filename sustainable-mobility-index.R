library(tidyverse)
library(openxlsx)

# The goal of the script is to implement the methodology of the Sustainable Mobility Index in Lithuania. 
# It will create an index for each municipality at a national level and within each class.  
# The index will be based on the following categories: 
# - Environmental Impact
# - Incentives and Policies 
# - Cycling 
# - Public Transport 
# - Walking 
# - Cars: Fossil Fuels 
# - Cars: Electric and Alternative Fuels 
# - Use of Space 
# We apply a min-max normalization to each variable. 
# Then we calculate the weighted average of the variables for each category. 
# Finally, we calculate the Sustainable Mobility Index as a weighted sum of the categories. 
lt_synthetic <- read.xlsx("lithuania_mobility_synthetic_60_classed_v2_for_software.xlsx") 

# In this code chunk, we will transform the data from wide to long format and assign the categories to each variable. 
# We will get the variable names after pivoting the data using pivot_longer and use those to assign the categories. 
# The categories are assigned based on the variable names to the following categories: 
# - Environmental Impact
# - Incentives and Policies
# - Cycling
# - Public Transport
# - Walking
# - Cars: Fossil Fuels
# - Cars: Electric and Alternative Fuels
# - Use of Space
lt_synthetic_long <- lt_synthetic %>% 
  pivot_longer(cols = -c(county, municipality, class), names_to = "variable", values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(sustainable_mobility_category = case_when(
    str_detect(variable, "co2") ~ "Environmental Impact", 
    str_detect(variable, "noise") ~ "Environmental Impact", 
    str_detect(variable, "nox") ~ "Environmental Impact", 
    str_detect(variable, "pm10") ~ "Environmental Impact", 
    str_detect(variable, "pm25") ~ "Environmental Impact", 
    str_detect(variable, "electric") ~ "Cars: Electric and Alternative Fuels", 
    str_detect(variable, "public.transport") ~ "Public Transport",  
    str_detect(variable, "public.transit") ~ "Public Transport",  
    str_detect(variable, "bus.services") ~ "Public Transport", 
    str_detect(variable, "daily.departures") ~ "Public Transport", 
    str_detect(variable, "monthly.PT.ticket") ~ "Public Transport", 
    str_detect(variable, "fines") ~ "Incentives and Policies", 
    str_detect(variable, "investment") ~ "Incentives and Policies",  
    str_detect(variable, "cycling") ~ "Cycling", 
    str_detect(variable, "cycle") ~ "Cycling", 
    str_detect(variable, "number_of_persons_fatally_or_seriously_injured_per_population") ~ "Cycling", 
    str_detect(variable, "bike") ~ "Cycling",  
    str_detect(variable, "zero_car") ~ "Cars: Fossil Fuels",
    str_detect(variable, "taxi") ~ "Cars: Fossil Fuels",
    str_detect(variable, "cars_by_type") ~ "Cars: Electric and Alternative Fuels", 
    str_detect(variable, "ev") ~ "Cars: Electric and Alternative Fuels",  
    str_detect(variable, "fossil_fuel_car") ~ "Cars: Fossil Fuels", 
    str_detect(variable, "by_car") ~ "Cars: Fossil Fuels", 
    str_detect(variable, "zone") ~ "Incentives and Policies", 
    str_detect(variable, "commitment") ~ "Incentives and Policies", 
    str_detect(variable, "walking") ~ "Walking",
    str_detect(variable, "pt_stop") ~ "Public Transport",  
    str_detect(variable, "wait_time") ~ "Public Transport", 
    str_detect(variable, "space") ~ "Use of Space",
    str_detect(variable, "traffic") ~ "Use of Space", 
    str_detect(variable, "parking") ~ "Use of Space", 
    str_detect(variable, "parked") ~ "Use of Space", 
    str_detect(variable, "street") ~ "Use of Space", 
    str_detect(variable, "sustainable_mobility_plan") ~ "Incentives and Policies", 
    str_detect(variable, "to_fine") ~ "Incentives and Policies", 
    str_detect(variable, "walk_more") ~ "Walking", 
    str_detect(variable, "pedestrian") ~ "Walking"
  )) %>% # now let's clean the variable names, 
  #making them into snake_case, which will require identifying the parts of the variable name that 
  #have multiple spaces between words and replacing them with a single underscore, then replacing any single spaces 
  # or periods with a single underscore, than stripping any other special characters, making them lowercase, and 
  # removing any underscores that are at the beginning or end of the variable name, 
  # and then finally making the full variable name lowercase. And removing underscores at the end of the variable name.
  mutate(variable = str_replace_all(variable, " ", "_")) %>% 
  mutate(variable = str_replace_all(variable, "\\.", "_")) %>% 
  mutate(variable = str_replace_all(variable, "[^a-zA-Z0-9_]", "_")) %>% 
  mutate(variable = str_replace_all(variable, "_+", "_")) %>% 
  mutate(variable = str_replace_all(variable, "_$", "")) %>% 
  mutate(variable = tolower(variable))

list_of_vars <- lt_synthetic_long %>%
    distinct(variable, sustainable_mobility_category)
# label by hand higher_is_better for each variable
#write.csv(list_of_vars, "list_of_vars.csv", row.names = FALSE)

higher_is_better <- read.csv("list_of_vars.csv")

lt_synthetic_long_final <- lt_synthetic_long %>%
    left_join(higher_is_better, by = c("variable"))

# We will now apply the min_max_norm function to each variable in the lt_synthetic_long dataframe.
lt_synthetic_long_minmax <- lt_synthetic_long_final %>% 
    group_by(variable, higher_is_better) %>%
    mutate(min_max_norm = if_else(higher_is_better, (value - min(value)) / (max(value) - min(value)), (max(value) - value) / (max(value) - min(value)))) %>%
    ungroup()

# We will now calculate the average of the min-max normalized values for each category for each municipality. 
# We will use the mean function to calculate the average. 
lt_synthetic_long_mean <- lt_synthetic_long_minmax %>% 
    group_by(municipality, sustainable_mobility_category) %>%
    summarise(average = mean(min_max_norm)) %>%
    ungroup()

# We will now calculate the Sustainable Mobility Index for each municipality. 
# The Sustainable Mobility Index will be the weighted sum of the average of the min-max normalized values for each category. 
# The weights are as follows: 
# - Environmental Impact: 0.05
# - Incentives and Policies: 0.05
# - Cycling: 0.2
# - Public Transport: 0.2
# - Walking: 0.2
# - Cars: Fossil Fuels: 0.2
# - Cars: Electric and Alternative Fuels: 0.10
# - Use of Space: 0.05
# We will use these weights the calculate the Sustainable Mobility Index for each municipality. 
# We will use the mutate function to calculate the Sustainable Mobility Index for each municipality. 
lt_synthetic_index <- lt_synthetic_long_mean %>%
    pivot_wider(names_from = sustainable_mobility_category, values_from = average) %>% 
    mutate(sustainable_mobility_index = 
    0.05 * `Environmental Impact` + 
    0.05 * `Incentives and Policies` + 
    0.2 * Cycling + 
    0.15 * `Public Transport` + 
    0.2 * Walking + 
    0.2 * `Cars: Fossil Fuels` + 
    0.1 * `Cars: Electric and Alternative Fuels` + 
    0.05 * `Use of Space`)

# Now, we will create a graph showing the top 10 municipalities with the highest Sustainable Mobility Index.  
# The graph is a bar chart with the Sustainable Mobility Index on the y-axis and the Municipality on the x-axis. 
# The graph has a horizontal line that shows the average Sustainable Mobility Index among all the municipalities. 
# There is also a label on the graph that shows the value of the average Sustainable Mobility Index next to the line. 
# The bars are outlined in grey and the fill of the bars is a uniform light blue.
# The red horizontal line is the average and is labelled as such.

lt_synthetic_index %>%
    arrange(desc(sustainable_mobility_index)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(municipality, sustainable_mobility_index), y = sustainable_mobility_index)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "grey") + 
    geom_hline(yintercept = mean(lt_synthetic_index$sustainable_mobility_index), color = "red") + #label the line as the average 
    geom_text(aes(x = 0, y = mean(lt_synthetic_index$sustainable_mobility_index), label = "Average Index Score"), color = "red", hjust = -.1, vjust = 1.5) +
    labs(title = "Top 5 Municipalities with the Highest Sustainable Mobility Index",
         x = "Municipality",
         y = "Sustainable Mobility Index") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylim(0, 1) 

# Now, we will create a graph showing the top 5 municipalities with the lowest Sustainable Mobility Index.  

lt_synthetic_index %>%
    arrange(sustainable_mobility_index) %>%
    head(5) %>%
    ggplot(aes(x = reorder(municipality, -sustainable_mobility_index), y = sustainable_mobility_index)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "grey") + 
    geom_hline(yintercept = mean(lt_synthetic_index$sustainable_mobility_index), color = "red") + #label the line as the average 
    geom_text(aes(x = 0, y = mean(lt_synthetic_index$sustainable_mobility_index), label = "Average Index Score"), color = "red", hjust = -.1, vjust = 1.5) +
    labs(title = "Bottom 5 Municipalities with the Lowest Sustainable Mobility Index",
         x = "Municipality",
         y = "Sustainable Mobility Index") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylim(0, 1) 

# Now we will create a graph showing the top 5 municipalities with the highest Cycling Index. 

lt_synthetic_index %>%
    arrange(desc(Cycling)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(municipality, Cycling), y = Cycling)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "grey") +
    labs(title = "Top 5 Municipalities with the Highest Cycling Index",
         x = "Municipality",
         y = "Cycling Index") +
    geom_hline(yintercept = mean(lt_synthetic_index$Cycling), color = "red") + #label the line as the average 
    geom_text(aes(x = 0, y = mean(lt_synthetic_index$Cycling), label = "Average Index Score"), color = "red", hjust = -.1, vjust = 1.5) +
   
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylim(0, 1) 

########################################################
# Now we will apply the min-max methodology, grouping by class. 
# We will then calculate the average of the min-max normalized values for each category for each class. 
# We will then calculate the Sustainable Mobility Index for each class. 
# We will then create a graph showing the top 5 classes with the highest Sustainable Mobility Index.
# We will now apply the min_max_norm function to each variable in the lt_synthetic_long dataframe.
class_lt_synthetic_long_minmax <- lt_synthetic_long_final %>% 
    group_by(class, variable, higher_is_better) %>%
    mutate(min_max_norm = if_else(higher_is_better, (value - min(value)) / (max(value) - min(value)), (max(value) - value) / (max(value) - min(value)))) %>%
    ungroup() 

# We will now calculate the average of the min-max normalized values for each category for each class. 
# We will use the mean function to calculate the average. 
class_lt_synthetic_long_mean <- class_lt_synthetic_long_minmax %>% 
    group_by(class, sustainable_mobility_category) %>%
    summarise(average = mean(min_max_norm)) %>%
    ungroup() 

# We will now calculate the Sustainable Mobility Index for each class. 
# The Sustainable Mobility Index will be the weighted sum of the average of the min-max normalized values for each category. 
# The weights are as follows: 
# - Environmental Impact: 0.05
# - Incentives and Policies: 0.05
# - Cycling: 0.2
# - Public Transport: 0.2
# - Walking: 0.2
# - Cars: Fossil Fuels: 0.2
# - Cars: Electric and Alternative Fuels: 0.10
# - Use of Space: 0.05
class_lt_synthetic_index <- class_lt_synthetic_long_mean %>%
    pivot_wider(names_from = sustainable_mobility_category, values_from = average) %>% 
    mutate(sustainable_mobility_index = 
    0.05 * `Environmental Impact` + 
    0.05 * `Incentives and Policies` + 
    0.2 * Cycling + 
    0.15 * `Public Transport` + 
    0.2 * Walking + 
    0.2 * `Cars: Fossil Fuels` + 
    0.1 * `Cars: Electric and Alternative Fuels` + 
    0.05 * `Use of Space`)

# Now we will create a bar graph showing the sustainable mobility index for each class. 
# The graph is a bar chart with the Sustainable Mobility Index on the y-axis and the Class on the x-axis. 
# The graph has a horizontal line that shows the average Sustainable Mobility Index among all the classes. 
# There is also a label on the graph that shows the value of the average Sustainable Mobility Index next to the line. 
# The bars are outlined in grey and the fill of the bars is a uniform light blue.

class_lt_synthetic_index %>%
    arrange(desc(sustainable_mobility_index)) %>%
    head(5) %>%
    ggplot(aes(x = reorder(class, sustainable_mobility_index), y = sustainable_mobility_index)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "grey") +
    #geom_hline(yintercept = mean(class_lt_synthetic_index$sustainable_mobility_index), color = "red") +
   # geom_text(aes(x = 0, y = mean(class_lt_synthetic_index$sustainable_mobility_index), label = "Average Index Score"), color = "red", hjust = -.1, vjust = 1.5) +
    labs(title = "Sustainable Mobility Index by Class",
         x = "Class",
         y = "Sustainable Mobility Index") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylim(0, 1)

# Now we will create a graph showing the the subindexes by class. 
# The graph is a grouped bar chart with the subindex value on the y-axis and the subindex category on the x-axis. 
# The grouped bars are grouped by class. # The grouping of the bars is ordered by class in this order: Metropolitan, Suburban/Mid-sized, Rural, Resort Town.   
# The bars are outlined in grey and the fill of the bars is a uniform light blue.
# The legend and bars are ordered by class in this order: Metropolitan, Suburban/Mid-sized, Rural, Resort Town.
# There is twice the amount of white space between each set of grouped bars

class_lt_synthetic_index %>%
    pivot_longer(cols = -class, names_to = "sustainable_mobility_category", values_to = "average") %>%
    mutate(sustainable_mobility_category = if_else(sustainable_mobility_category == "sustainable_mobility_index", 
    "OVERALL SUSTAINABLE MOBILITY INDEX SCORE", sustainable_mobility_category)) %>%
    ggplot(aes(x = sustainable_mobility_category, y = average, fill = class)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.5, color = "darkgrey") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylim(0, .75) +
    scale_fill_manual(values = c("Metropolitan" = "orange", "Suburban/Mid-sized" = "lightblue", "Rural" = "darkred", "Resort Town" = "#bfe4bf")) + 
    coord_flip() + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) + 
    labs(title = "Sustainable Mobility Sub-Index by Class",
        x ="", y="")
