data <- read.csv("deaths.csv")

#install.packages("dplyr")
library(dplyr)

hispanic_population <- data %>%
  filter(ethnicity=="Hispanic/ Latino") 

 hispanic_deathcount <- nrow(hispanic_population)

black_population <- data %>%
  filter(race=="African-American/ Black")

Black_deathcount <- nrow(black_population)

total_deaths <- nrow(data)
  
proportion_BlackHispanic_Deaths <- (Black_deathcount + hispanic_deathcount)/total_deaths

df <- read.csv("deaths.csv")

# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)

# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)

# Read the data
df <- read.csv("deaths.csv")

library(dplyr)

summary_table <- data %>% 
  group_by(ethnicity) %>% 
  summarise(
    count = n(),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    ci_lower = mean - qt(0.975, df=n()-1)*sd/sqrt(n()),
    ci_upper = mean + qt(0.975, df=n()-1)*sd/sqrt(n())
  )

#write.csv(summary_table, file ="summary_table.csv" , row.names = FALSE)

df$indicator_chronic_condition_Yes <- as.integer(df$chronic_condition == "Yes")
 mean_indicator <- mean(df$indicator_chronic_condition_Yes)
 
 summary_indicator <- data.frame(
   Variable = c("Indicator Chronic Condition (Yes)", "Mean Indicator"),
   Value = c(sum(df$indicator_chronic_condition_Yes), mean_indicator)
 )

 write.csv(summary_indicator, file ="summary_indicator.csv" , row.names = FALSE)
 
 summary_table_gender <- data %>%
   group_by(sex, chronic_condition) %>%
   summarise(count = n())
 
#write.csv(summary_table_gender, file ="summary_table_gender.csv" , row.names = FALSE)
 
 grouped_bar_chart <- ggplot(data, aes(x = sex, fill = chronic_condition)) +
   geom_bar(position = "dodge") +
   ggtitle('Distribution of Chronic Conditions by Gender') +
   xlab('gender') +
   ylab('Count')
 grouped_bar_chart
 
#ggsave("grouped_bar_chart.png", plot = grouped_bar_chart, width = 8, height = 6)
 
 library(ggplot2)
 library(dplyr)
 
 # Assuming you have a data frame named 'data' with your information
 
 # Aggregate the data for 'sex' variable
 aggregated_data_sex <- data %>%
   group_by(sex, chronic_condition) %>%
   summarise(count = n(), .groups = 'drop')
 
 # Scatter Plot
 scatter_plot_sex <- ggplot(aggregated_data_sex, aes(x = sex, y = count, color = chronic_condition)) +
   geom_point(position = position_jitter(width = 0.1)) +
   ggtitle('Scatter Plot of Count by Gender and Chronic Condition') +
   xlab('Gender') +
   ylab('Count')
 
 # Display the scatter plot
 print(scatter_plot_sex)
 ggsave(" scatter_plot_sex.png", plot =  scatter_plot_sex, width = 8, height = 6)
 
 data$age <- as.numeric(data$age)
 
 library(ggplot2)
 
 # Ensure 'age' is numeric
 df$age <- as.numeric(data$age)
 
 # Create a histogram for the 'age' column
Distribution_Ages <-  ggplot(df, aes(x = age)) +
   geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
   labs(title = "Distribution of Age",
        x = "Age",
        y = "Frequency") +
   theme_minimal()

#ggsave("Distribution_Ages.png", plot = Distribution_Ages, width = 8, height = 6)
 
 
 
 



 
 

 


 

 
 


