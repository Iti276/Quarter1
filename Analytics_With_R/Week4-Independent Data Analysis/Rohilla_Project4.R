####Name-Iti Rohilla###Date-17/10/2023####Class-Intro to Analytics-By Roy Wada-----
#install.packages("dplyr")
#install.packages("readr")
#install.packages("highcharter")
library(readr)
data <- read_csv("drugs.csv")
# Load the required libraries
library(readr)
library(dplyr)
library(ggplot2)
# Read the CSV file
#data <- read_csv("drugs.csv")
#cleaning the data
install.packages("janitor")


library(janitor)
data <- clean_names(data)
data

#names(data)

# Visualization 1 ####
library(highcharter)
# Filter the data for the specified years (2002, 2003, 2004, and 2005)
years_of_interest <- c(2002, 2003, 2004, 2005)
filtered_data <- data %>%
  filter(year %in% years_of_interest)
alcohol_cols <- c("year", 
                  "rates_alcohol_use_disorder_past_year_12_17",
                  "rates_alcohol_use_disorder_past_year_18_25",
                  "rates_alcohol_use_disorder_past_year_26")

filtered_data <- filtered_data %>% select(all_of(alcohol_cols))

# Group by year and calculate the average rate for each age group
avg_alcohol_rates <- filtered_data %>%
  group_by(year) %>%
  summarise(Avg_Rate_12_17 = mean(`rates_alcohol_use_disorder_past_year_12_17`, na.rm = TRUE),
            Avg_Rate_18_25 = mean(`rates_alcohol_use_disorder_past_year_18_25`, na.rm = TRUE),
            Avg_Rate_26_plus = mean(`rates_alcohol_use_disorder_past_year_26`, na.rm = TRUE))
# Print the resulting data frame with average rates
avg_alcohol_rates
# Create the multi-bar bar chart
HighchartGraph <- highchart() %>%
  hc_chart(type = 'multiBarChart') %>%
  hc_xAxis(categories = avg_alcohol_rates$year) %>%
  hc_add_series(name = 'Avg_Rate_12_17', data = avg_alcohol_rates$Avg_Rate_12_17, type = 'column') %>%
  hc_add_series(name = 'Avg_Rate_18_25', data = avg_alcohol_rates$Avg_Rate_18_25, type = 'column') %>%
  hc_add_series(name = 'Avg_Rate_26_plus', data = avg_alcohol_rates$Avg_Rate_26_plus, type = 'column') %>%
  hc_title(text = "Multi-Bar Bar Chart") %>%
  hc_subtitle(text = "Comparison of Multiple Age_Groups by years") %>%
  hc_yAxis(title = list(text = "Avg_Rate_Of_Alcohol_Intake_Pastyear")) %>%
  hc_legend(enabled = TRUE)

HighchartGraph
# Visualization 2 ----
library(plotly)

states = read_csv("states.csv",show_col_types = FALSE) %>%
  rename(Code = Abbreviation)


Marijuana_df <- data %>%
  inner_join(states) %>%
  select(year, state, Code, y1 = `totals_marijuana_new_users_12_17`)



Marijuana_graph <- plot_geo(Marijuana_df,
                            locationmode = 'USA-states',
                            frame = ~year) %>%
  add_trace(locations = ~Code,
            z = ~y1,
            zmin = 0,
            zmax = max(Marijuana_df$y1),
            color = ~y1,
            colorscale = "Earth",
            colorbar = list(title = "Totals_Age=12-17")) %>%
  layout(geo = list(scope = 'usa'),
         title='\n New Users of Marijuana in the US\n 2002-2018')


Marijuana_graph <- plot_geo(Marijuana_df,
                            locationmode = 'USA-states',
                            frame = ~year) %>%
  add_trace(locations = ~Code,
            z = ~y1,
            zmin = 0,
            zmax = max(Marijuana_df$y1),
            color = ~y1,
            colorscale = "Earth",
            colorbar = list(title = "Totals_Age=12-17")) %>%
  layout(geo = list(scope = 'usa'),
         title = '\n New Users of Marijuana in the US\n 2002-2018',
         annotations = list(
           list(
             x = 0.5,
             y = -0.1,
             showarrow = FALSE,
             text = "Note: This chart displays the total number of new marijuana users aged 12-17 in totals i.e (in thousands of people).",
             xref = "paper",
             yref = "paper"
           )
         )
  )

Marijuana_graph


# Visualization 3 ----
# Calculate the total number of users for each substance within the 18-25 age group
data$TotalUsersAlcohol18_25 <- data$`totals_alcohol_use_disorder_past_year_18_25` + data$`totals_alcohol_use_past_month_18_25`
data$TotalUsersTobaccoCigarette18_25 <- data$`totals_tobacco_cigarette_past_month_18_25`
data$TotalUsersCocaine18_25 <- data$`totals_illicit_drugs_cocaine_used_past_year_18_25`
data$TotalUsersMarijuana18_25 <- data$`totals_marijuana_used_past_year_18_25` + data$`totals_marijuana_used_past_month_18_25`+data$`totals_marijuana_new_users_18_25`
data$TotalUsersTobacco18_25 <- data$`totals_tobacco_use_past_month_18_25`
# Create a new dataframe to store the total users for each substance
total_users_data <- data[, c("state", "TotalUsersAlcohol18_25", "TotalUsersTobaccoCigarette18_25", 
                             "TotalUsersCocaine18_25", "TotalUsersMarijuana18_25",
                             "TotalUsersTobacco18_25")]

# Most common user types for each substance
most_common_users <- total_users_data %>% 
  summarise(
    MostCommonAlcohol = which.max(TotalUsersAlcohol18_25),
    MostCommonTobaccoCigarette = which.max(TotalUsersTobaccoCigarette18_25),
    MostCommonCocaine = which.max(TotalUsersCocaine18_25),
    MostCommonMarijuana = which.max(TotalUsersMarijuana18_25),
    MostCommonTobacco = which.max(TotalUsersTobacco18_25)
  )

# Least common user types for each substance
least_common_users <- total_users_data %>% 
  summarise(
    LeastCommonAlcohol = which.min(TotalUsersAlcohol18_25),
    LeastCommonTobaccoCigarette = which.min(TotalUsersTobaccoCigarette18_25),
    LeastCommonCocaine = which.min(TotalUsersCocaine18_25),
    LeastCommonMarijuana = which.min(TotalUsersMarijuana18_25),
    LeastCommonTobacco = which.min(TotalUsersTobacco18_25)
  )

most_common_df <- data.frame(
  Substance = c("Alcohol", "Tobacco", "Cocaine", "Marijuana","TobaccoCigarette"),
  Count = c(most_common_users$MostCommonAlcohol, most_common_users$MostCommonTobacco, most_common_users$MostCommonCocaine,
            most_common_users$MostCommonMarijuana,most_common_users$MostCommonTobaccoCigarette),
  User_Type = c("Alcohol User", "Tobacco User", "Cocaine User", "Marijuana User","TobaccoCigarette User")
)

least_common_df <- data.frame(
  Substance = c("Alcohol", "Tobacco", "Cocaine", "Marijuana","TobaccoCigarette"),
  Count = c(least_common_users$LeastCommonAlcohol, least_common_users$LeastCommonTobacco, 
            least_common_users$LeastCommonCocaine, least_common_users$LeastCommonMarijuana,
            least_common_users$LeastCommonTobacco),
  User_Type = c("Alcohol User", "Tobacco User", "Cocaine User", "Marijuana User","TobaccoCigarette User")
)

# Create a graph for most common user types
MostCommonUserTypesGraph <- ggplot(most_common_df, aes(x = Substance, y = Count, fill = User_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Common User Types for Each Substance",
       x = "Substance",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

MostCommonUserTypesGraph

# Create a graph for least common user types
LeastCommonUserTypesGraph <- ggplot(least_common_df, aes(x = Substance, y = Count, fill = User_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Least Common User Types for Each Substance",
       x = "Substance",
       y = "Count") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

LeastCommonUserTypesGraph
# Visualization 4 ----
# Calculate the percentage increase in drug usage for each age group over the years
data <- data %>%
  mutate(PercentageIncrease1825 = ((`totals_marijuana_used_past_year_18_25` - `totals_marijuana_used_past_year_12_17`) / `totals_marijuana_used_past_year_12_17`) * 100)


HistogramGraph <- ggplot(data = data, aes(x = PercentageIncrease1825)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(
    title = "Percentage Increase in Drug Usage (18-25 Age Group)",
    x = "Percentage Increase",
    y = "Frequency"
  ) +
  theme_minimal()

HistogramGraph








































