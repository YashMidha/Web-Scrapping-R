library(stringr)
library(rvest)
library(ggplot2)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
page <- read_html(url)
population_table <- page %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE)
population_table <- population_table %>%
  select(Location=2, Population=3)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_life_expectancy"
page <- read_html(url)
life_expectancy_table <- page %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE)
life_expectancy_table <- life_expectancy_table %>%
  select(Location = 1, Life_Expectancy = 2)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_percentage_of_population_living_in_poverty"
page <- read_html(url)
poverty_rate_table <- page %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE)
poverty_rate_table <- poverty_rate_table %>%
  select(Location = 1, Income_group = 3, Poverty_rate = 4)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"
page <- read_html(url)
gdp_table <- page %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE)
gdp_table <- gdp_table %>%
  select(Location = 1, GDP = 2)

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_inflation_rate"
page <- read_html(url)
inflation_rate_table <- page %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE)
inflation_rate_table <- inflation_rate_table %>%
  select(Location = 1, Inflation_Rate = 2)

url <- "https://en.wikipedia.org/wiki/Human_Capital_Index"
page <- read_html(url)
Human_Capital_Index <- page %>% 
  html_node("table.wikitable") %>% 
  html_table(fill = TRUE)
Human_Capital_Index <- Human_Capital_Index %>%
  select(Location = 2, Human_Capital_Index = 3)


data <- left_join(population_table, life_expectancy_table, by = "Location")
data <- left_join(data, poverty_rate_table, by="Location")
data <- left_join(data, gdp_table, by="Location")
data <- left_join(data, inflation_rate_table, by="Location")
data <- left_join(data, Human_Capital_Index, by="Location")
head(data)


# 1 Discover and Inital examination
str(data)
summary(data)
cat("Diminsion: ", dim(data))
lapply(data, class)
print("Missing values")
data %>% summarise(across(everything(), ~sum(is.na(.))))
summary_stats <- data %>%
  summarise(
    Population_mean = mean(Population, na.rm = TRUE),
    Life_Expectancy_mean = mean(Life_Expectancy, na.rm = TRUE),
    Poverty_rate_mean = mean(Poverty_rate, na.rm = TRUE),
    GDP_mean = mean(GDP, na.rm = TRUE),
    Inflation_Rate_mean = mean(Inflation_Rate, na.rm = TRUE),
    Human_Capital_Index_mean = mean(Human_Capital_Index, na.rm = TRUE)
  )

print(summary_stats)

# 2 Structure:-
data <- data %>% 
  mutate( Population = as.numeric(gsub(",", "", Population)) )

data <- data %>% 
  mutate( GDP = as.numeric(gsub(",", "", GDP)) )

data <- data %>% 
  mutate(
    Life_Expectancy = as.numeric(Life_Expectancy),
    Inflation_Rate = as.numeric(Inflation_Rate),
    Human_Capital_Index = as.numeric(Human_Capital_Index),
    Poverty_rate = as.numeric(gsub("%", "", Poverty_rate)) / 100
  )

data <- data %>% filter(Location != "World")  # removing the world row

# Re-examination
str(data)
summary(data)
cat("Diminsion: ", dim(data))
lapply(data, class)
print("Missing values")
data %>% summarise(across(everything(), ~sum(is.na(.))))

summary_stats <- data %>%
  summarise(
    Population_mean = mean(Population, na.rm = TRUE),
    Life_Expectancy_mean = mean(Life_Expectancy, na.rm = TRUE),
    Poverty_rate_mean = mean(Poverty_rate, na.rm = TRUE),
    GDP_mean = mean(GDP, na.rm = TRUE),
    Inflation_Rate_mean = mean(Inflation_Rate, na.rm = TRUE),
    Human_Capital_Index_mean = mean(Human_Capital_Index, na.rm = TRUE)
  )

print(summary_stats)

# Visualization
ggplot(data, aes(x = Income_group)) +
  geom_bar(fill = "blue") + 
  labs(title = "Income Group Distribution", x = "Income Group", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Life_Expectancy)) +
  geom_histogram(binwidth = 2, fill = "green", color = "black") +
  labs(title = "Distribution of Life Expectancy", x = "Life Expectancy", y = "Frequency")

# 3 - Cleaning

# Missing values
data <- data %>%
  mutate(
    Life_Expectancy = ifelse(is.na(Life_Expectancy), mean(Life_Expectancy, na.rm = TRUE), Life_Expectancy),
    Income_group = ifelse(is.na(Income_group), "Unknown", Income_group), 
    Poverty_rate = ifelse(is.na(Poverty_rate), mean(Poverty_rate, na.rm = TRUE), Poverty_rate),
    GDP = ifelse(is.na(GDP), mean(GDP, na.rm = TRUE), GDP),
    Inflation_Rate = ifelse(is.na(Inflation_Rate), mean(Inflation_Rate, na.rm = TRUE), Inflation_Rate),
    Human_Capital_Index = ifelse(is.na(Human_Capital_Index), mean(Human_Capital_Index, na.rm = TRUE), Human_Capital_Index)
  )
print(head(data))

# Check for duplicates in the dataset
duplicates <- data[duplicated(data), ]
print(duplicates)
data <- data %>% distinct()

# 4 - Enrich

# Add a new categorical feature based on Income Group
data <- data %>%
  mutate(
    Wealthy = case_when(
      Income_group == "High income" ~ "Yes",
      TRUE ~ "No"
    )
  )

# GDP per capita
data <- data %>%
  mutate(GDP_per_capita = GDP / Population)

# life expentancy_change above or below global average
data <- data %>%
  mutate(Life_Expectancy_Above_Average = ifelse(Life_Expectancy > mean(Life_Expectancy), "Above Average", "Below Average"))

# e. Validate

# Check for missing values
missing_check <- data %>% summarise(across(everything(), ~sum(is.na(.))))

# Check for duplicates
duplicates_check <- data %>% distinct() %>% count()

# 1. Range Consistency Checks
# Check for unrealistic Life Expectancy values (e.g., > 100)
life_expectancy_check <- data %>%
  filter(Life_Expectancy > 100) %>%
  select(Location, Life_Expectancy)

# Check for unrealistic Inflation Rates (should be between 0 and 100)
inflation_rate_check <- data %>%
  filter(Inflation_Rate < 0 | Inflation_Rate > 100) %>%
  select(Location, Inflation_Rate)

# Check for Human Capital Index range (should be between 0 and 1)
human_capital_check <- data %>%
  filter(Human_Capital_Index < 0 | Human_Capital_Index > 1) %>%
  select(Location, Human_Capital_Index)

# 2. Categorical Consistency Check (Income Group)
income_group_check <- data %>%
  filter(!Income_group %in% c("Unknown", "Low income", "Lower middle income", "Upper middle income", "High income")) %>%
  select(Location, Income_group)

# 3. Cross-Column Consistency Check
gdp_income_consistency_check <- data %>%
  filter(
    (Income_group == "High income" & Wealthy == "No") | 
      (Income_group == "Low income" & Wealthy != "No")
  ) %>%
  select(Location, GDP, Income_group)

list(
  life_expectancy_check = life_expectancy_check,
  inflation_rate_check = inflation_rate_check,
  human_capital_check = human_capital_check,
  income_group_check = income_group_check,
  gdp_income_consistency_check = gdp_income_consistency_check
)

data <- data %>%
  mutate(Income_group = ifelse(Location == "Venezuela" & Income_group == "0", "Unknown", Income_group))

data %>% filter(Location == "Venezuela")

# 3. Publish
write.csv(data, "countries_statistics.csv", row.names = FALSE)
