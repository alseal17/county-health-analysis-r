install.packages(c("tidyverse", "janitor", "skimr", "corrplot"))

library(tidyverse)
library(janitor)
library(skimr)
library(corrplot)

# Load the data 

ranked <- read_csv("C:/Users/alyss/Downloads/2023 County Health Rankings Data/Ranked Measure Data.csv", skip = 1) %>%
  clean_names()

additional <- read_csv("C:/Users/alyss/Downloads/2023 County Health Rankings Data/Additional Measure Data.csv", skip = 1) %>%
  clean_names()

rankings <-read_csv("C:/Users/alyss/Downloads/2023 County Health Rankings Data/Outcomes and Factors Rankings.csv", skip = 1) %>%
  clean_names()

# Inspect the data 

glimpse(ranked)
glimpse(additional)
glimpse(rankings)

names(ranked)
names(additional)
names(rankings)

# clean county level data

ranked_counties <- ranked %>%
  filter(!is.na(county))

additional_counties <- additional %>%
  filter(!is.na(county))

rankings_counties <- rankings %>%
  filter(!is.na(county))

# Select useful variables 

health_data <- ranked_counties %>%
  select(
    fips,
    state,
    county,
    
    # Health outcomes
    years_of_potential_life_lost_rate,
    percent_fair_or_poor_health,
    average_number_of_physically_unhealthy_days,
    average_number_of_mentally_unhealthy_days,
    
    # Behaviors
    percent_adults_reporting_currently_smoking,
    percent_adults_with_obesity,
    percent_physically_inactive,
    
    # Healthcare access
    percent_uninsured,
    primary_care_physicians_rate,
    mental_health_provider_rate,
    
    # Socioeconomic
    percent_some_college,
    percent_unemployed,
    percent_children_in_poverty,
    income_ratio,
    
    # Safety / environment (nice add for your background)
    injury_death_rate,
    
    # Environment
    average_daily_pm2_5,
    percent_severe_housing_problems
  )

# Add life expectancy and median income 

additional_selected <- additional_counties %>%
  select(
    fips, 
    life_expectancy,
    median_household_income,
    population,
    percent_rural
    ) 

health_data <- health_data %>%
  left_join(additional_selected, by = "fips")

# Clean numeric columns 

health_data <- health_data %>%
  mutate(across(
    where(is.character),
    ~ str_replace_all(.x, ",", "")
  )) %>%
  mutate(across(
    -c(fips, state, county),
    as.numeric
  ))

# Basic summary 

summary(health_data)

skim(health_data)

# Main visualization: income vs life expectancy

ggplot(health_data, aes(x = median_household_income, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Median Household Income and Life Expectancy by U.S. County",
    x = "Median Household Income",
    y = "Life Expectancy"
  ) +
  theme_minimal()

# Education vs life expectancy 

ggplot(health_data, aes(x = percent_some_college, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Education and Life Expectancy by U.S. County",
    x = "% Some College",
    y = "Life Expectancy"
  ) +
  theme_minimal()

# Smoking vs life expectancy 

ggplot(health_data, aes(x = percent_adults_reporting_currently_smoking, y = life_expectancy)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Smoking and Life Expectancy by U.S. County",
    x = "% Adult Smoking",
    y = "Life Expectancy"
  ) +
  theme_minimal()

# Correlation Heatmap 

cor_data <- health_data %>%
  select(
    life_expectancy,
    median_household_income,
    percent_some_college,
    percent_adults_reporting_currently_smoking,
    percent_adults_with_obesity,
    percent_physically_inactive,
    percent_uninsured,
    percent_children_in_poverty,
    injury_death_rate
  ) %>%
  drop_na()

cor_matrix <- cor(cor_data)

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)


# Simple regression model 

model <- lm(
  life_expectancy ~ median_household_income +
    percent_some_college +
    percent_adults_reporting_currently_smoking +
    percent_adults_with_obesity +
    percent_uninsured +
    percent_children_in_poverty,
  data = health_data
)

summary(model)

# Save Cleaned data 

write_csv(health_data, "health_county_cleaned.csv")
