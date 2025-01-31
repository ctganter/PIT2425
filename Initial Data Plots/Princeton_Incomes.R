library(tidycensus)
library(tidyverse)
library(janitor)

census_api_key("8524147f6edf7fe4b7c85681397fe5acd6993d62")
# Distribution of household incomes
acs_income <- get_acs(geography = "place",
                      state = "NJ",
                      variables = c("B19001_002","B19001_003","B19001_004","B19001_005","B19001_006","B19001_007",
                                       "B19001_008","B19001_009","B19001_010","B19001_011","B19001_012","B19001_013","B19001_014",
                                       "B19001_015","B19001_016","B19001_017"),
                      year = 2023,
                      survey = "acs5")

princeton_acs_income <- acs_income %>%
  filter(NAME == "Princeton, New Jersey")

head(princeton_acs_income)

# Run a visualization

# Create the data for the chart, into five bins

labels <- c("Less than $50,000", "$50,000 to $100,000", 
            "$100,000 to $150,000", "$150,000 to $200,000",
            "$200,000+")
labels_ordered <- factor(labels, levels=rev(labels))

bins <- c(1, 2, 3, 4, 5)

# sum the estimates
income_bins <- c(
  sum(princeton_acs_income$estimate[1:9]),   # Less than $50,000 (B19001_002 to B19001_010)
  sum(princeton_acs_income$estimate[10:12]),  # $50,000 to $100,000 (B19001_011 to B19001_013)
  sum(princeton_acs_income$estimate[13:14]),  # $100,000 to $150,000 (B19001_014 to B19001_015)
  sum(princeton_acs_income$estimate[15]),  # $150,000 to $200,000 (B19001_016 to B19001_017)
  sum(princeton_acs_income$estimate[16])      # $200,000+ (B19001_017)
)

total_households <- sum(income_bins)

income_percentage <- income_bins / total_households * 100

income_dist <- data.frame(labels_ordered, percentages = income_percentage)


barchart <- ggplot(income_dist) +
  aes(x = percentages, y = labels_ordered) +
  geom_col(fill = "lightblue") +
  scale_x_continuous(labels = scales::comma) + # format count labels with commas and thousands
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  labs(
    title = 'Income Distribution Among Princeton Households',
    x = "Percentage of Princeton households",
    y = "Household income in the past 12 months",
    caption = "Source: Census American Community Survey 2023 5-Year Estimates"
  )
  

barchart


