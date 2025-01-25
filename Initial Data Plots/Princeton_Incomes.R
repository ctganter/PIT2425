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

# Create the data for the chart
estimates = princeton_acs_income[['estimate']]

labels <- c("Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999",
            "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
            "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999",
            "$50,000 to $59,999", "$60,000 to $74,999", "$75,000 to $99,999",
            "$100,000 to $124,999", "$125,000 to $149,999","$150,000 to $199,999",
            "$200,000 or more")
labels_ordered <- factor(labels, levels=labels)


income_dist <- data.frame(labels_ordered, estimates)
print(income_dist)

barchart <- ggplot(income_dist) +
  aes(x = estimates, y = labels_ordered) +
  geom_col(fill = "lightblue") +
  scale_x_continuous(labels = scales::comma) + # format count labels with commas and thousands
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  labs(
    title = 'Income Distribution Among Princeton Households',
    x = "Number of Princeton households",
    y = "Household income in the past 12 months",
    caption = "Source: Census American Community Survey 2023 5-Year Estimates"
  )
  

barchart


