library(tidycensus)
library(tidyverse)
library(janitor)

census_api_key("8524147f6edf7fe4b7c85681397fe5acd6993d62")
# Distribution of household incomes
acs_income <- get_acs(geography = "place",
                      state = "NJ",
                      variables = c("B19001B_002", "B19001B_003", "B19001B_004", "B19001B_005", "B19001B_006",
                                    "B19001B_007", "B19001B_008", "B19001B_009", "B19001B_010", "B19001B_011",
                                    "B19001B_012", "B19001B_013", "B19001B_014", "B19001B_015", "B19001B_016",
                                    "B19001B_017"),
                      year = 2023,
                      survey = "acs5")

princeton_acs_income <- acs_income %>%
  filter(NAME == "Princeton, New Jersey")
princeton_acs_income_shortened <- c(sum(princeton_acs_income[0:3]),sum(princeton_acs_income[4:7]),
                                    sum(princeton_acs_income[8:11]),sum(princeton_acs_income[12:15]))
head(princeton_acs_income_shortened)

# Run a visualization

# Create the data for the chart
estimates = princeton_acs_income_shortened[['estimate']]

labels <- c("Less than $24,999", "$25,000 to $44,999", "$45,000 to $99,999",
            "$100,000 or more")
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


