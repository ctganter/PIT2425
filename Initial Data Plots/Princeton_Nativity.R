# Test Comment
library(tidycensus)
library(tidyverse)
library(janitor)

census_api_key("8524147f6edf7fe4b7c85681397fe5acd6993d62")
# Distribution of household nativites
acs_nativity <- get_acs(geography = "place",
                      state = "NJ",
                      variables = c("B05001_002", "B05001_003", "B05001_004", "B05001_005", "B05001_006"),
                      survey = "acs5")

princeton_acs_nativity <- acs_nativity %>%
  filter(NAME == "Princeton, New Jersey")

head(princeton_acs_nativity)

# Run a visualization

# Create the data for the chart
estimates = princeton_acs_nativity[['estimate']]

labels <- c(
  "Citizen, \nborn in U.S",
  "Citizen, \nborn in Puerto \nRico or U.S. \nIsland Areas",
  "Citizen, \nborn abroad of \nAmerican \nparent(s)",
  "Citizen \nby naturalization",
  "Not a Citizen")
labels_ordered <- factor(labels, levels=labels)


nativity_dist <- data.frame(labels_ordered, estimates)
print(nativity_dist)

barchart <- ggplot(nativity_dist) +
  aes(y = estimates, x = labels_ordered) +
  geom_col(fill = "lightblue") +
  scale_y_continuous(labels = scales::comma) + # format count labels with commas and thousands
  theme_minimal() +
  theme(panel.grid.major.x = element_blank()) +
  labs(
    title = 'Nativity and Citizenship Status Among Princeton Residents',
    y = "Number of Princeton Residents",
    x = "Nativity and Citizenship Status",
    caption = "Source: Census American Community Survey 2023 5-Year Estimates"
  )


barchart





