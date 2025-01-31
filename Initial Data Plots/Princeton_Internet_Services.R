library(tidycensus)
library(tidyverse)
library(janitor)

census_api_key("8524147f6edf7fe4b7c85681397fe5acd6993d62")
# Distribution of internet services
acs_internet <- get_acs(geography = "place",
                      state = "NJ",
                      variables = c(
                                    "B28011_004", "B28011_005", 
                                    "B28011_006", "B28011_007", 
                                    "B28011_008"),
                      year = 2023,
                      survey = "acs5")

princeton_acs_internet <- acs_internet %>%
  filter(NAME == "Princeton, New Jersey")

head(princeton_acs_internet)

# Run a visualization

# Create the data for the chart
estimates = princeton_acs_internet[['estimate']]

total_households <- sum(estimates)
internet_percentage <- estimates / total_households * 100

# Sample data
internet_sub <- data.frame(
  category = c( 
               "Broadband such as cable, fiber optic, or DSL", 
               "Satellite Internet service", 
               "Other service", 
               "Internet access without a subscription", 
               "No Internet access"),
  internet_percentage  # Example values for each category
)

# Create a new variable for grouping
internet_sub$group <- ifelse(internet_sub$category %in% c("Broadband such as cable, fiber optic, or DSL", 
                                          "Satellite Internet service", 
                                          "Other service"),
                     "With an Internet subscription", 
                     internet_sub$category)

# Reorder factor levels for better stacking order
internet_sub$category <- factor(internet_sub$category, levels = c("Broadband such as cable, fiber optic, or DSL", 
                                                  "Satellite Internet service", 
                                                  "Other service",  
                                                  "Internet access without a subscription", 
                                                  "No Internet access"))

# Plot the stacked bar chart
ggplot(internet_sub, aes(x = group, y = internet_percentage, fill = category)) +
  geom_bar(stat = "identity") +
  labs(title = "Internet Subscription Breakdown in Princeton Households",
       x = "Type of Subscription",
       y = "Percentage of Households") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  # You can change the color palette here
 # guides(fill = guide_legend(
    # Remove "Internet access without a subscription" and "No Internet access" from the legend
  #  override.aes = list(fill = c("red", "blue", "green", "purple", "pink")[1:4])
 # )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate axis labels for readability
  geom_text(aes(label = round(internet_percentage, 1)), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 2)


