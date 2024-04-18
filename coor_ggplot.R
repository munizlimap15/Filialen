# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Load the data
data <- read_excel("Filialdaten.xlsx")

# Handle missing values by replacing them with the mean of each column
data <- data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))%>%
  mutate(Normalized_Revenue = Umsatz / `Kund. Anz.`)

# Calculate correlation matrix and focus on correlation with Revenue (assuming 'Umsatz' is Revenue)
correlations <- cor(data %>% select_if(is.numeric))
revenue_correlations <- correlations['Umsatz',]
revenue_correlations2 <- correlations['Normalized_Revenue',]
# Create a dataframe for plotting
revenue_correlations_df <- data.frame(Variable = names(revenue_correlations), Correlation = revenue_correlations)
revenue_correlations_df2 <- data.frame(Variable = names(revenue_correlations2), Correlation = revenue_correlations2)
# Remove the correlation of 'Umsatz' with itself
revenue_correlations_df <- revenue_correlations_df[-which(revenue_correlations_df$Variable == "Umsatz"),]
revenue_correlations_df2 <- revenue_correlations_df2[-which(revenue_correlations_df2$Variable == "Normalized_Revenue"),]


# Plotting the correlation coefficients
revenue_plot = ggplot(revenue_correlations_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Correlation of Variables with Revenue", x = "Variable", y = "Correlation Coefficient") +
  coord_flip() +  # This flips the axes for better readability
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0, limit = c(min(revenue_correlations_df$Correlation), max(revenue_correlations_df$Correlation))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylim(-1,1)

# Plotting the correlation coefficients
normalized_revenue_plot = ggplot(revenue_correlations_df2, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Correlation of Variables with Revenue/visitors", x = "Variable", y = "Correlation Coefficient") +
  coord_flip() +  # This flips the axes for better readability
  scale_fill_gradient2(low = "red", high = "green", mid = "yellow", midpoint = 0, limit = c(min(revenue_correlations_df2$Correlation), max(revenue_correlations_df2$Correlation))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ylim(-1,1)



grid.arrange(revenue_plot, normalized_revenue_plot, nrow = 2)

