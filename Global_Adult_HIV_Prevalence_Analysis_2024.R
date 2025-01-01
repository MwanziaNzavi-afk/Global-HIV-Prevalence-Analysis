# Load necessary libraries
library(tidyverse)
library(corrplot)
library(stringr)  # For handling string replacements

# ===========================================
# Step 1: Load the Dataset
# ===========================================

# Specify the file path
file_path <- "C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/hiv_prevalence.csv"

# Read the dataset using semicolon as the delimiter
hiv_data <- read.csv(file_path, stringsAsFactors = FALSE, sep = ";", header = FALSE)

# Check the first few rows of the dataset
head(hiv_data)

# ===========================================
# Step 2: Clean the Data
# ===========================================

# Remove rows that contain metadata (e.g., first and second rows)
hiv_data_cleaned <- hiv_data[-c(1, 2), ]

# Fix column names to make them appropriate
colnames(hiv_data_cleaned) <- c("Country_Region", "Adult_Prevalence", "People_Living_With_HIV", "Annual_Deaths", "Year")

# Clean up the data by removing unwanted characters like extra spaces and non-breaking spaces
hiv_data_cleaned$Country_Region <- str_replace_all(hiv_data_cleaned$Country_Region, "\u00A0", "")  # Replace non-breaking space with nothing
hiv_data_cleaned$Country_Region <- str_trim(hiv_data_cleaned$Country_Region)  # Remove extra spaces

# Clean up the Adult_Prevalence column (remove percentage signs)
hiv_data_cleaned$Adult_Prevalence <- as.numeric(gsub("%", "", hiv_data_cleaned$Adult_Prevalence))

# Clean up the People_Living_With_HIV and Annual_Deaths columns (remove commas and convert to numeric)
hiv_data_cleaned$People_Living_With_HIV <- as.numeric(gsub(",", "", hiv_data_cleaned$People_Living_With_HIV))
hiv_data_cleaned$Annual_Deaths <- as.numeric(gsub(",", "", hiv_data_cleaned$Annual_Deaths))


# Convert Year column to numeric
hiv_data_cleaned$Year <- as.numeric(hiv_data_cleaned$Year)

# Save the cleaned data to the same directory
write.csv(hiv_data_cleaned, "C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/hiv_prevalence_cleaned.csv", row.names = FALSE)

# ===========================================
# Step 3: Exploratory Data Analysis (EDA)
# ===========================================

# Summary statistics
summary(hiv_data_cleaned)

# Distribution of Adult HIV Prevalence
plot1 <- ggplot(hiv_data_cleaned, aes(x = Adult_Prevalence)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Adult HIV Prevalence", x = "Adult HIV Prevalence (%)", y = "Frequency")

# Save the plot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/adult_hiv_prevalence_distribution.png", plot = plot1)

# Scatter plot: HIV prevalence vs. Annual deaths
plot2 <- ggplot(hiv_data_cleaned, aes(x = Adult_Prevalence, y = Annual_Deaths)) +
  geom_point(color = "red", alpha = 0.7) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Adult HIV Prevalence vs. Annual Deaths", x = "Adult HIV Prevalence (%)", y = "Annual Deaths")

# Save the plot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/hiv_prevalence_vs_deaths.png", plot = plot2)

# Correlation between Adult Prevalence, People Living With HIV, and Annual Deaths
correlation_matrix <- cor(hiv_data_cleaned[, c("Adult_Prevalence", "People_Living_With_HIV", "Annual_Deaths")], use = "complete.obs")
print("Correlation Matrix:")
print(correlation_matrix)

# Heatmap of the correlation matrix
plot3 <- corrplot(correlation_matrix, method = "color", addCoef.col = "black", number.cex = 0.7, title = "Correlation Matrix")

# Save the heatmap plot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/correlation_matrix.png", plot = plot3)

# ===========================================
# Step 4: Regional Analysis
# ===========================================

# Summarize by region
regional_summary <- hiv_data_cleaned %>%
  group_by(Country_Region) %>%
  summarise(
    Avg_Adult_Prevalence = mean(Adult_Prevalence, na.rm = TRUE),
    Total_Annual_Deaths = sum(Annual_Deaths, na.rm = TRUE),
    Total_People_Living_With_HIV = sum(People_Living_With_HIV, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Annual_Deaths))

# Top 10 regions with the highest total deaths
top_regions <- regional_summary[1:10, ]
print("Top 10 Regions with Highest Annual Deaths:")
print(top_regions)

# Save the regional summary to a CSV file
write.csv(regional_summary, "C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/regional_summary.csv", row.names = FALSE)

# ===========================================
# Step 5: Trends Over Time
# ===========================================

# Line plot of average prevalence over time
trend_data <- hiv_data_cleaned %>%
  group_by(Year) %>%
  summarise(Avg_Adult_Prevalence = mean(Adult_Prevalence, na.rm = TRUE))

plot4 <- ggplot(trend_data, aes(x = Year, y = Avg_Adult_Prevalence)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Trend of Adult HIV Prevalence Over Time", x = "Year", y = "Average Adult HIV Prevalence (%)")

# Save the trend plot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/adult_hiv_prevalence_trend.png", plot = plot4)

# ===========================================
# Step 6: Additional Plots
# ===========================================

# Boxplot for Annual Deaths by Country/Region
plot5 <- ggplot(hiv_data_cleaned, aes(x = Country_Region, y = Annual_Deaths)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Annual Deaths by Country/Region", x = "Country/Region", y = "Annual Deaths") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the boxplot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/annual_deaths_by_region.png", plot = plot5)

# Bar plot: Total People Living With HIV by Country/Region
plot6 <- ggplot(hiv_data_cleaned, aes(x = Country_Region, y = People_Living_With_HIV)) +
  geom_bar(stat = "identity", fill = "purple", color = "black") +
  labs(title = "Total People Living With HIV by Country/Region", x = "Country/Region", y = "People Living With HIV") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the bar plot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/people_living_with_hiv_by_region.png", plot = plot6)

# ===========================================
# Step 7: Statistical Modeling
# ===========================================

# Linear regression to predict annual deaths based on HIV prevalence
model <- lm(Annual_Deaths ~ Adult_Prevalence, data = hiv_data_cleaned)

# Summary of the model
summary(model)

# Plot the regression line
plot7 <- ggplot(hiv_data_cleaned, aes(x = Adult_Prevalence, y = Annual_Deaths)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Linear Regression: Annual Deaths vs. HIV Prevalence", x = "Adult HIV Prevalence (%)", y = "Annual Deaths")

# Save the regression plot
ggsave("C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/regression_plot.png", plot = plot7)

# ===========================================
# Step 8: Create Summary Report
# ===========================================

# Create a summary text file
summary_text <- "
# HIV Prevalence Data Analysis Summary

1. Data Cleaning:
- The dataset was cleaned by removing metadata rows, fixing column names, and cleaning the data columns.
- Non-breaking spaces and commas were removed, and percentage signs in the prevalence column were stripped.

2. Exploratory Data Analysis (EDA):
- The distribution of adult HIV prevalence was visualized through a histogram.
- A scatter plot was created to show the relationship between HIV prevalence and annual deaths.
- A correlation matrix heatmap was generated to show the relationships between key variables.

3. Regional Analysis:
- A summary of HIV prevalence and related statistics was provided for each country/region.
- The top 10 regions with the highest annual deaths were identified.

4. Trends Over Time:
- A line plot showed the trend of adult HIV prevalence over time.

5. Additional Plots:
- A boxplot was created to visualize annual deaths by country/region.
- A bar plot was generated to show the total number of people living with HIV in each country/region.

6. Statistical Modeling:
- A linear regression model was created to predict annual deaths based on HIV prevalence.

# Saved Files:
- Cleaned data: hiv_prevalence_cleaned.csv
- Plots: 
  - adult_hiv_prevalence_distribution.png
  - hiv_prevalence_vs_deaths.png
  - correlation_matrix.png
  - adult_hiv_prevalence_trend.png
  - annual_deaths_by_region.png
  - people_living_with_hiv_by_region.png
  - regression_plot.png

"

# Save the summary text file
writeLines(summary_text, "C:/Users/mwanz/OneDrive/Desktop/Global Adult HIV Prevalence/analysis_summary.txt")
