# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(foreign)
library(broom)
library(plotly)

education_data <- read.csv("C:/Users/Alex/Desktop/Academic Projos/Global Education analysis/Global_Education.csv")


# Explore the structure of the dataset
str(education_data)

# Display the first few rows of the dataset
head(education_data)

# Summary statistics
summary(education_data)

# Check for missing values
sapply(education_data, function(x) sum(is.na(x)))

# Correlation matrix for key variables
key_variables <- c("Completion_Rate_Primary_Male", "Unemployment_Rate", "Gross_Primary_Education_Enrollment")
correlation_matrix <- cor(education_data[, key_variables])
print(correlation_matrix)



# Visualize the correlation matrix using heatmap with improved labels
heatmap(correlation_matrix,
        col = colorRampPalette(c("navy", "white", "firebrick3"))(90),
        main = "Correlation Matrix for Key Variables",
        cex.main = 2.5,
        cex.axis = 2.5,  # font size of axis labels
        margins = c(10, 10),  # extra margin space
        symm = TRUE)

# Rename column
names(education_data)[names(education_data) == "Countries.and.areas"] <- "Countries_and_areas"



names(education_data)

# Select relevant columns for completion rates
completion_data <- education_data %>%
  select(
    Countries_and_areas,
    Completion_Rate_Primary_Male,
    Completion_Rate_Primary_Female,
    Completion_Rate_Lower_Secondary_Male,
    Completion_Rate_Lower_Secondary_Female,
    Completion_Rate_Upper_Secondary_Male,
    Completion_Rate_Upper_Secondary_Female
  )

# Reshape the data for plotting
completion_data_long <- completion_data %>%
  pivot_longer(
    cols = starts_with("Completion_Rate"),
    names_to = c("Level", "Gender"),
    names_pattern = "Completion_Rate_(.+)_(.+)",
    values_to = "Completion_Rate"
  )

# Convert non-ASCII characters to ASCII
completion_data_long$Countries_and_areas <- iconv(completion_data_long$Countries_and_areas, to = "ASCII", sub = " ")

# Create an interactive map using plotly
map_plot <- plot_geo(completion_data_long, 
                     locationmode = "country names",
                     locations = ~Countries_and_areas,
                     color = ~Completion_Rate,
                     colorscale = "Viridis",
                     text = ~paste("Country: ", Countries_and_areas, "<br>",
                                   "Completion Rate: ", Completion_Rate, "<br>",
                                   "Level: ", Level, "<br>",
                                   "Gender: ", Gender),
                     marker = list(line = list(width = 0.5, color = "white")))

# C Customize the layout
map_layout <- list(title = "Completion Rates Over Different Education Levels",
                   geo = list(showframe = FALSE, showcoastlines = TRUE, projection = list(type = "mercator")))

# Combine the map and layout
map_plot <- map_plot %>% layout(map_layout)

# Display the interactive map
map_plot


# Select relevant columns for completion rates
dropout_data <- education_data %>%
  select(
    OOSR_Primary_Age_Male,
    OOSR_Primary_Age_Female
  )

# Reshape the data for plotting
dropout_data_long <- dropout_data %>%
  pivot_longer(
    cols = starts_with("OOSR_Primary_Age"),
    names_to = "Gender",
    values_to = "Dropout_Rate"
  )

# Create a histogram for completion rates
ggplot(dropout_data_long, aes(x = Dropout_Rate, fill = Gender)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of Dropout Rates from Primary Education",
       x = "Dropout Rate",
       y = "Frequency") +
  scale_fill_manual(values = c("pink", "blue")) 


# Select notable variables for distribution visualization
notable_variables <- education_data %>%
  select(
    Birth_Rate,
    Youth_15_24_Literacy_Rate_Male,
    Youth_15_24_Literacy_Rate_Female,
    Gross_Primary_Education_Enrollment,
    Unemployment_Rate
  )

# Reshape the data for plotting (optional if you want to use ggplot)
notable_variables_long <- notable_variables %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Create individual histograms for each variable
ggplot(notable_variables_long, aes(x = Value, fill = Variable)) +
  geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +  
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Distribution of Notable Variables",
       x = "Value",
       y = "Frequency")




                             #EXPERIMENT 
#1. Impact of Socio-Economic Status on Completion Rates
# Select relevant columns for the analysis
ses_completion_data <- education_data %>%
  select(Completion_Rate_Primary_Male, Unemployment_Rate, Birth_Rate, Gross_Primary_Education_Enrollment)


# Handle missing values if any
ses_completion_data <- na.omit(ses_completion_data)

# Fit a linear regression model
ses_model <- lm(Completion_Rate_Primary_Male ~ Unemployment_Rate + Birth_Rate + Gross_Primary_Education_Enrollment, 
                data = ses_completion_data)

# Summarize the regression results
summary(ses_model)


# Create diagnostic plots
par(mfrow = c(2, 2))  # Set up a 2x2 grid for the plots

# Plot residuals vs. fitted values
plot(ses_model, which = 1, main = "Residuals vs. Fitted")

# Plot normal Q-Q plot of residuals
plot(ses_model, which = 2, main = "Normal Q-Q")

# Plot scale-location plot (square root of standardized residuals vs. fitted values)
plot(ses_model, which = 3, main = "Scale-Location")

# Plot residuals vs. leverage
plot(ses_model, which = 5, main = "Residuals vs. Leverage")

par(mfrow = c(1, 1))  # Reset plotting layout


# Visualize the relationship between Unemployment_Rate and Completion_Rate_Primary_Male
ggplot(ses_completion_data, aes(x = Unemployment_Rate, y = Completion_Rate_Primary_Male)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Unemployment Rate and Completion Rate",
       x = "Unemployment Rate",
       y = "Completion Rate (Primary Male)")

#2: Regional Disparities in Educational Metrics

# Define regions based on geographical coordinates
education_data <- education_data %>%
  mutate(Region = case_when(
    Latitude < 0 & Longitude < 30 ~ "Africa",
    Latitude > 0 & Longitude < 30 ~ "Europe",
    Latitude < 0 & Longitude > 30 ~ "South America",
    Latitude > 0 & Longitude > 30 ~ "Asia",
    TRUE ~ "Other"
  ))

# Check the unique regions
unique(education_data$Region)

# Compute average completion rates for each region
average_completion_rates <- education_data %>%
  group_by(Region) %>%
  summarize(
    Avg_Completion_Rate_Primary_Male = mean(Completion_Rate_Primary_Male, na.rm = TRUE),
    Avg_Completion_Rate_Primary_Female = mean(Completion_Rate_Primary_Female, na.rm = TRUE),
    Avg_Completion_Rate_Lower_Secondary_Male = mean(Completion_Rate_Lower_Secondary_Male, na.rm = TRUE),
    Avg_Completion_Rate_Lower_Secondary_Female = mean(Completion_Rate_Lower_Secondary_Female, na.rm = TRUE),
    Avg_Completion_Rate_Upper_Secondary_Male = mean(Completion_Rate_Upper_Secondary_Male, na.rm = TRUE),
    Avg_Completion_Rate_Upper_Secondary_Female = mean(Completion_Rate_Upper_Secondary_Female, na.rm = TRUE)
  )

# Print the computed averages
print(average_completion_rates)



# Convert the average_completion_rates tibble to long format
average_completion_rates_long <- tidyr::gather(average_completion_rates, key = "Completion_Rate_Type", value = "Average_Completion_Rate", -Region)

# Create a bar plot
ggplot(average_completion_rates_long, aes(x = Region, y = Average_Completion_Rate, fill = Completion_Rate_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Completion Rates by Region",
       y = "Average Completion Rate",
       x = "Region",
       fill = "Completion Rate Type") +
  theme_minimal()



#Experiment last: Socio-Economic Influences on Proficiency Levels
# Select relevant variables for the analysis
selected_variables <- c("Grade_2_3_Proficiency_Reading", "Grade_2_3_Proficiency_Math",
                        "Birth_Rate", "Gross_Primary_Education_Enrollment",
                        "Gross_Tertiary_Education_Enrollment", "Unemployment_Rate")

selected_data <- education_data %>%
  select(all_of(selected_variables))

# Check for missing values
missing_values <- colSums(is.na(selected_data))
print(missing_values)


# Fit multiple regression models
model_reading <- lm(Grade_2_3_Proficiency_Reading ~ ., data = selected_data)
model_math <- lm(Grade_2_3_Proficiency_Math ~ ., data = selected_data)

# Summarize the regression models
summary_reading <- tidy(model_reading)
summary_math <- tidy(model_math)

# Print the summaries
print(summary_reading)
print(summary_math)




# Plot for Grade_2_3_Proficiency_Reading
ggplot(data = selected_data, aes(x = Grade_2_3_Proficiency_Math, y = Grade_2_3_Proficiency_Reading)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Proficiency in Reading vs. Math",
       x = "Grade 2-3 Proficiency in Math",
       y = "Grade 2-3 Proficiency in Reading") +
  theme_minimal()

# Plot for Grade_2_3_Proficiency_Math
ggplot(data = selected_data, aes(x = Grade_2_3_Proficiency_Reading, y = Grade_2_3_Proficiency_Math)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Proficiency in Math vs. Reading",
       x = "Grade 2-3 Proficiency in Reading",
       y = "Grade 2-3 Proficiency in Math") +
  theme_minimal()


