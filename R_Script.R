# ------------------------------------------------------------------------------
#                             Final Project 
#             ALY 6010 Probability Theory and Statistics 
#                               HUSKY 4 
# Group Project  Professor- Sergiy Shevchenko   Northeastern University
# ------------------------------------------------------------------------------

# STEP - 1
# Read / import the data set file and store into variable.

# Access the 'pacman' package
library(pacman) 

# read.csv() used to read file
EV <- read.csv("Electric_Vehicle_Population_Data.csv") 
View(EV) # view data set into tabular format


# STEP - 2
# Preparing Data Frame for Analysis.

# Clean Names - Clean column names which have special characters within it.

p_load(janitor) # Install and load janitor package

# Removes wild characters from column names.
EV <- clean_names(EV) 

#  Display only Column names.
names(EV)  

# Rename columns - Assigning Appropriate names to columns. 
# Assign new names to columns 

names(EV)[names(EV) == "vin_1_10"] <- "VIN"
names(EV)[names(EV) == "clean_alternative_fuel_vehicle_cafv_eligibility"] <- "CAFV_Eligibility"
names(EV)[names(EV) == "electric_vehicle_type"] <- "EV_Type"
names(EV)[names(EV) == "dol_vehicle_id"] <- "DOL"
names(EV)[names(EV) == "electric_range"] <- "EV_Range"
names(EV)[names(EV) == "county"]<- "country"

#  Display only Column names.
names(EV)


# Drop / Remove unnecessary variables / columns

p_load(tidyverse) # load the 'tidyverse' library

# Drop columns which are not much important
EV <- EV |> select(-vehicle_location,-state,-electric_utility) 
View(EV)


# Data Cleaning
# Managing N/As
EV$EV_Range[is.na(EV$EV_Range)] <- median(EV$EV_Range, na.rm = TRUE)
EV$make[is.na(EV$make)] <- 'Unknown'
EV$model[is.na(EV$model)] <- 'Unknown'


# Manage Data Structure
# Arrange appropriate data types and formats of every columns / variables.

EV$model_year <- as.integer(EV$model_year)
EV$postal_code <- as.integer(EV$postal_code)
EV$EV_Range <- as.integer(EV$EV_Range)
EV$legislative_district <- as.integer(EV$legislative_district)
EV$DOL <- as.integer(EV$DOL)

# Convert data to Upper case which are not well readable in lower case
EV$country <- toupper(EV$country)
EV$city <- toupper(EV$city)

# Organizing Data to Appropriate readable format  
EV$CAFV_Eligibility <- ifelse(EV$CAFV_Eligibility == "Clean Alternative Fuel Vehicle Eligible", "Eligible", EV$CAFV_Eligibility)
EV$CAFV_Eligibility <- ifelse(EV$CAFV_Eligibility == "Not eligible due to low battery range", "Not Eligible", EV$CAFV_Eligibility)
EV$CAFV_Eligibility <- ifelse(EV$CAFV_Eligibility == "Eligibility unknown as battery range has not been researched", "Unkown", EV$CAFV_Eligibility)
View(EV)

# statistical Summary of EV data frame
summary(EV)
# it provides Minimum , Maximum , Mean , Median and Quarter values of each column / variable.



# ------------------------------------------------------------------------------
#        Producing Several histogram , scatter plot and boxplots etc.
# ------------------------------------------------------------------------------

# 1. Bar Graph of Average Electric Vehicle Range by EV_Makers

average_range_by_make <- EV |>
  group_by(make) |>
  summarise(Average_Electric_Range = mean(EV_Range, na.rm = TRUE)) |>
  arrange(desc(Average_Electric_Range))

ggplot(average_range_by_make, aes(x = reorder(make, Average_Electric_Range), y = Average_Electric_Range, fill = make)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(title = "Average Electric Range by Make", x = "EV_Maker", y = "Average Electric Range (miles)")

# 2. Electric vehicle Distribution according to CAFV Eligibility and Count

dis <- EV |> select(CAFV_Eligibility,make) |>  filter(make=='TESLA'|make=='NISSAN'|make=='CHEVROLET'|make=='FORD'|make=='BMW'|make=='KIA'|make=='TOYOTA'|make=='VOLKSWAGEN'|make=='JEEP'|make=='HYUNDAI')

ggplot(dis, aes(x = `CAFV_Eligibility`, fill = `make`)) +
  geom_bar() +
  ggtitle("Electric Vehicle Type Distribution as per CAFV Eligibility") +
  xlab("Electric Vehicle Type CAFV ") +
  ylab("Count") 


# Creating Bar Plots Based on Models by Top 8 Cars Makers 

# Modify the column names accordingly
top_makers = EV |>
  group_by(make) |> summarise(count = n()) |>
  arrange(desc(count)) |> top_n(8) |> data.frame()

top_models = EV |>
  filter(make %in% top_makers$make) |>
  group_by(make, model) |> summarise(Count = n()) |>
  arrange(desc(Count)) |>
  top_n(5, Count) |> data.frame()

# Create multiple bar charts
MakeModelBar = ggplot(top_models, aes(x = model, y = Count, fill = model)) +
  geom_bar(stat = "identity") +
  facet_wrap(~make, scales = "free") +
  labs(title = "Count of Models by Top 8 Car Makers", x = "Model",y = "Count",fill = "Model") +
  theme_classic() +
  scale_y_continuous(labels = scales::comma) +  # Use scales::comma for formatting labels
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::comma(Count)), color = "black", size = 3, vjust = -0.6) +
  guides(fill = "none") 

# Display the plot
print(MakeModelBar)



# ------------------------------------------------------------------------------
#     Inferential Statistics and Hypothesis Testing [ Null & Alternative ]
#                         One / Two sample test
# ------------------------------------------------------------------------------

# Question 1: Is there a significant difference in the electric range 
# between Tesla vehicles and vehicles of other makes?

# Hypothesis Testing : -
# Null Hypothesis (H0): There is no significant difference in the electric range
#                       between Tesla vehicles and vehicles of other makes.

# Alternative Hypothesis (H1): There is a significant difference in the electric range 
#                             between Tesla vehicles and vehicles of other makes.

# Two sample T Test 

# Subset data for Tesla vehicles and vehicles of other makes
tesla_data <- EV |> filter(make == "TESLA")
other_makes_data <- EV |> filter(make != "TESLA")

# Perform two-sample t-test
t_test_result <- t.test(tesla_data$EV_Range, other_makes_data$EV_Range)

# Print test results
print(t_test_result)

# Interpret the results
if (t_test_result$p.value < 0.05) {
  cat("Since the p-value is less than the significance level (0.05), we reject the null hypothesis.")
  cat("There is a significant difference in the electric range between Tesla vehicles and vehicles of other makes.")
} else {
  cat("Since the p-value is greater than or equal to the significance level (0.05), we fail to reject the null hypothesis.")
  cat("There is no significant difference in the electric range between Tesla vehicles and vehicles of other makes.")
}


# Question 2 : Is there a significant difference in the mean base MSRP of Electric Vehicles
# between different countries?

# Hypothesis Testing : 
# Null Hypothesis (H0): The mean base MSRP for Electric Vehicles is the same across all countries.
# Alternative Hypothesis (H1): There is a significant difference in the mean base MSRP for 
#                               Electric Vehicles across different countries.

# One sample T Test

alpha <- 0.05

grouped_data <- split(EV$base_msrp, EV$country)
anova_result <- aov(base_msrp ~ country, data = EV)

summary(anova_result)

p_value <- summary(anova_result)[[1]][["Pr(>F)"]][1]
cat("P-value:", p_value, "\n")

if (p_value < alpha) {
  cat("Reject the null hypothesis. There is a significant difference in mean base MSRP among the countries.\n")
} else {
  cat("Fail to reject the null hypothesis. There is no significant difference in mean base MSRP among the countries.\n")
}


# ------------------------------------------------------------------------------
#             Relationships between Variables in EV dataset
#  Hypothesis testing, Inferential Statistics, scatterplots & Regression testing
# ------------------------------------------------------------------------------


# Question 1: One-Sample T-Test for Electric Range between CAFV Eligible and Non-Eligible Vehicles
# CAFV Eligible and Non-Eligible vehicles have same range or not ?

# Subset data for CAFV eligible and non-eligible vehicles
caf_eligible_data <- EV |> filter(CAFV_Eligibility == "Eligible")
non_caf_eligible_data <- EV %>% filter(CAFV_Eligibility != "Not Eligible")

# Perform one-sample t-test
t_test_result <- t.test(caf_eligible_data$EV_Range, non_caf_eligible_data$EV_Range)

# Print test results
print(t_test_result)

# Interpret the results
if (t_test_result$p.value < 0.05) {
  cat("Since the p-value is less than 0.05, we reject the null hypothesis.\n")
  cat("CAFV eligible electric vehicles have a significantly higher average electric range compared to not eligible vehicles.\n")
} else {
  cat("Since the p-value is greater than or equal to 0.05, we fail to reject the null hypothesis.\n")
  cat("There is no significant difference in average electric range between CAFV eligible and not eligible electric vehicles.\n")
}

# Scatterplot of electric range for CAFV eligibility vs non-eligible vehicles
ggplot(EV, aes(x = EV_Range, color = CAFV_Eligibility)) +
  geom_point(aes(y = 1)) +
  ggtitle("Electric Range by CAFV Eligibility") +
  xlab("Electric Range") +
  ylab("CAFV Eligibility") +
  theme_minimal()

# Liner regression to compare electric range between CAFV eligibility and non-eligible vehicles
lm_model <- lm(EV_Range ~ CAFV_Eligibility, data =EV)
summary(lm_model)



# Question 2: Does the electric range of vehicles significantly vary based on the
# legislative district they belong to?

# install library 
library(ggplot2)

anova_model <- aov(EV_Range ~ as.factor(legislative_district),data=EV)
summary(anova_model)

# create a summarized dataframe for plotting
summary_data <- EV |> group_by(legislative_district) |> summarise(mean_range = mean(EV_Range))

# scatterplot
ggplot(EV,aes(x=as.factor(legislative_district), y=EV_Range, color=as.factor(legislative_district))) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm",se=FALSE,color="black")+
  labs(x="Legislative District",y="EV_Range",title = "Sactterplot of Ev_Range Vs. Legislative District")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45 , hjust = 1) )+
  scale_color_brewer(palette = "Set1")



# Question 3 :- Does the make of the vehicle significantly impact its electric range,
# controlling for other factors like model year and vehicle type? 

# Since 'Make' and 'Electric Vehicle Type' are categorical, we need to ensure they're treated as factors
EV$make <- as.factor(EV$make)
EV$EV_Type <- as.factor(EV$EV_Type)

# Conducting Multiple Linear Regression
model_Vehicle_ER <- lm(EV_Range ~ make + model_year + EV_Type, data = EV)

# Summary of the linear model to check coefficients and statistical significance
summary(model_Vehicle_ER)

# Interpretation:
# The summary provides coefficients for each level of the 'Make' variable (except the reference level),
# and the p-values associated with these coefficients will indicate whether the differences in makes are statistically significant,
# controlling for model year and vehicle type.

# We also want to check the diagnostics plots to ensure the assumptions of linear regression are met.
par(mfrow = c(2, 2))  # Organizing the plots in a 2x2 grid
plot(model_Vehicle_ER)  # Plotting diagnostic plots

# Interpretation:
# If the p-values for the 'Make' coefficients are less than 0.05, we reject the null hypothesis, suggesting that
# the make of the vehicle has a significant impact on its electric range, controlling for model year and vehicle type.
# If the p-values are greater than 0.05, we fail to reject the null hypothesis, suggesting that the make does not have a significant impact.

