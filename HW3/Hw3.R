# Read the data
data <- read.csv("data.csv")

# Filter data for those with HbA1c levels
hba1c_data <- data[!is.na(data$HbA1c_level), ]
# Create a function to categorize HbA1c levels
categorize_hba1c <- function(level) {
  if (level < 5.7) {
    return("Normal")
  } else if (level >= 5.7 & level <= 6.4) {
    return("Prediabetes")
  } else {
    return("Diabetes")
  }
}

# Apply the function to the data
hba1c_data$HbA1c_category <- sapply(hba1c_data$HbA1c_level, categorize_hba1c)

# Count of each category
cat("HbA1c Level Categories:\n")
table(hba1c_data$HbA1c_category)

# Filter data for those with BMI values
bmi_data <- data[!is.na(data$bmi), ]

# Create a function to categorize BMI levels
categorize_bmi <- function(bmi) {
  if (bmi < 30) {
    return("Normal Weight")
  } else if (bmi >= 30 & bmi < 35) {
    return("Obese (Class I)")
  } else if (bmi >= 35) {
    return("Obese (Class II/III)")
  } else {
    return("Undefined")
  }
}

# Apply the function to the data
bmi_data$BMI_category <- sapply(bmi_data$bmi, categorize_bmi)

# Count of each category
cat("\nBMI Categories:\n")
table(bmi_data$BMI_category)

# Filter data for diabetes and non-diabetes
diabetes_data <- data[data$diabetes == 1, ]
nondiabetes_data <- data[data$diabetes == 0, ]

# Boxplot for blood glucose levels
boxplot(
  diabetes_data$blood_glucose_level,
  nondiabetes_data$blood_glucose_level,
  names = c("Diabetes", "No Diabetes"),
  col = c("red", "blue"),
  ylab = "Blood Glucose Level",
  main = "Blood Glucose Level Comparison between Diabetes and Non-Diabetes"
)
