# Create a numeric vector
nums <- c(1, 2, 3, 4, 5, 5, 6)

# Calculate the mean
mean_value <- mean(nums)

# Calculate the median
median_value <- median(nums)

# Define a function to calculate the mode
calc_mode <- function(x) {
  # Get the unique values
  unique_vals <- unique(x)
  # Count frequency for each unique value
  freq <- tabulate(match(x, unique_vals))
  # Return the value with the highest frequency
  unique_vals[which.max(freq)]
}

# Calculate the mode
mode_value <- calc_mode(nums)

# Print the results
cat("Mean:", mean_value, "\n")
cat("Median:", median_value, "\n")
cat("Mode:", mode_value, "\n")
# Generate a sequence of numbers from 1 to 100
nums <- 1:100

# Extract even numbers
even_nums <- nums[nums %% 2 == 0]

# Calculate the sum of even numbers
sum_even <- sum(even_nums)

# Print the result
print(sum_even)
# Define multiple strings
str1 <- "Hello"
str2 <- "RStudio"
str3 <- "World!"

# Concatenate the strings using a comma and space as the separator
result <- paste(str1, str2, str3, sep = ", ")

# Print the concatenated string
print(result)
# 1. Using the paste() function to concatenate strings with a separator
str1 <- "Hello"
str2 <- "World"
str3 <- "from R!"
# Concatenate the strings with " - " as the separator
concatenated_string <- paste(str1, str2, str3, sep = " - ")
print(concatenated_string)
# Output: "Hello - World - from R!"

# 2. Creating a matrix and performing operations
# Create a 3x3 matrix
m <- matrix(c(4, 2, 3,
              1, 5, 6,
              7, 8, 9), nrow = 3, byrow = TRUE)
cat("Original Matrix:\n")
print(m)

# Transpose the matrix
m_transposed <- t(m)
cat("Transposed Matrix:\n")
print(m_transposed)

# Calculate the determinant of the matrix
m_det <- det(m)
cat("Determinant of the Matrix:\n")
print(m_det)

# Invert the matrix if it's invertible (determinant != 0)
if (m_det != 0) {
  m_inverse <- solve(m)
  cat("Inverse of the Matrix:\n")
  print(m_inverse)
} else {
  cat("The matrix is not invertible (determinant is 0).\n")
}
# Create a 3x3 matrix (by row)
m <- matrix(c(4, 2, 3,
              1, 5, 6,
              7, 8, 9), nrow = 3, byrow = TRUE)
cat("Original Matrix:\n")
print(m)

# Transpose the matrix
m_transpose <- t(m)
cat("\nTransposed Matrix:\n")
print(m_transpose)

# Calculate the determinant of the matrix
m_det <- det(m)
cat("\nDeterminant of the Matrix:\n")
print(m_det)

# Invert the matrix if it is invertible (i.e., determinant is not zero)
if (m_det != 0) {
  m_inverse <- solve(m)
  cat("\nInverse of the Matrix:\n")
  print(m_inverse)
} else {
  cat("\nThe matrix is not invertible (determinant is 0).\n")
}
# Install ggplot2 if not already installed
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
} else {
  library(ggplot2)
}

# Create a basic scatterplot using ggplot2
# Plotting miles per gallon (mpg) vs. weight (wt) from the mtcars dataset
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Scatterplot of MPG vs Weight",
       x = "Weight (1000 lbs)",
       y = "Miles Per Gallon") +
  theme_minimal()

# Load dplyr package
library(dplyr)

# Filter the iris dataset for species "setosa" and arrange by Sepal.Length
iris_filtered <- iris %>%
  filter(Species == "setosa") %>%
  arrange(Sepal.Length)

# Display the first few rows of the filtered and arranged dataset
head(iris_filtered)
install.packages("dplyr")
library(dplyr)
# Install tidyr package if it is not already installed
if (!require(tidyr)) {
  install.packages("tidyr")
  library(tidyr)
} else {
  library(tidyr)
}

# Load additional packages for data manipulation
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
} else {
  library(dplyr)
}

if (!require(tibble)) {
  install.packages("tibble")
  library(tibble)
} else {
  library(tibble)
}

# Create a sample dataset in wide format
data_wide <- tibble(
  id = 1:3,
  measure1 = c(10, 20, 30),
  measure2 = c(40, 50, 60)
)

cat("Wide dataset:\n")
print(data_wide)

# Pivot the dataset from wide to long format
data_long <- data_wide %>%
  pivot_longer(
    cols = c(measure1, measure2),  # Columns to pivot
    names_to = "measurement",       # New column for variable names
    values_to = "value"             # New column for values
  )

cat("\nLong dataset:\n")
print(data_long)

# Pivot the long format data back to wide format
data_wide_again <- data_long %>%
  pivot_wider(
    names_from = measurement,       # Variable names to spread out as columns
    values_from = value             # Values to fill the columns
  )

cat("\nWide dataset (after pivoting back):\n")
print(data_wide_again)
# Install data.table if not already installed
if (!require(data.table)) {
  install.packages("data.table")
  library(data.table)
} else {
  library(data.table)
}

# Create a sample data table
dt <- data.table(
  group = rep(c("A", "B", "C"), each = 4),
  value1 = c(5, 6, 7, 8, 10, 9, 12, 11, 20, 21, 19, 22),
  value2 = c(100, 150, 120, 130, 200, 210, 190, 205, 300, 310, 305, 320)
)

cat("Original Data Table:\n")
print(dt)

# Perform simple aggregation:
# For each group, compute:
# - Sum of value1
# - Mean of value1
# - Maximum of value2
agg_dt <- dt[, .(
  sum_value1 = sum(value1),
  mean_value1 = mean(value1),
  max_value2 = max(value2)
), by = group]

cat("\nAggregated Data Table:\n")
print(agg_dt)
# Install and load the lubridate package if not already installed
if (!require(lubridate)) {
  install.packages("lubridate")
  library(lubridate)
} else {
  library(lubridate)
}

# -------------------------------
# Parsing Date Strings
# -------------------------------

# Example 1: Parse a date in "YYYY-MM-DD" format using ymd()
date_str1 <- "2023-03-21"
parsed_date1 <- ymd(date_str1)
cat("Parsed date 1:", parsed_date1, "\n")
# Output: "2023-03-21"

# Example 2: Parse a date in "DD/MM/YYYY" format using dmy()
date_str2 <- "21/03/2023"
parsed_date2 <- dmy(date_str2)
cat("Parsed date 2:", parsed_date2, "\n")
# Output: "2023-03-21"

# Example 3: Parse a date in "Month day, Year" format using mdy()
date_str3 <- "March 21, 2023"
parsed_date3 <- mdy(date_str3)
cat("Parsed date 3:", parsed_date3, "\n")
# Output: "2023-03-21"

# -------------------------------
# Formatting Dates
# -------------------------------

# Format a parsed date into a more readable format using base R's format() function
formatted_date1 <- format(parsed_date1, format = "%B %d, %Y")  # e.g., "March 21, 2023"
cat("Formatted date 1:", formatted_date1, "\n")

# Alternatively, using lubridate functions to extract components
month_name <- month(parsed_date1, label = TRUE, abbr = FALSE)  # Full month name
day_value  <- day(parsed_date1)
year_value <- year(parsed_date1)

# Combine components to form a custom formatted date
formatted_date2 <- paste0(month_name, " ", day_value, ", ", year_value)
cat("Formatted date 2:", formatted_date2, "\n")
# Install rvest if not already installed
if (!require(rvest)) {
  install.packages("rvest")
  library(rvest)
} else {
  library(rvest)
}

# Specify the URL to scrape
url <- "https://example.com"

# Read the HTML content from the webpage
webpage <- read_html(url)

# Extract the page title using the <title> tag
page_title <- webpage %>% html_node("title") %>% html_text()
cat("Page Title:", page_title, "\n")

# Extract all hyperlinks from the page by selecting <a> tags and getting their href attribute
links <- webpage %>% html_nodes("a") %>% html_attr("href")
cat("Links on the page:\n")
print(links)
# Install caret package if not already installed
if (!require(caret)) {
  install.packages("caret")
  library(caret)
} else {
  library(caret)
}

# For decision tree, install rpart if not already installed
if (!require(rpart)) {
  install.packages("rpart")
  library(rpart)
} else {
  library(rpart)
}

# Set seed for reproducibility
set.seed(123)

# Load the iris dataset
data(iris)

# Partition the data into training (70%) and testing (30%) sets
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData  <- iris[-trainIndex, ]

# Train a classification model using caret with the rpart method (decision tree)
model <- train(Species ~ ., data = trainData, method = "rpart")

# Display model details
print(model)

# Make predictions on the test dataset
predictions <- predict(model, newdata = testData)

# Evaluate model performance using a confusion matrix
confMat <- confusionMatrix(predictions, testData$Species)
print(confMat)
# Install and load the necessary packages if not already installed
if (!require(xts)) {
  install.packages("xts")
  library(xts)
} else {
  library(xts)
}

if (!require(zoo)) {
  install.packages("zoo")
  library(zoo)
} else {
  library(zoo)
}

# Create sample time-series data
set.seed(123)  # for reproducibility
# Generate a sequence of monthly dates for one year
dates <- seq(as.Date("2021-01-01"), by = "month", length.out = 12)
# Create some random data for these dates
values <- rnorm(12, mean = 100, sd = 10)

# Convert the data into an xts object
ts_xts <- xts(values, order.by = dates)

# Plot the xts time-series data
plot(ts_xts, 
     main = "Time Series Visualization using xts", 
     xlab = "Date", 
     ylab = "Value", 
     col = "blue", 
     lwd = 2)

# Convert the xts object to a zoo object
ts_zoo <- as.zoo(ts_xts)

# Plot the zoo time-series data
plot(ts_zoo, 
     main = "Time Series Visualization using zoo", 
     xlab = "Date", 
     ylab = "Value", 
     col = "red", 
     lwd = 2)
