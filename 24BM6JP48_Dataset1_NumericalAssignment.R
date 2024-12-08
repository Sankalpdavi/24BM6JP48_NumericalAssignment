
###################------------------------------Dataset-1-----------------------------##########################

# Loading the Library
library(ggplot2)
# Dataset 1 Download Link :- https://www.kaggle.com/datasets/mirichoi0218/insurance/data------------------------

# Changing the directory to where the CSV of Data set is present
getwd()
setwd("C:/Users/Sankalp Davi/OneDrive/PGDBA/ISI/SSD/Assignment")

# Loading the Data set
dataset1 <- read.csv("insurance_raw.csv")

#Numerical Variable choosen for this dataset - AGE
X <- dataset1$age 

#----------------------------------------Univariate Analysis-------------------------------------------------------

#-----------------------------------------Solving Question-1-------------------------------------------------------

# Inspecting the Data
head(dataset1)       # View the first few rows
str(dataset1)        # View the structure

# Calculating Number of Observations and Variables
cat("Number of observations:", nrow(dataset1), "\n") # Number of observations (rows)
cat("Number of variables:", ncol(dataset1), "\n")    # Number of variables (columns)


#----------------------------------------Solving Question-2--------------------------------------------

## Calculating the mean, median, standard deviation, minimum,and maximum of the Variable and Printing the result
mean_age <- mean(X, na.rm = TRUE)     # Calculating mean
cat("Mean:", mean_age, "\n")

median_age <- median(X, na.rm = TRUE) # Calculating median
cat("Median:", median_age, "\n")

sd_age <- sd(X, na.rm = TRUE)         # Calculating standard deviation
cat("Standard Deviation:", sd_age, "\n")

min_age <- min(X, na.rm = TRUE)       # Calculating minimum
cat("Minimum:", min_age, "\n")

max_age <- max(X, na.rm = TRUE)       # Calculating maximum
cat("Maximum:", max_age, "\n")


#----------------------------------------Solving Question-3--------------------------------------------


# Histogram for age (X) 
# Using Sturges' method for bin width
cat("\nUsing Sturges' method for bin width calculation")
hist(X, breaks = "Sturges", main = "Histogram of Age",
     xlab = "AGE", col = "lightgreen",border = "black")

cat("Frequency distribution for Age (Discrete):\n")
table_age <- table(X)
print(table_age)

# For the discrete variable (Age), displaying the box plot
boxplot(table_age, main = "Boxplot of AGE",
        ylab = "AGE", col = "lightgreen",horizontal = TRUE)


#----------------------------------------Solving Question-4--------------------------------------------

Y <- dataset1$smoker

# Calculate the frequency of each category in "Smoker"
counts <- table(Y)
table(Y)

# Create a bar plot
barplot(counts, 
        main = "Bar Plot of Somker Variable", 
        xlab = "Smoker", 
        ylab = "Frequency", 
        col = c("skyblue", "pink"))

# Add labels to the bars
text(x = 1:length(counts), 
     y = counts, 
     labels = counts, 
     pos = 1, col = "black")


#----------------------------------------Multivariate Analysis-------------------------------------------------------

#----------------------------------------Solving Question-5---------------------------------------------------------

# Assuming your dataset has columns 'Age' and 'BMI'
pearson_correlation <- cor(dataset1$age, dataset1$bmi, method = "pearson")

# Print the result
cat("Pearson correlation coefficient between Age and BMI:", pearson_correlation, "\n")

#----------------------------------------Solving Question-6---------------------------------------------------------

# Creating the scatter plot
plot(dataset1$age, dataset1$bmi, 
     main = "Scatter Plot of Age vs. BMI", 
     xlab = "Age", 
     ylab = "BMI", 
     pch = 19,              # Solid circle for points
     col = "lightgreen")          # Color of points

# Add a linear regression trend line
abline(lm(dataset1$bmi ~ dataset1$age), col = "red", lwd = 2)  # Red trend line


#----------------------------------------Solving Question-7-AND-Question-8--------------------------------------------------------
charges<- dataset1$charges 
smoker<- dataset1$smoker 
age<- dataset1$age 
bmi<- dataset1$bmi 
sex<- dataset1$sex 
children<- dataset1$children 
region<- dataset1$region 

#Fitting a MLR model on Charges using all columns
mlr_model <- lm(charges ~ smoker + age + bmi + sex + children + region, data = dataset1)

# Summary of the model
summary(mlr_model)

# Calculating fitted values
fitted_values <- fitted(mlr_model)

# Calculating residuals
residuals <- residuals(mlr_model)

# Creating the plots

# Plot 1: Residuals vs Fitted
plot(mlr_model, which=1, main="Residuals vs Fitted",col = "red")

# Plot 2: Normal Q-Q
plot(mlr_model, which=2, main="Normal Q-Q",col = "red")

# Plot 3: Scale-Location (Spread-Location)
plot(mlr_model, which=3, main="Scale-Location",col = "red")

# Plot 4: Histogram of Residuals
hist(residuals, breaks = 20, probability = TRUE,  # Enables density overlay
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue")
lines(density(residuals), col = "red", lwd = 2)  # Adds a density line


# TRYING A DIFFERENT MODEL

#Fitting a MLR model on all columns except Sex & As it seems Smoker & BMI has a interaction putting it in the model. 
mlr_model <- lm(charges ~ smoker + age^2 + bmi + children + region + smoker*bmi, data = dataset1)

# Summary of the model
summary(mlr_model)

# Calculating fitted values
fitted_values <- fitted(mlr_model)

# Calculating residuals
residuals <- residuals(mlr_model)

# Creating the plots

# Plot 1: Residuals vs Fitted
plot(mlr_model, which=1, main="Residuals vs Fitted",col = "red")

# Plot 2: Normal Q-Q
plot(mlr_model, which=2, main="Normal Q-Q",col = "red")

# Plot 3: Scale-Location (Spread-Location)
plot(mlr_model, which=3, main="Scale-Location",col = "red")

# Plot 4: Histogram of Residuals
hist(residuals, breaks = 20, probability = TRUE,  # Enables density overlay
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue")
lines(density(residuals), col = "red", lwd = 2)  # Adds a density line


#----------------------------------------Solving Question-9-AND-Question-10------------------------------------------------------

dataset1_numeric<- dataset1[, c("age", "bmi", "charges")]

# Perform PCA with Standardizing the data
pca_result <- prcomp(dataset1_numeric, scale. = TRUE)

# View the summary of PCA results
summary(pca_result)

# Plot the proportion of variance explained by each component
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(explained_variance, 
     main = "Explained Variance by Principal Components", 
     xlab = "Principal Component", 
     ylab = "Variance Explained", 
     type = "b", pch = 19, col = "blue")

# Create a biplot
biplot(pca_result, 
       main = "PCA Biplot", 
       col = c("blue", "red"), 
       xlabs = rep("", nrow(dataset1)))


#---------------------------------***************************************************-------------------------------




