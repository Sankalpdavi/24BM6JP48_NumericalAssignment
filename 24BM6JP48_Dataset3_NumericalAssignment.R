
###################------------------------------Dataset-3-------mtcars-----------------##########################

# Loading the Library
library(ggplot2)

# Loading the Data set
dataset3<- datasets::mtcars

#Numerical Variable choosen for this dataset - mpg
X <- dataset3$mpg

#----------------------------------------Univariate Analysis-------------------------------------------------------

#-----------------------------------------Solving Question-1-------------------------------------------------------

# Inspecting the Data
head(dataset3)       # View the first few rows
str(dataset3)        # View the structure

# Calculating Number of Observations and Variables
cat("Number of observations:", nrow(dataset3), "\n") # Number of observations (rows)
cat("Number of variables:", ncol(dataset3), "\n")    # Number of variables (columns)


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


# Histogram for Mpg (X) 
# Using Sturges' method for bin width
cat("\nUsing Sturges' method for bin width calculation")
hist(X, breaks = "Sturges", main = "Histogram of mpg",
     xlab = "mpg", col = "lightgreen",border = "black")

cat("Frequency distribution for mpg (Discrete):\n")
table_age <- table(X)
print(table_age)

# For the discrete variable (mpg), displaying the box plot
boxplot(table_age, main = "Boxplot of mpg",
        ylab = "Mpg", col = "lightgreen",horizontal = TRUE)

#----------------------------------------Solving Question-4--------------------------------------------

Y <- dataset3$gear

# Calculate the frequency of each category in "Gear"
counts <- table(Y)

# Create a bar plot
barplot(counts, 
        main = "Bar Plot of Gear Variable", 
        xlab = "No. of Gears", 
        ylab = "Frequency", 
        col = c("skyblue", "pink"))

# Add labels to the bars
text(x = 1:length(counts), 
     y = counts, 
     labels = counts, 
     pos = 1, col = "black")


#----------------------------------------Multivariate Analysis-------------------------------------------------------

#----------------------------------------Solving Question-5---------------------------------------------------------

# Assuming your dataset has columns 'hp' and 'wt'
pearson_correlation <- cor(dataset3$hp, dataset3$wt, method = "pearson")

# Print the result
cat("Pearson correlation coefficient between hp and wt:", pearson_correlation, "\n")

#----------------------------------------Solving Question-6---------------------------------------------------------

# Creating the scatter plot
plot(dataset3$hp, dataset3$wt, 
     main = "Scatter Plot of hp vs. wt", 
     xlab = "hp", 
     ylab = "wt", 
     pch = 19,              # Solid circle for points
     col = "lightgreen")          # Color of points

# Add a linear regression trend line
abline(lm(dataset3$wt ~ dataset3$hp), col = "red", lwd = 2)  # Red trend line

#----------------------------------------Solving Question-7-AND-Question-8------------------------------------------------------

#Fitting a MLR model
mlr_model <- lm(mpg ~ hp + wt, data = dataset3)

# Summary of the model
summary(mlr_model)

# Calculating fitted values
fitted_values <- fitted(mlr_model)

# Calculating residuals
residuals <- residuals(mlr_model)

# Creating the plots

# Plot 1: Residuals vs Fitted
plot(mlr_model, which=1,col = "red")

# Plot 2: Normal Q-Q
plot(mlr_model, which=2,col = "red")

# Plot 3: Scale-Location (Spread-Location)
plot(mlr_model, which=3,col = "red")

# Plot 4: Histogram of Residuals
hist(residuals, breaks = 20, probability = TRUE,  # Enables density overlay
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue")
lines(density(residuals), col = "red", lwd = 2)  # Adds a density line


#----------------------------------------Solving Question-9-AND-Question-10------------------------------------------------------


# Perform PCA with Standardizing the data
pca_result <- prcomp(dataset3, scale. = TRUE)

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
       xlabs = rep("", nrow(dataset3)))

#---------------------------------***************************************************-----------------------------
