
###################------------------------------Dataset-2-----------------------------##########################

# Loading the Library
library(ggplot2)
# Loading the Data set
dataset2<- datasets::ChickWeight

#Numerical Variable choosen for this dataset - Weight
X <- dataset2$weight 

#----------------------------------------Univariate Analysis-------------------------------------------------------

#-----------------------------------------Solving Question-1-------------------------------------------------------

# Inspecting the Data
head(dataset2)       # View the first few rows
str(dataset2)        # View the structure

# Calculating Number of Observations and Variables
cat("Number of observations:", nrow(dataset2), "\n") # Number of observations (rows)
cat("Number of variables:", ncol(dataset2), "\n")    # Number of variables (columns)


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


# Histogram for Weight (X) 
# Using Sturges' method for bin width
cat("\nUsing Sturges' method for bin width calculation")
hist(X, breaks = "Sturges", main = "Histogram of Weight",
     xlab = "Weight", col = "lightgreen",border = "black")

cat("Frequency distribution for Weight (Discrete):\n")
table_age <- table(X)
print(table_age)

# For the discrete variable (Weight), displaying the box plot
boxplot(table_age, main = "Boxplot of Weight",
        ylab = "Weight", col = "lightgreen",horizontal = TRUE)

#----------------------------------------Solving Question-4--------------------------------------------

Y <- dataset2$Diet

# Calculate the frequency of each category in "Diet"
counts <- table(Y)

# Create a bar plot
barplot(counts, 
        main = "Bar Plot of Diet Variable", 
        xlab = "Diet", 
        ylab = "Frequency", 
        col = c("skyblue", "pink"))

# Add labels to the bars
text(x = 1:length(counts), 
     y = counts, 
     labels = counts, 
     pos = 1, col = "black")


#----------------------------------------Multivariate Analysis-------------------------------------------------------

#----------------------------------------Solving Question-5---------------------------------------------------------

# Assuming your dataset has columns 'Weight' and 'Time'
pearson_correlation <- cor(dataset2$weight, dataset2$Time, method = "pearson")

# Print the result
cat("Pearson correlation coefficient between Weight and Time:", pearson_correlation, "\n")

#----------------------------------------Solving Question-6---------------------------------------------------------

# Creating the scatter plot
plot(dataset2$weight, dataset2$Time, 
     main = "Scatter Plot of Weight vs. Time", 
     xlab = "Weight", 
     ylab = "Time", 
     pch = 19,              # Solid circle for points
     col = "lightgreen")          # Color of points

# Add a linear regression trend line
abline(lm(dataset2$Time ~ dataset2$weight), col = "red", lwd = 2)  # Red trend line

#----------------------------------------Solving Question-7-AND-Question-8--------------------------------------------------------
weight<- dataset2$weight 
time<- dataset2$Time 
chick<- dataset2$Chick 
diet<- dataset2$Diet 

#Fitting a MLR model on Charges using all columns
mlr_model <- lm(weight ~ diet + time , data = dataset2)

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

dataset2_numeric<- dataset2[, c("Time", "weight")]

# Perform PCA with Standardizing the data
pca_result <- prcomp(dataset2_numeric, scale. = TRUE)

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
       xlabs = rep("", nrow(dataset2)))

#---------------------------------***************************************************-----------------------------
