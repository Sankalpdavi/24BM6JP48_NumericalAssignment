
head(datasets::iris)

###################------------------------------Dataset-3-----------------------------##########################

# Loading the Library
library(ggplot2)

# Loading the Data set
dataset4<- datasets::iris

#Numerical Variable choosen for this dataset - sepal length
X <- dataset4$Sepal.Length

#----------------------------------------Univariate Analysis-------------------------------------------------------

#-----------------------------------------Solving Question-1-------------------------------------------------------

# Inspecting the Data
head(dataset4)       # View the first few rows
str(dataset4)        # View the structure

# Calculating Number of Observations and Variables
cat("Number of observations:", nrow(dataset4), "\n") # Number of observations (rows)
cat("Number of variables:", ncol(dataset4), "\n")    # Number of variables (columns)


#----------------------------------------Solving Question-2--------------------------------------------

## Calculating the mean, median, standard deviation, minimum,and maximum of the Variable and Printing the result
mean_age <- mean(X, na.rm = FALSE)     # Calculating mean
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


# Histogram for sepal length (X) 
# Using Sturges' method for bin width
cat("\nUsing Sturges' method for bin width calculation")
hist(X, breaks = "Sturges", main = "Histogram of sepal length",
     xlab = "sepal length", col = "lightgreen",border = "black")

cat("Frequency distribution for sepal length (Discrete):\n")
table_age <- table(X)
print(table_age)

# For the discrete variable (sepal length), displaying the box plot
boxplot(table_age, main = "Boxplot of sepal length",
        ylab = "sepal length", col = "lightgreen",horizontal = TRUE)

#----------------------------------------Solving Question-4--------------------------------------------

Y <- dataset4$Species

# Calculate the frequency of each category in "Species"
counts <- table(Y)

# Create a bar plot
barplot(counts, 
        main = "Bar Plot of Species Variable", 
        xlab = "Species", 
        ylab = "Frequency", 
        col = c("skyblue", "pink"))

# Add labels to the bars
text(x = 1:length(counts), 
     y = counts, 
     labels = counts, 
     pos = 1, col = "black")


#----------------------------------------Multivariate Analysis-------------------------------------------------------

#----------------------------------------Solving Question-5---------------------------------------------------------

# Assuming your dataset has columns 'sepal length' and 'sepal width'
pearson_correlation <- cor(dataset4$Sepal.Length, dataset4$Sepal.Width, method = "pearson")

# Print the result
cat("Pearson correlation coefficient between 'sepal length' and 'sepal width':", pearson_correlation, "\n")

#----------------------------------------Solving Question-6---------------------------------------------------------

# Creating the scatter plot
plot(dataset4$Sepal.Length, dataset4$Sepal.Width, 
     main = "Scatter Plot of sepal length vs. sepal width", 
     xlab = "sepal length", 
     ylab = "sepal width", 
     pch = 19,              # Solid circle for points
     col = "lightgreen")          # Color of points

# Add a linear regression trend line
abline(lm(dataset4$Sepal.Width ~ dataset4$Sepal.Length), col = "red", lwd = 2)  # Red trend line

#----------------------------------------Solving Question-7-AND-Question-8------------------------------------------------------

#Fitting a MLR model 
mlr_model <- lm(Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width, data = dataset4)

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

dataset4_numeric<- dataset4[, c("Petal.Length","Sepal.Length", "Sepal.Width" , "Petal.Width")]

# Perform PCA with Standardizing the data
pca_result <- prcomp(dataset4_numeric, scale. = TRUE)

# View the summary of PCA results
summary(pca_result)

# Plot the proportion of variance explained by each component
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(explained_variance, 
     main = "Explained Variance by Principal Components", 
     xlab = "Principal Component", 
     ylab = "Variance Explained", 
     type = "b", pch = 19, col = "blue")

# Create a bi plot
biplot(pca_result, 
       main = "PCA Biplot", 
       col = c("blue", "red"), 
       xlabs = rep("", nrow(dataset4)))

#---------------------------------***************************************************-----------------------------
