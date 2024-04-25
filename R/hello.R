# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function(x = NULL) {
  if (!is.null(x)) {
    if (is.character(x)) {
      if (x == "ca") {
        cat("
library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(maps)
library(dplyr)

# Read the CSV file (replace 'your_file.csv' with the actual file path)
df <- read.csv('D:/kaushik/MSC/MVAsem2/Sales_Product_Details.csv')
head(df, 2)

# Convert non-numerical col to numerical col
df$Product_Description <- as.numeric(factor(df$Product_Description))
df$Product_Line <- as.numeric(factor(df$Product_Line))
df$Product_Category <- as.numeric(factor(df$Product_Category))
df$Raw_Material <- as.numeric(factor(df$Raw_Material))
df$Region <- as.numeric(factor(df$Region))
head(df, 2)

# Check for null values and remove it
df1 <- na.omit(df)
null_count <- colSums(is.na(df1))
print(null_count)
head(df1)

# Standardize the data
df1 <- scale(df1)
head(df1, 2)

# Visualize optimal number of clusters using WCSS
fviz_nbclust(df1, kmeans, method = 'wss')

# Calculate gap statistic based on number of clusters
gap_stat <- clusGap(df1,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

# Plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

# Make this example reproducible
set.seed(1)

# Perform k-means clustering with k = 3 clusters
km1 <- kmeans(df1, centers = 3, nstart = 25)
km1

# Plot results of final k-means model
fviz_cluster(km1, data = df1)

# Find means of each cluster
cluster_means <- aggregate(df, by = list(cluster = km1$cluster), mean)
cluster_means

# Combine cluster assignments with original data
final_data <- cbind(df, cluster = km1$cluster)

# Function to create bar graphs for Product_ID against Quantity for all clusters together
plot_product_id_vs_quantity_all_clusters <- function(cluster_data) {
  ggplot(cluster_data, aes(x = as.factor(Product_ID), y = Quantity)) +
    geom_bar(stat = 'summary', fun = 'mean', fill = 'skyblue', color = 'black') +
    labs(title = 'Product ID vs. Quantity',
         x = 'Product ID', y = 'Average Quantity') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Create bar graph for all clusters together
plot_list <- list()
for (i in unique(final_data$cluster)) {
  cluster_subset <- final_data[final_data$cluster == i, ]
  plot_list[[i]] <- plot_product_id_vs_quantity_all_clusters(cluster_subset) +
    ggtitle(paste('Cluster', i))
}

# Combine plots into a single plot with subplots
all_plots <- do.call(grid.arrange, c(plot_list, ncol = 3))

# Display the combined plot
print(all_plots)

# Create pie charts for each cluster
plots <- lapply(unique(final_data$cluster), function(cluster_num) {
  cluster_subset <- final_data[final_data$cluster == cluster_num, ]
  region_counts <- table(cluster_subset$Region)
  pie_data <- data.frame(region = names(region_counts), count = as.numeric(region_counts))
  ggplot(pie_data, aes(x = '', y = count, fill = region)) +
    geom_bar(width = 1, stat = 'identity') +
    coord_polar(theta = 'y') +
    labs(title = paste('Region Distribution for Cluster', cluster_num)) +
    theme_void() +
    theme(legend.position = 'right')
})

# Combine plots into a single plot with subplots
combined_plot <- wrap_plots(plots, nrow = 1)

# Display the combined plot
print(combined_plot)

# Add cluster information to the dataset
df$Cluster <- as.factor(km1$cluster)

# Create scatter plot for latitude and longitude
# Plot map focusing on specified limits
world <- map_data('world')
plot_map <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = 'gray90', color = 'black') +
  geom_point(data = df, aes(x = Longitude, y = Latitude, color = Cluster), size = 3) +
  labs(title = 'Latitude and Longitude Scatter Plot') +
  theme_minimal() +
  scale_color_manual(values = c('blue', 'red', 'green')) +  # Define colors for clusters
  coord_cartesian(xlim = c(-10, 10), ylim = c(48, 55)) +  # Adjust the limits as specified
  geom_text(data = df, aes(label = Cluster  , x = Longitude, y = Latitude), size = 3, vjust = -0.5, position = position_jitter(width = 0.1, height = 0.1))  # Add jittering to text labels

# Display the plot
print(plot_map)

# Group points with the same coordinates and cluster, and print corresponding dates
grouped_df <- df %>%
  group_by(Latitude, Longitude, Cluster) %>%
  summarise(Dates = paste(Date, collapse = ', '))

print(grouped_df)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Compute the distance matrix using Euclidean distance
res.dist <- dist(df)

# Perform hierarchical clustering
#res.hc <- hclust(d = res.dist, method = 'ward.D2')

# Print the hierarchical clustering result
print(res.hc)

# Visualize the dendrogram with cluster membership
fviz_dend(res.hc, cex = 0.5, k = 3, color_labels = TRUE)

# Compute cophenetic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and the original distance
correlation <- cor(res.dist, res.coph)
print(correlation)

# Perform hierarchical clustering with a different method (optional)
res.hc2 <- hclust(res.dist, method = 'average')
correlation2 <- cor(res.dist, cophenetic(res.hc2))
print(correlation2)

# Visualize the dendrogram with cluster membership by average method
fviz_dend(res.hc2, cex = 0.5, k = 3, color_labels = TRUE)

# Determine the cluster membership
grp <- cutree(res.hc, k = 3)
print(grp)

# Display the distribution of clusters
table_grp <- table(grp)
print(table_grp)

        ")
      } else if (x == "fc") {
        cat('
library(corrplot)
library(GPArotation)
library(FactoMineR)
library(factoextra)
library(ggcorrplot)
library(ggplot2)
library(psych)
library(ltm)

#Factor Analysis
# Load the CSV file
dfff <- read.csv("D:/kaushik/MSC/MVAsem2/apple_quality.csv")
head(dfff)

#Convert to numeric
dfff$Quality <- as.numeric(factor(dfff$Quality))
head(dfff)

#count null value
null_count <- colSums(is.na(dfff))
print(null_count)

# Omit rows with NA values
nonnull <- na.omit(dfff)
head(nonnull)
null_count <- colSums(is.na(nonnull))
print(null_count)

numeric_columns <- nonnull[, sapply(nonnull, is.numeric)]

#Corelation
c1 <- cor(numeric_columns)
par(mar = c(1, 1, 1, 1))  # c(bottom, left, top, right)
corrplot(c1, method = "color", addCoef.col = "black", number.cex = 0.1)

# Parallel analysis (using the paran package) see fa actual and red line
optimal_num_factors_parallel <- fa.parallel(c1)$nfact

#Model Factor analysis
model <- fa(c1, 5, rotate = "promax")
loadings <- model$loadings
fa.diagram(loadings, factors = colnames(loadings), node.size = 2, mar = c(0.1, 0.1, 0.1, 0.1),
           node.color = "lightblue", arrow.color = "darkblue")

KMO(numeric_columns)
bartlett.test(numeric_columns)
cronbach.alpha(numeric_columns)

##################################################################################
##################################################################################

library(FactoMineR)
library(factoextra)
library(ggcorrplot)
library(ggplot2)

# Read the CSV file
d1<-(FactorAnalysis)

# Check for null values
null_count <- colSums(is.na(d1))
print(null_count)

# Normalization
data_normalized <- scale(d1)

# Get the correlation matrix of principal components
pc_cor1 <- cor(data_normalized)

# Perform PCA using prcomp
pca_result1 <- prcomp(pc_cor1)

# Plot the correlation matrix using ggcorrplot
ggcorrplot(pc_cor1, type = "upper", outline.color = "white", lab = TRUE, lab_size = 3)

# Summary of PCA results
summary(pca_result1)

# Biplot of the first two dimensions
fviz_pca_biplot(pca_result1, geom = "point", col.ind = "cos2", col.var = "contrib",
                gradient.cols = c("blue", "red"), repel = TRUE)
# Eigenvalues plot
fviz_eig(pca_result1, addlabels = TRUE)
fviz_cos2(pca_result1, choice = "var", axes = 1:2)
fviz_pca_var(pca_result1, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


#################################################################################
##################################################################################

# Load the CSV file
dfff <- read.csv("D:/kaushik/MSC/MVAsem2/apple_quality.csv")
head(dfff)

dfff$Quality <- as.numeric(factor(dfff$Quality))
head(dfff)

# Omit rows with NA values
nonnull <- na.omit(dfff)
head(nonnull)
null_count <- colSums(is.na(nonnull))
print(null_count)

numeric_columns <- nonnull[, sapply(nonnull, is.numeric)]
head(numeric_columns)
numeric_columns <- numeric_columns[, -which(names(numeric_columns) == "A_id")]
head(numeric_columns)

data_normalized <- scale(numeric_columns)

# Get the correlation matrix of principal components
pc_cor2 <- cor(data_normalized)

# Perform PCA using prcomp
pca_result2 <- prcomp(pc_cor2)

# Plot the correlation matrix using ggcorrplot
ggcorrplot(pc_cor2, type = "upper", outline.color = "white", lab = TRUE, lab_size = 3)

# Summary of PCA results
summary(pca_result2)

# Biplot of the first two dimensions
fviz_pca_biplot(pca_result2, geom = "point", col.ind = "cos2", col.var = "contrib",
                gradient.cols = c("blue", "red"), repel = TRUE)
# Eigenvalues plot
fviz_eig(pca_result2, addlabels = TRUE)
fviz_cos2(pca_result2, choice = "var", axes = 1:2)
fviz_pca_var(pca_result2, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)
            ')
      } else if (x == "log") {
        cat('
library(corrplot)
library(ggplot2)
library(psych)
library(DescTools)
library(factoextra)
library(paran)

#Import the training CSV
ap_train<- read.csv("D:/kaushik/MSC/MVAsem2/Logi data/ap_train.csv")

#Convert non-numerical col to numerical col
ap_train_encode <- c("Gender", "Customer.Type", "Type.of.Travel", "Class", "satisfaction")
for (col in ap_train_encode) {
  ap_train[[col]] <- as.numeric(factor(ap_train[[col]]))
}
head(ap_train,2)

# Convert values to 0 and 1
ap_train$satisfaction <- ifelse(ap_train$satisfaction == 2, 0, 1)
head(ap_train,3)

#Check for null value
ap_train_nn <- na.omit(ap_train)
null_count_ap <- colSums(is.na(ap_train_nn))
print(null_count_ap)

# Exclude columns "ID", "X", and "satisfaction"
cols_to_plot <- colnames(ap_train_nn)[!(colnames(ap_train_nn) %in% c("id", "X", "satisfaction"))]
variables <- c("v1", "v2", "v3", "v4")

# Set up the plotting layout
par(mfrow = c(1, length(variables)))

# Create a boxplot for each column
for (col in cols_to_plot) {
  boxplot(ap_train_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Specify the columns to Winsorize
cols_to_winsorize <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")

# Apply Winsorization to the specified columns
for (col in cols_to_winsorize) {
  ap_train_nn[[col]] <- Winsorize(ap_train_nn[[col]], probs = c(0.10, 0.90))  # Trim 5% from both tails
}

variables1 <- c("Customer.Type","Flight.Distance", "Checkin.service", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")
# Set up the plotting layout
par(mfrow = c(1, length(variables1)))

# Create a boxplot for each column
for (col in variables1) {
  boxplot(ap_train_nn[[col]], main = col)
}
# Reset the plotting layout
par(mfrow = c(1, 1))

# Convert the correlation matrix to a data frame
cap <- cor(ap_train_nn)
cor_df <- as.data.frame(as.table(cap))

# Create the ggplot2 correlation plot
names(cor_df) <- c("Variable1", "Variable2", "Correlation")
ggplot(cor_df, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), color = "black") +  # Add text labels
  scale_fill_gradient2(low = "blue", high = "orange", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   hjust=1, size = 8, face = "italic"),
        axis.text.y = element_text(size = 8, face = "italic"))


# Reset the plotting layout
par(mfrow = c(1, 1))

# Filter the correlation matrix for variables with correlation greater than 0.2 with "satisfaction"
satisfaction_correlation <- cap[abs(cap["satisfaction", ]) > 0.2, ]

# Extract variable names
relevant_variables <- rownames(satisfaction_correlation)

# Subset the original dataframe based on relevant variables
relevant_data <- ap_train_nn[, relevant_variables]

# Print the first few rows of the relevant data
head(relevant_data)

print(colnames(relevant_data))

#Factor Analysis
#Create a new Dataframe with all relevant Columns
Fa_cols <- c("Inflight.wifi.service", "Food.and.drink", "Online.boarding",
                   "Seat.comfort", "Inflight.entertainment", "On.board.service",
                   "Leg.room.service", "Baggage.handling", "Checkin.service",
                   "Inflight.service", "Cleanliness")

# Convert the correlation matrix to a data frame
fa_df <- relevant_data[Fa_cols]
capdr <- cor(fa_df)
cor_dfr <- as.data.frame(as.table(capdr))
print(capdr)

# Parallel analysis (using the paran package)
optimal_num_factors_parallel <- fa.parallel(capdr)$nfact

# Factor analysis
model <- fa(capdr , 3, rotate = "promax")
loadings <- model$loadings
fa.diagram(loadings, factors = colnames(loadings), node.size = 2, mar = c(0.1, 0.1, 0.1, 0.1),
           node.color = "lightblue", arrow.color = "darkblue")


# Getting factor scores
factor_names <- colnames(model$loadings)
print(factor_names)
out<-fa(fa_df,3,scores = "regression")
factor_scores=out$scores

# Add factors and thier scores to relevant_data dataframe
relevant_data <- cbind(relevant_data, factor_scores)
head(relevant_data,2)

# Build logistic regression model
logit_model <- glm(satisfaction ~ MR1 + MR2 + MR3 + Type.of.Travel + Class + Flight.Distance, data = relevant_data, family = "binomial")

# Summarize the model
summary(logit_model)

# Predict on test data see dataset and change
predicted <- predict(logit_model, newdata = ap_test_nn, type = "response")

# calculate accuracy
predicted_class <- ifelse(predicted > 0.5, 1, 0)  # Convert probabilities to classes
accuracy <- mean(predicted_class == ap_test_nn$satisfaction)
print(paste("Accuracy:", accuracy))
            ')
      } else if (x == "mba") {
        cat('
# Load required packages
library(arules)
library(arulesViz)

# Read the CSV file and convert it into transactions
transactions <- read.transactions(file = "D:/kaushik/MSC/MVAsem2/groceries.csv",
                                  format = "basket", sep = ",", rm.duplicates = TRUE)

# Explore the summary of your transaction data
summary(transactions)

# Plot the absolute item frequency plot
itemFrequencyPlot(transactions,
                  type = "absolute",
                  topN = 10,
                  horiz = TRUE,
                  main = "Absolute item frequency")

# Mine association rules with specified parameters
rules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.5))

# Sort rules by confidence in descending order(can change to lift and support)
rules_lift_sorted <- sort(rules, by = "confidence", decreasing = TRUE)

# Get the top 10 rules based on lift
top_10_rules <- head(rules_lift_sorted, n = 10)

# Print the top 10 rules using the inspect function
inspect(top_10_rules)

# Plot the top 10 rules
plot(top_10_rules, method = "graph")

# Plot the top 10 rules with customized appearance
plot(top_10_rules,
     method = "graph",           # Plot using the graph method
     engine = "htmlwidget",      # Use htmlwidget engine for more interactive visualization
     nodeColors = c("blue", "orange", "darkgreen"), # Custom color scheme for nodes (blue, orange, red)
     edgeColors = "red",    # Color for edges
     edgeWidth = 2,              # Thicker edges for better visibility
     vertex.label.cex = 1.2,     # Larger label size for better readability
     vertex.size = 4,            # Larger node size for better visibility
     layout = "fruchterman_reingold", # Layout algorithm for improved visual arrangement
     main = "Top 10 Association Rules" # Title of the graph
)

            ')
      } else if (x == "sem") {
        cat("
library(lavaan)
library(semPlot)
library(OpenMx)
library(tidyverse)
library(knitr)
library(kableExtra)
library(GGally)
library(ggcorrplot)
library(cfa)


data(PoliticalDemocracy)
head(PoliticalDemocracy)

model <- '
            # measurement model
            ind60 =~ x1 + x2 + x3
            dem60 =~ y1 + y2 + y3 + y4
            dem65 =~ y5 + y6 + y7 + y8
            # regressions
            dem60 ~ ind60
            dem65 ~ ind60 + dem60
            # residual correlations
            y1 ~~ y5
            y2 ~~ y4 + y6
            y3 ~~ y7
            y4 ~~ y8
            y6 ~~ y8
            '

fit <- sem(model, data=PoliticalDemocracy)
summary(fit, standardized=TRUE)

semPaths(fit, what = 'std', layout = 'tree', edge.label.cex=.9, curvePivot = TRUE)
semPaths(fit, whatLabels = 'est', style='lisrel',main='SEM Diagram')

################################################################################
################################################################################

?HolzingerSwineford1939
data(HolzingerSwineford1939)
head(HolzingerSwineford1939)

model <-'# measurement model
  visual =~ x1 + x2 + x3
  textual =~ x4 + x5+ x6
  speed =~ x7 + x8 + x9
   # regressions
    visual ~ textual
    textual ~ speed
    visual ~ speed
  # residual correlations
    x1 ~~ x2 + x3
    x4 ~~ x6
    x5 ~~ x6
    x7 ~~ x8
    x8 ~~ x9
'
fit <- sem(model, data=HolzingerSwineford1939)
summary(fit, standardized=TRUE)

semPaths(fit, whatLabels = 'est', style='lisrel',main='SEM Diagram')
            ")
      } else if (x == "cf") {
        cat("
#Confirmatory Factor Anaylsis

library(lavaan)
library(semPlot)
library(tidyverse)
library(kableExtra)
library(GGally)
library(cfa)

HSmodel <-'# measurement model
            visual =~ x1 + x2 + x3
            textual =~ x4 + x5+ x6
            speed =~ x7 + x8 + x9

            x1 ~~ x4
            x2 ~~ x5
            x3 ~~ x6
            x4 ~~ x7
            x5 ~~ x8
            x6 ~~ x9'

HSfit <- cfa(HSmodel, data=HolzingerSwineford1939)
summary(HSfit, standardized=TRUE)

#Visualize the result (optional)
inspect(fit, 'std.lv')

# Plot the standardized factor loadings
semPaths(fit, 'std', layout = 'tree2')
semPaths(HSfit, whatLabels = 'est', style='lisrel',main='cfa diagram')
            ")
      } else if (x == "lqda") {
        cat('
#Daibetes dataset
library(klaR)
library(psych)
library(MASS)

# LDA

# Read the dataset
df <- read.csv("diabetes.csv")

# Train LDA model
model_lda <- lda(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = df)
model_lda
plot(model_lda)
# Summary of the LDA model
summary(model_lda)

# Make predictions on the training set using LDA
predictions_lda <- predict(model_lda)
predictions_lda
# Access class means and priors for LDA
class_means_lda <- model_lda$means
priors_lda <- model_lda$prior

# Access coefficients for linear discriminants
coefficients_lda <- model_lda$scaling
coefficients_lda

# Access the confusion matrix for LDA
conf_matrix_lda <- table(predictions_lda$class, df$Outcome)
print("Confusion Matrix for LDA:")
print(conf_matrix_lda)


# Train QDA model
model_qda <- qda(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = df)
model_qda


# Make predictions on the training set using QDA
predictions_qda <- predict(model_qda)
predictions_qda



# Access the confusion matrix for QDA
conf_matrix_qda <- table(predictions_qda$class, df$Outcome)
print("Confusion Matrix for QDA:")
print(conf_matrix_qda)


# roc curve

# Load necessary libraries
library(MASS)
library(pROC)

# Read the dataset
df <- read.csv("diabetes.csv")

# Specify the formula for the discriminant analysis
formula <- Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age
formula
# Train LDA model
model_lda <- lda(formula, data = df)
model_lda

# Train QDA model
model_qda <- qda(formula, data = df)
model_qda
# Make predictions on the training set
predictions_lda <- as.numeric(predict(model_lda)$class == "1")
predictions_qda <- as.numeric(predict(model_qda)$class == "1")

# Create ROC curves
roc_lda <- roc(df$Outcome, predictions_lda)
roc_qda <- roc(df$Outcome, predictions_qda)

# Plot ROC curves
plot(roc_lda, col = "blue", main = "ROC Curves", lwd = 2, col.main = "black", ylim = c(0, 1), xlim = c(0, 1), xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_qda, col = "red", lwd = 2)

# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lwd = 2)

###############################################################################################
###############################################################################################

#Breast Cancer
library(klaR)
library(psych)
library(MASS)
library(pROC)

# LDA

# Read the dataset
df <- read.csv("breastCancer.cs")
df

df <- df[, -c(1, ncol(df))]
df
df$diagnosis <- factor(df$diagnosis)

lda_model <- lda(diagnosis ~ ., data = df)
lda_model


qda_model <- qda(diagnosis ~ ., data = df)
qda_model

# Predictions using LDA
lda_pred <- predict(lda_model)
lda_pred
lda_conf_matrix <- table(Actual = df$diagnosis, Predicted = lda_pred$class)
lda_conf_matrix
# Print the confusion matrix for LDA
print("Confusion Matrix for LDA:")
print(lda_conf_matrix)

# Predictions using QDA
qda_pred <- predict(qda_model)
qda_pred
qda_conf_matrix <- table(Actual = df$diagnosis, Predicted = qda_pred$class)
qda_conf_matrix
# Print the confusion matrix for QDA
print("Confusion Matrix for QDA:")
print(qda_conf_matrix)

# Coefficients of LDA
lda_coefficients <- coef(lda_model)
print("Coefficients of LDA:")
print(lda_coefficients)

# Coefficients of QDA
qda_coefficients <- coef(qda_model)
print("Coefficients of QDA:")
print(qda_coefficients)

str(df$diagnosis)
str(lda_model$posterior)

# Inspect the lda_model object
str(lda_model)

# Check for missing values in the data
any(is.na(df))

class(lda_pred)
str(lda_pred)

# Create ROC curve for LDA
roc_lda <- roc(df$diagnosis, lda_pred$posterior[, "M"], levels = c("B", "M"))

# Create ROC curve for QDA
roc_qda <- roc(df$diagnosis, qda_pred$posterior[, "M"], levels = c("B", "M"))

# Plot ROC curve for LDA
plot(roc_lda, col = "blue", main = "ROC Curve for LDA")

# Add ROC curve for QDA
plot(roc_qda, col = "red", add = TRUE)

# Add legend
legend("bottomright", legend = c("LDA", "QDA"), col = c("blue", "red"), lty = 1)

            ')
      } else if (x == "kp") {
        cat('
# Load required library
library(kernlab)

# Load the iris dataset
data(iris)

# Separate features and labels
X <- iris[, -5] # Features
y <- iris[, 5] # Labels

# Perform Kernel PCA
kpca_model <- kpca(~., data = X, kernel = "rbfdot", kpar = list(sigma = 0.1))

# Get the transformed data
X_transformed <- as.data.frame(predict(kpca_model, X))

# Print the transformed data
head(X_transformed)

# Plot the transformed data
plot(X_transformed[,1], X_transformed[,2], col = y, pch = 19,
 xlab = "Principal Component 1", ylab = "Principal Component 2")
# Add legend
legend("topright", legend = levels(iris$Species), col = 1:3, pch = 19, title = "Species")
# Add title
title("Kernel PCA on Iris Dataset")

# Highlight components with high variance
points(X_transformed[,1][which.max(var(X_transformed))],
       X_transformed[,2][which.max(var(X_transformed))], col= "red", pch = 20, cex = 2)

points(X_transformed[,1][which.min(var(X_transformed))],
       X_transformed[,2][which.min(var(X_transformed))], col= "blue", pch = 20, cex = 2)

            ')
      } else if (x == "fp") {
        cat("
# Load the required package
library(kernlab)

# Load the built-in 'iris' dataset
data(iris)

# Extract the features from the 'iris' dataset
features <- iris[, 1:4]

# Define a kernel function (e.g., radial basis function kernel)
kernel <- rbfdot(sigma = 0.1)

# Convert features to matrix
features_matrix <- as.matrix(features)

# Perform Kernel PCA
kpca_result <- kpca(features_matrix, kernel = kernel, features = 2)

# Extract the projected data
projected_data <- as.data.frame(predict(kpca_result, features_matrix))

# Plot the projected data
plot(projected_data, col = iris$Species, pch = 20, main = 'Kernel PCA on Iris Dataset')
legend('topleft', legend = unique(iris$Species), col = 1:length(unique(iris$Species)), pch = 20)

            ")
      } else if (x == "cj") {
        cat("
library(cautilities)
library(conjoint)

conjointAnalyis = read.csv('pizza_data.csv')
str(conjointAnalyis)
head(conjointAnalyis)


# converting into char to numeric

conjointAnalyis$brand = as.numeric(factor(conjointAnalyis$brand))
conjointAnalyis$price = as.numeric(factor(conjointAnalyis$price))
conjointAnalyis$weight = as.numeric(factor(conjointAnalyis$weight))
conjointAnalyis$crust = as.numeric(factor(conjointAnalyis$crust))
conjointAnalyis$cheese = as.numeric(factor(conjointAnalyis$crust))
conjointAnalyis$size = as.numeric(factor(conjointAnalyis$size))
conjointAnalyis$toppings = as.numeric(factor(conjointAnalyis$toppings))
conjointAnalyis$spicy = as.numeric(factor(conjointAnalyis$spicy))

# removing na values

colSums(is.na(conjointAnalyis))
conjointAnalyis = na.omit(conjointAnalyis)
conjointAnalyis

# removing last col from dataset
conjointAnalyis = conjointAnalyis[,-ncol(conjointAnalyis)]
conjointAnalyis

# creating data with 10 observation and 16 profile
tprefm = matrix(sample(0:16 , 10*16 , replace = TRUE) , ncol = 16)

colnames(tprefm) = paste0('profile',1:16)
tprefm = as.data.frame(tprefm)
str(tprefm)

# Extracting columns names from dataset

col_names = colnames(conjointAnalyis)
col_names

caUtilities(y=tprefm[1,], x=conjointAnalyis, z=col_names)

conjoint_model = Conjoint(y=tprefm , x = pizza_data , z = col_names)
            ")
      } else {
        cat("Input is\n",
            'ca = "Cluster Analysis Kmeans & HCA dendogram"\n',
            'fc = "Exploratory Factor Analysis & PCA"\n',
            'log = "Logistic Reg with factor analysis"\n',
            'mba = "Market Basket Analysis"\n',
            'sem = "Structural Equation Modiling"\n',
            'cf = "Conformatory Factor Analysis"\n',
            'lqda = "LDA and QDA"\n',
            'kp = "Kernal PCA"\n',
            'fp = "Functional PCA"\n',
            'cj = "Conjoint analysis"\n'
        )
      }
    } else {
      cat("Enter input in character not number.\n")
    }
  }
}
