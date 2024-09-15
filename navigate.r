# nolint start

library(poLCA)
library(readxl)
library(openxlsx)

# x <- file.exists("/Users/milenapetrovic/UFL Dropbox/Milena Petrovic/dataCopy/processed_data/training/navigation_training_processed/main_data/nav_training_main_data_2024_05_30.csv") 
# print(x) 

#data <- read.csv("/Users/milenapetrovic/UFL Dropbox/Milena Petrovic/dataCopy/processed_data/training/navigation_training_processed/main_data/nav_training_main_data_2024_05_30.csv", header = TRUE, sep = ",") 
#df <- read_excel("/Users/milenapetrovic/Downloads/nav-training-analysis-2/nav_metrics.xlsx")
df <- read_excel("nav_metrics.xlsx")

#create columns for df_env
df$Environment1 <- as.factor(grepl("A", df$Environment))
df$Environment2 <- as.factor(grepl("B", df$Environment))
df$Environment3 <- as.factor(grepl("A,B", df$Environment))
df$Environment4 <- as.factor(grepl("C", df$Environment))

#### all instances of `Unique Pairs` can be replaced with `Learning Efficicency`, `Path Accuracy`, ...  ####
df$`Unique Pairs` <- as.factor(df$`Unique Pairs`)    

# specify rows?
df_env <- df[, c("Environment1", "Environment2", "Environment3", "Environment4", "Unique Pairs")]

# Define model formula
f <- cbind(Environment1, Environment2, Environment3, Environment4,`Unique Pairs`) ~ 1

# LCA for 2 classes
lca_model_2 <- poLCA(f, df_env, nclass = 2, maxiter = 1000, na.rm = TRUE, graphs = TRUE)

# LCA for 3 classes
lca_model_3 <- poLCA(f, df_env, nclass = 3, maxiter = 1000, na.rm = TRUE, graphs = TRUE)

# Compare AIC and BIC for model selection
cat("AIC for 2 classes:", lca_model_2$aic, "\n")
cat("BIC for 2 classes:", lca_model_2$bic, "\n")
cat("AIC for 3 classes:", lca_model_3$aic, "\n")
cat("BIC for 3 classes:", lca_model_3$bic, "\n")

# Bar plot for 2 classes
barplot(lca_model_2$P, main = "Class Probabilities (2 classes)", xlab = "Class", ylab = "Probability", col = c("blue", "red"))

# Bar plot for 3 classes
barplot(lca_model_3$P, main = "Class Probabilities (3 classes)", xlab = "Class", ylab = "Probability", col = c("blue", "red", "green"))


# I am not sure if entropy is a statistic worth using for model selection, but could add more context?
calculate_entropy <- function(post_probs) {
  # Calculate 
  entropy <- -rowSums(post_probs * log(post_probs + 1e-10)) # Adding a small value to avoid log(0)
  # Average across all observations
  avg_entropy <- mean(entropy)
  return(avg_entropy)
}

#posterior probabilities for the 2-class model
post_probs_2 <- lca_model_2$posterior

# Calculate entropy for the 2-class model
entropy_2 <- calculate_entropy(post_probs_2)
cat("Entropy for 2-class model:", entropy_2, "\n")

# posterior probabilities for the 3-class model
post_probs_3 <- lca_model_3$posterior

# Calculate entropy for the 3-class model
entropy_3 <- calculate_entropy(post_probs_3)
cat("Entropy for 3-class model:", entropy_3, "\n")

# # Output results
# print(lca_model)
# print(lca_model$P)
# print(lca_model$probs)
# print(lca_model$predclass)

#nolint end