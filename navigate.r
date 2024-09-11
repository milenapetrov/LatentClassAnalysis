# nolint start
library(poLCA)
library(readxl)
library(openxlsx)

# x <- file.exists("/Users/milenapetrovic/UFL Dropbox/Milena Petrovic/dataCopy/processed_data/training/navigation_training_processed/main_data/nav_training_main_data_2024_05_30.csv") 
# print(x) 

#data <- read.csv("/Users/milenapetrovic/UFL Dropbox/Milena Petrovic/dataCopy/processed_data/training/navigation_training_processed/main_data/nav_training_main_data_2024_05_30.csv", header = TRUE, sep = ",") 
df <- read_excel("/Users/milenapetrovic/Downloads/nav-training-analysis-2/nav_metrics.xlsx")

df$Environment1 <- as.factor(grepl("A", df$Environment))
df$Environment2 <- as.factor(grepl("B", df$Environment))
df$Environment3 <- as.factor(grepl("A,B", df$Environment))
df$Environment4 <- as.factor(grepl("C", df$Environment))
df$`Path Accuracy` <- as.factor(df$`Path Accuracy`)

df_env <- df[, c("Environment1", "Environment2", "Environment3", "Environment4", "Path Accuracy")]
# Define the model formula
f <- cbind(Environment1, Environment2, Environment3, Environment4,`Path Accuracy`) ~ 1

# Run latent class analysis for 2 classes
lca_model <- poLCA(f, df_env, nclass = 2, maxiter = 1000, na.rm = TRUE, graphs = TRUE)

# # Output results
print(lca_model)
print(lca_model$P)
print(lca_model$probs)
print(lca_model$predclass)


#nolint end