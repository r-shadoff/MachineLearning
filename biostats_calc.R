library(tidyverse)
library(dplyr)

df <- read.csv("PhenotypePredictions.tsv", sep="\t")

# Clean up formatting
df_clean <- df %>%
  mutate(Eye.Self.ID = recode(Eye.Self.ID,
                              "Intermediate - Green" = "Intermediate",
                              "Intermediate - Hazel" = "Intermediate",
                              "brown" = "Brown"))
df_clean <- df_clean %>%
  mutate(Hair.Predicted = recode(Hair.Predicted,
                              "black" = "Black"))

lapply(df_clean, unique) # Check if additional values need cleaning

# Add SNP data 
snp_df <- read.csv("snp_data.tsv", sep="\t")
snp_df$X <- sapply(snp_df$X, as.numeric)
combined_df <- left_join(df_clean, snp_df, by = c("Sample" = "X"))

# Convert categories to levels 
hair_levels <- union(unique(df_clean$Hair.Predicted), unique(df_clean$Hair.Predicted))
df_clean$Hair.Predicted <- factor(df_clean$Hair.Predicted, levels = hair_levels)
df_clean$Hair.Self.ID <- factor(df_clean$Hair.Self.ID, levels = hair_levels)

eye_levels <- union(unique(df_clean$Eye.Predicted), unique(df_clean$Eye.Self.ID))
df_clean$Eye.Predicted <- factor(df_clean$Eye.Predicted, levels = eye_levels)
df_clean$Eye.Self.ID <- factor(df_clean$Eye.Self.ID, levels = eye_levels)

# Train test split
set.seed(412)
train_index_hair <- createDataPartition(df$Hair.Self.ID, p=0.6, list=FALSE)
train_data_hair <- combined_df[train_index_hair, ]
test_data_hair <- combined_df[-train_index_hair, ]
fit_control_hair <- trainControl(method = "cv", number = 10)
SNP_cols = colnames(combined_df[6:29])
SNP_str = paste(SNP_cols, collapse = " + ")
model_formula = paste("Hair.Self.ID ~", SNP_str)
model_hair <- train(as.formula(model_formula), 
                    data = train_data_hair,
                    method = "rf",
                    trControl = fit_control_hair)
pred_hair <- predict(model_hair, newdata = test_data_hair)










# Create matrix 
hair_matrix <- confusionMatrix(data = df_clean$Hair.Predicted, reference = df_clean$Hair.Self.ID)
eye_matrix <- confusionMatrix(data = df_clean$Eye.Predicted, reference = df_clean$Eye.Self.ID)

# Calculate evaluation metrics 
# Accuracy
# Sensitivity
# Specificity 
# PPV 