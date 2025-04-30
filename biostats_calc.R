library(tidyverse)
library(dplyr)
library(caret)
library(pROC)
library(reshape2)

df <- read.csv("PhenotypePredictions.tsv", sep="\t")

# Clean up formatting
df_clean <- df_clean %>%
  mutate(Eye.Self.ID = recode(Eye.Self.ID,
                              "Intermediate - Green" = "Intermediate",
                              "Intermediate - Hazel" = "Intermediate",
                              "brown" = "Brown"))
df_clean <- df_clean %>%
  mutate(Hair.Predicted = recode(Hair.Predicted,
                              "black" = "Black"))

df_clean <- df_clean %>%
  mutate(Eye.Predicted = recode(Eye.Predicted,
                                 "brown" = "Brown"))

lapply(df_clean, unique) # Check if additional values need cleaning

# Add SNP data 
snp_df <- read.csv("snp_data.tsv", sep="\t")
snp_df$X <- sapply(snp_df$X, as.numeric)
combined_df <- left_join(df_clean, snp_df, by = c("Sample" = "X"))
combined_df_clean <- na.omit(combined_df)

SNP_cols = colnames(combined_df_clean[6:29])
for (col in SNP_cols) {
  combined_df_clean[[col]] <- factor(combined_df_clean[[col]], levels = unique(combined_df_clean[[col]]))
}

# Hair Colour Model
hair_levels <- union(unique(combined_df_clean$Hair.Predicted), unique(combined_df_clean$Hair.Self.ID))
combined_df_clean$Hair.Predicted <- factor(combined_df_clean$Hair.Predicted, levels = hair_levels)
combined_df_clean$Hair.Self.ID <- factor(combined_df_clean$Hair.Self.ID, levels = hair_levels)


# Train test split
set.seed(412)
train_index_hair <- createDataPartition(combined_df_clean$Hair.Self.ID, p=0.8, list=FALSE)
train_data_hair <- combined_df_clean[train_index_hair, ]
test_data_hair <- combined_df_clean[-train_index_hair, ]
fit_control_hair <- trainControl(method = "cv", number = 5, sampling = "up") # Upsampled due to low counts in minority class (blonde) 

# Remove SNPs with only one level



valid_SNP_cols_hair <- SNP_cols[sapply(train_data_hair[ , SNP_cols], function(x) length(unique(x)) > 1)]

SNP_str_hair = paste(valid_SNP_cols_hair, collapse = " + ")
model_formula_hair <- paste("Hair.Self.ID ~", SNP_str)
model_hair <- train(as.formula(model_formula_hair), 
                    data = train_data_hair,
                    method = "rf",
                    trControl = fit_control_hair)
pred_hair <- predict(model_hair, newdata = test_data_hair)
prob_hair <- predict(model_hair, newdata = test_data_hair, type = "prob")
# Eye Colour Model
eye_levels <- union(unique(combined_df_clean$Eye.Predicted), unique(combined_df_clean$Eye.Self.ID))
combined_df_clean$Eye.Predicted <- factor(combined_df_clean$Eye.Predicted, levels = eye_levels)
combined_df_clean$Eye.Self.ID <- factor(combined_df_clean$Eye.Self.ID, levels = eye_levels)

# Train test split
set.seed(412)
train_index_eye <- createDataPartition(combined_df_clean$Eye.Self.ID, p=0.8, list=FALSE)

train_data_eye <- combined_df_clean[train_index_eye, ]

test_data_eye <- combined_df_clean[-train_index_eye, ]

fit_control_eye <- trainControl(method = "cv", number = 5, sampling = "up") # Upsampled due to low counts in minority class (intermediate) 

# Remove SNPs with only one level
valid_SNP_cols_eye <- SNP_cols[sapply(train_data_eye[ , SNP_cols], function(x) length(unique(na.omit(x))) > 1)]


SNP_str_eye = paste(valid_SNP_cols_eye, collapse = " + ")
model_formula_eye <- paste("Eye.Self.ID ~", SNP_str_eye)
model_eye <- train(as.formula(model_formula_eye), 
                    data = train_data_eye,
                    method = "rf",
                    trControl = fit_control_eye)
pred_eye <- predict(model_eye, newdata = test_data_eye)
prob_eye <- predict(model_eye, newdata = test_data_eye, type = "prob")



# Evaluation
conf_matrix_eye <- confusionMatrix(pred_eye, test_data_eye$Eye.Self.ID)
print(conf_matrix_eye)
# Actual labels
true_labels <- test_data_eye$Eye.Self.ID

# Convert to one-hot encoding
actual_mat <- model.matrix(~ true_labels - 1)
colnames(actual_mat) <- levels(true_labels)

# Create an empty list to store ROC curves
roc_list <- list()
auc_list <- c()

# Loop through each class
for (class in levels(true_labels)) {
  roc_obj <- roc(actual_mat[, class], prob_eye[, class])
  auc_val <- auc(roc_obj)
  auc_list[class] <- auc_val
  
  plot(roc_obj, main = paste("ROC Curve -", class), col = "blue", lwd = 2)
  legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "blue", lwd = 2)
  
  roc_list[[class]] <- roc_obj
}
print(auc_list)


# Hair


conf_matrix_hair <- confusionMatrix(pred_hair, test_data_hair$Hair.Self.ID)
print(conf_matrix_hair)

# Actual labels
true_labels <- test_data_hair$Hair.Self.ID

# Convert to one-hot encoding
actual_mat <- model.matrix(~ true_labels - 1)
colnames(actual_mat) <- levels(true_labels)

# Create an empty list to store ROC curves
roc_list <- list()
auc_list <- c()

# Loop through each class
for (class in levels(true_labels)) {
  roc_obj <- roc(actual_mat[, class], prob_hair[, class])
  auc_val <- auc(roc_obj)
  auc_list[class] <- auc_val
  
  plot(roc_obj, main = paste("ROC Curve -", class), col = "blue", lwd = 2)
  legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), col = "blue", lwd = 2)
  
  roc_list[[class]] <- roc_obj
}
print(auc_list)





