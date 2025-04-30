library(tidyverse)
library(dplyr)
library(caret)

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
SNP_cols = colnames(combined_df_clean[6:29])
valid_SNP_cols_hair <- SNP_cols[sapply(train_data_hair[ , SNP_cols], function(x) length(unique(x)) > 1)]

SNP_str_hair = paste(valid_SNP_cols_hair, collapse = " + ")
model_formula_hair <- paste("Hair.Self.ID ~", SNP_str)
model_hair <- train(as.formula(model_formula_hair), 
                    data = train_data_hair,
                    method = "rf",
                    trControl = fit_control_hair)
pred_hair <- predict(model_hair, newdata = test_data_hair)

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








