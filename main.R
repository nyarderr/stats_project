library(haven)
library(tidyverse)


# Read in the data
data <- read_sav("data/DSCD603 Data.sav")


df <- data |> 
  mutate(across(where(is.labelled), as_factor))

write_csv(df, "data/DSCD603_Data.csv")

# ============================================
# Task 1: Exploratory Data Analysis
# ============================================

## Selected variables for EDA
eda_data <- data %>%
  select(PatientID,Age,Gender,PhysicalActivity,DietQuality,BMI,HealthScore,HighRisk) |> 
  mutate(across(where(is.labelled), as_factor))

# ## convert labelled variables to factors
# for (i in 1:ncol(eda_data)) {
#   if (is.labelled(eda_data[[i]])) {
#     eda_data[[i]] <- as_factor(eda_data[[i]])
#   }
# }

# ## Summary statistics
# View(as_tibble(summary(eda_data), .name_repair = "unique"))

## check for missing values
cat ("Missing values:",mean(is.na(eda_data) * 100), "%\n")

## check for duplicates
cat("Duplicate PatientIDs:", sum(duplicated(eda_data$PatientID)))

## descriptive statistics for numeric variables
numeric_vars <- eda_data %>%
  select(where(is.numeric)) |> 
  summarise_all(list(mean = mean, median = median, sd = sd, min = min, max = max), na.rm = TRUE)

## descriptive statistics for categorical variables
categorical_vars <- eda_data %>%
  select(where(is.factor)) |> 
  names()
  
# for (var in categorical_vars) {
#   cat("Frequency distribution for", var, ":\n")
#   eda_data |> 
#     count(.data[[var]]) |> 
#     mutate(percent = n / sum(n) * 100) |>
#     print()
# }
cat_summary <- map(categorical_vars, function(var) {
  eda_data %>%
    count(.data[[var]]) %>%
    mutate(Percentage = round(n / sum(n) * 100, 1)) %>%
    rename(Category = 1) |> 
    as_data_frame() -> summary_df
})





cat_summary |> 
  set_names(categorical_vars) |> 
  imap(function(df, var_name) { # imap lets us use the name for the caption
    table_html <- kable(df, caption = paste("Frequency distribution for", var_name)) |> 
      kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE) |> 
      column_spec(1, bold = TRUE) |> 
      # Dynamically set the header to span all columns minus the first one
      add_header_above(setNames(c(1, ncol(df) - 1), c(" ", "Frequency Distribution")))
    
    return(HTML(as.character(table_html)))
  }) |> 
  tagList()


## Visualizations

mtheme <- theme(panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_line(color = "black"),
                text = element_text(family = "Century Gothic", color = "black"),
                axis.text = element_text(size = 12, face = "bold"))

# Age distribution
ggplot(eda_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "turquoise", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))+
  mtheme -> p1
  


## Gender distribution
p2 <- eda_data %>%
  count(Gender) %>%
  mutate(pct   = scales::number(n / sum(n) * 100, accuracy = .1),
         label = paste0(n, " (", pct, "%)")) %>%
  ggplot(aes(x = Gender, y = n, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = label), vjust = -0.3, fontface = "bold", size = 5, family = "Century Gothic") +
  scale_fill_manual(values = c("#206095", "#F66068")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Gender Distribution", x = "Gender", y = "Count") +
  mtheme+ theme(legend.position = "none")


### Physical Activity distribution


eda_data %>%
  count(PhysicalActivity) %>%
  mutate(pct   = scales::number(n / sum(n) * 100, accuracy = .1),
         label = paste0(n, " (", pct, "%)")) %>%
  ggplot(aes(x = PhysicalActivity, y = n)) +
  geom_col(fill = "#27A0CC") +
  geom_text(aes(label = label), vjust = -0.3, fontface = "bold", size = 4.5,family = "Century Gothic") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Physical Activity Distribution", x = "Physical Activity Level", y = "Count") +
  mtheme -> p3



### Diet Quality distribution

p4 <- eda_data %>%
  count(DietQuality) %>%
  mutate(pct   = scales::number(n / sum(n) * 100, accuracy = .1),
         label = paste0(n, " (", pct, "%)")) %>%
  ggplot(aes(x = DietQuality, y = n)) +
  geom_col(fill = "#27A0CC") +
  geom_text(aes(label = label), vjust = -0.3, fontface = "bold", size = 4.5,family = "Century Gothic") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Diet Quality Distribution", x = "Diet Quality", y = "Count") +
  mtheme


### BMI distribution

ggplot(eda_data, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "#27A0CC", color = "black") +
  theme_minimal() +
  labs(title = "BMI Distribution", x = "BMI", y = "Frequency") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  mtheme -> p5


### Health Score distribution

ggplot(eda_data, aes(x = HealthScore)) +
  geom_histogram(binwidth = 5, fill = "#27A0CC", color = "black") +
  theme_minimal() +
  labs(title = "Health Score Distribution", x = "Health Score", y = "Frequency") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  mtheme -> p6


### High Risk distribution

p7 <- eda_data %>%
  count(HighRisk) %>%
  mutate(pct   = scales::number(n / sum(n) * 100, accuracy = .1),
         label = paste0(n, " (", pct, "%)")) %>%
  ggplot(aes(x = HighRisk, y = n)) +
  geom_col(fill = "#27A0CC") +
  geom_text(aes(label = label), vjust = -0.3, fontface = "bold", size = 4.5,family = "Century Gothic") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "High Risk Distribution", x = "High Risk Status", y = "Count") +
  mtheme



print(p1); print(p2); print(p3); print(p4)
print(p5); print(p6); print(p7)

plots <- list(p1, p2, p3, p4, p5, p6, p7)

for (i in seq_along(plots)) {
  ggsave(paste0("src/plots/eda_plot_", i, ".png"), plot = plots[[i]], width = 6, height = 4, dpi = 300)
}

# list.files("src/plots", pattern = "eda_plot_.*\\.png", full.names = TRUE)



# ====================================================================================================
# Task 2: Distributional Assessment and Assumptions
# ====================================================================================================

# Numeric variables 
numeric_vars <- eda_data %>%
  select(where(is.numeric)) |>
  select(-PatientID) |>
  names()

### Histograms with density curves for numeric variables


for (var in numeric_vars){
  df <- eda_data |> 
    select(all_of(var)) |> 
    filter(!is.na(.data[[var]]))
 
   # print(df)
  
  df |> 
    ggplot(aes(x = .data[[var]]))+
    geom_histogram(aes(y=after_stat(density)), binwidth = ifelse(var == "BMI",2,5), fill = "#27A0CC", color = "black") +
     geom_density(color = "red", linewidth = 1)+
    mtheme -> p 
  
  ggsave(paste0("src/plots/eda_hist_density_", var, ".png"), plot = p, width = 6, height = 4, dpi = 300)
}

### Q-Q plots for numeric variables

for (var in numeric_vars){
  df <- eda_data |> 
    select(all_of(var)) |> 
    filter(!is.na(.data[[var]]))
  
  # Create the plot object
  ppp <- ggplot(df, aes(sample = .data[[var]])) +
    geom_qq() +
    geom_qq_line(color = "red", linewidth = 0.5) +
    mtheme+
    labs(
      title = paste("Normal Q-Q Plot:", var),
      #subtitle = "Checking for normality (Points should follow the red line)",
      x = "Theoretical Quantiles",
      y = paste("Sample Quantiles of", var)
    ) 
  ggsave(
    filename = paste0("src/plots/qq_", var, ".png"), 
    plot = ppp, 
    width = 6, 
    height = 4, 
    dpi = 300
  )
}


levenes_test <- car::leveneTest(BMI ~ PhysicalActivity, data = eda_data)
cat("Levene's Test for Homogeneity of Variances (BMI by Physical Activity):\n")
print(levenes_test)


tuskey_result <- TukeyHSD(anova_result)
tuskey_summary <- as_tibble(tuskey_result$PhysicalActivity, rownames = "Comparison") |> 
  rename( Mean_Difference = diff, Lower_CI = lwr, Upper_CI = upr, P_Value = `p adj`) |>
  select(Comparison, Mean_Difference, Lower_CI, Upper_CI, P_Value ) |>
  mutate(Significant = ifelse(P_Value < 0.05, "Yes", "No"))


# ====================================================================================================
# LR
# ====================================================================================================

# ```{r model-evaluation, echo = FALSE, message = FALSE, warning = FALSE, results = "asis" }
# 1. Get the predicted probabilities from your model (from 0 to 1)
predicted_probs <- predict(logistic_model, type = "response")

# 2. Convert those probabilities into "Yes" or "No" (using a standard 50% threshold)
predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")

# 3. Create the Confusion Matrix table
conf_matrix <- table(Predicted = predicted_classes, Actual = eda_data$HighRisk)

# 4. Print the matrix
print(conf_matrix)

# 5. Calculate and print the overall accuracy percentage
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# ```

# 1. Install the packages if you haven't already (uncomment the line below to run once)
# install.packages(c("rpart", "rpart.plot"))

# ====================================================================================================
# DECISION TREE
# ====================================================================================================

# 2. Load the libraries
library(rpart)
library(rpart.plot)

# 3. Train the Decision Tree Model
# We use method = "class" because HighRisk is a categorical (Yes/No) classification
tree_model <- rpart(HighRisk ~ Age + BMI + PhysicalActivity + DietQuality, 
                    data = eda_data, 
                    method = "class")

# 4. Plot the beautiful Decision Tree flowchart
# The extra = 104 argument adds the percentages and probabilities to the boxes
png("src/plots/decision_tree.png", width = 800, height = 600, res = 100)

rpart.plot(tree_model, 
           type = 3, 
           extra = 104, 
           fallen.leaves = TRUE, 
           main = "Decision Tree Classification for High Risk Patients",
           box.palette = "RdBu", # Colors boxes red for risk, blue for safe
           shadow.col = "gray")

dev.off()

# 5. Evaluate Performance (Confusion Matrix)
# Get the model's exact "Yes" or "No" predictions
tree_predictions <- predict(tree_model, type = "class")

# Create and print the Confusion Matrix
tree_conf_matrix <- table(Predicted = tree_predictions, Actual = eda_data$HighRisk)
print("Decision Tree Confusion Matrix:")
print(tree_conf_matrix)

# Calculate and print overall Accuracy
tree_accuracy <- sum(diag(tree_conf_matrix)) / sum(tree_conf_matrix)
print(paste("Decision Tree Accuracy:", round(tree_accuracy * 100, 2), "%"))



# 1. Extract the True Positives, False Positives, and False Negatives from your matrix
# Make sure your matrix rows/cols are "Yes" and "No"
TP <- tree_conf_matrix["Yes", "Yes"]
FP <- tree_conf_matrix["Yes", "No"]
FN <- tree_conf_matrix["No", "Yes"]
TN <- tree_conf_matrix["No", "No"]

# 2. Calculate Precision and Recall
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))

# 3. Calculate and Plot the ROC-AUC
# Install pROC if you don't have it: install.packages("pROC")
library(pROC)

# For ROC, we need the raw PROBABILITIES, not the "Yes/No" classes
tree_probs <- predict(tree_model, type = "prob")[, "Yes"]

# Generate the ROC curve
roc_curve <- roc(eda_data$HighRisk, tree_probs)

# Plot it
plot(roc_curve, 
     main = "ROC Curve: Decision Tree Classifier", 
     col = "darkblue", 
     lwd = 3, 
     print.auc = TRUE) # This automatically prints the AUC score on the plot!



# 1. Open the file device
png("src/plots/roc_curve_plot.png", width = 800, height = 600, res = 300)

# 2. Run your plot code
plot(roc_curve, 
     main = "ROC Curve: Decision Tree Classifier", 
     col = "darkblue", 
     lwd = 3, 
     print.auc = TRUE)

# 3. Close the device to save the file
dev.off()











