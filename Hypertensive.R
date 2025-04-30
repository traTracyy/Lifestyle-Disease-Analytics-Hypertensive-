# Load necessary libraries
library(dplyr)
library(caret)
library(e1071)
library(ggplot2)
library(tidyverse)
library(GGally)
library(rlang)
library(gridExtra)
library(corrplot)
library(gridExtra)
library(pheatmap)
library(ggpubr)
library(viridis)
library(ggrepel) 
library(caret)
library(pROC)
library(PRROC)
library(vcd)
library(ggpmisc)

######################## import dataset and select features ########################
# Read data
df <- read.csv("MSFactors.csv")

str(df)

# Select features and target
features <- df %>%
  select(age, BMI, BloodGlucose, Dyslipidemia_HDL, Dyslipidemia, Hyperglycemia, 
         Obesity, MetabolicSyndrome)
target <- df$Hypertension

######################## Data Transformation ########################
# outlier detection
boxplot(features$age, main = "Age")

par(mfrow = c(1, 2)) 
boxplot(features$BMI, main = "BMI") # related to obesity
# Capping Outliers
iqr_bmi <- IQR(features$BMI)# Calculate IQR
lower_bound_bmi <- quantile(features$BMI, 0.25) - 1.5 * iqr_bmi # Calculate the upper and lower bounds
upper_bound_bmi <- quantile(features$BMI, 0.75) + 1.5 * iqr_bmi
features$BMI <- pmin(pmax(features$BMI, lower_bound_bmi), upper_bound_bmi)# Cap BMI values at the lower and upper bounds
boxplot(features$BMI, main = "BMI after Handling Outliers")


boxplot(features$BloodGlucose, main = "Blood Glucose")
iqr_bloodglucose <- IQR(features$BloodGlucose)
lower_bound_bloodglucose <- quantile(features$BloodGlucose, 0.25) - 1.5 * iqr_bloodglucose
upper_bound_bloodglucose <- quantile(features$BloodGlucose, 0.75) + 1.5 * iqr_bloodglucose
features$BloodGlucose <- pmin(pmax(features$BloodGlucose, lower_bound_bloodglucose), upper_bound_bloodglucose)
boxplot(features$BloodGlucose, main = "Blood Glucose after Handling Outliers")

par(mfrow = c(1, 1)) 

# Check for missing values
colSums(is.na(features))

# Check unique values for categorical variables
table(features$Dyslipidemia)
table(features$Hyperglycemia)
table(features$Obesity)
table(features$MetabolicSyndrome)

# Check for duplicate rows in the dataframe
duplicates <- duplicated(df)
sum(duplicates)


# Statistical summary for detecting noise through high variances
summary(features)

CSV <- features %>%
  mutate(
    age_category = cut(
      age,breaks = seq(20, 80, by = 5), include.lowest = TRUE,
      labels = paste(seq(20, 75, by = 5), seq(24, 79, by = 5), sep = "-")
    ),
    BMI_category = cut(
      BMI,breaks = seq(15, 45, by = 5),include.lowest = TRUE,
      labels = paste(seq(15, 40, by = 5), seq(19, 44, by = 5), sep = "-")
    ),
    BloodGlucose_category = cut(
      BloodGlucose,breaks = seq(65, 140, by = 5),include.lowest = TRUE,
      labels = paste(seq(65, 135, by = 5), seq(69, 139, by = 5), sep = "-")
    ),
    target = factor(target) 
  )


######################## Data Visualization ########################
# Age ↔ target
counts <- CSV %>%
  group_by(age_category, target) %>%
  summarise(count = n(), .groups = 'drop')
counts$target <- as.factor(counts$target)

ggplot(counts, aes(x = age_category, y = count, fill = target)) + 
  geom_bar(position = position_dodge(width = 0.9), stat = "identity") +
  geom_text(aes(label = count), vjust = 1.5, position = position_dodge(width = 0.9)) +
  facet_grid(rows = vars(target), scales = "free_y", 
             labeller = as_labeller(c("0" = "No Hypertension", "1" = "Hypertension"))) +
  scale_fill_manual(values = c("0" = "#00BFC4", "1" = "#F8766D")) +
  labs(
    title = "Hypertension Condition in Each Age Range",
    x = "", y = "Count of Individuals"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5),  
    legend.position = "none"  
  ) 

# BMI ↔ 5 features
counts1 <- CSV %>%
  group_by(BMI_category, target) %>%
  summarise(counts1 = n(), .groups = 'drop') 
counts1$target <- as.factor(counts1$target)
ggplot(counts1, aes(x = BMI_category, y = counts1, color = target)) + 
  geom_point(size = 10) + 
  geom_segment(aes(x = as.numeric(BMI_category), xend = as.numeric(BMI_category), y = 0, yend = counts1)) +
  geom_text(aes(label = counts1), color = "black") +
  scale_color_manual(values = c("0" = "#00BFC4", "1" = "#F8766D")) +  # Adjust the fill to color for points
  labs(
    title = "Hypertension Condition in Each BMI Category",
    x = "BMI Category", y = "Count of Individuals",
    color = "Hypertension Status"
  ) +
  facet_grid(rows = vars(target), labeller = as_labeller(c("0" = "No Hypertension", "1" = "Hypertension"))) +
  theme_update() +
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "none"  
  )


# Blood Glucose ↔ 5 features
counts2 <- CSV %>%
  group_by(BloodGlucose_category, target) %>%
  summarise(counts2 = n(), .groups = 'drop') 
counts2 <- counts2 %>%
  mutate(target = factor(target, levels = c(0, 1), labels = c("No Hypertension", "Hypertension")))
counts2$target <- as.factor(counts2$target)
ggplot(counts2, aes(BloodGlucose_category, counts2,group=target, color = target))+
  geom_line(size = 1) + geom_point()+ theme_bw()+theme(legend.position = "top")+
  labs(
    title = "Hypertension Condition in Each Blood Glucose Category",
    x = "Blood Glucose Category",
    y = "Count of Individuals",
    color = "Hypertension Condition" 
  ) +
  scale_color_manual(values = c("No Hypertension" = "#00BFC4", "Hypertension" = "#F8766D"))+
  stat_peaks(geom="text",
             aes(label = BloodGlucose_category),
             colour="black",vjust=-0.5,check_overlap=TRUE,span = NULL)


# Age ↔ BMI and Blood Glucose 
counts3 <- CSV %>%
  filter(target == 1) %>%
  group_by(age_category, BMI_category,BloodGlucose_category) %>%
  summarise(counts3 = n(), .groups = 'drop')
bmi_data <- counts3 %>%
  group_by(age_category, BMI_category) %>%
  summarise(total_BMI_counts = sum(counts3), .groups = 'drop')
glucose_data <- counts3 %>%
  group_by(age_category, BloodGlucose_category) %>%
  summarise(total_Glucose_counts = sum(counts3), .groups = 'drop')
bmi_data$type <- "BMI"
glucose_data$type <- "Blood Glucose"
bmi_data <- rename(bmi_data, category = BMI_category, counts = total_BMI_counts)# Renaming columns for a consistent format
glucose_data <- rename(glucose_data, category = BloodGlucose_category, counts = total_Glucose_counts)
combined_data <- rbind(bmi_data, glucose_data)
color_palette <- viridis::viridis(length(unique(combined_data$category)), option = "C")
max_counts <- combined_data %>%
  group_by(age_category, type) %>%
  summarise(counts = max(counts)) %>%
  left_join(combined_data, by = c("age_category", "type", "counts"))
ggplot(combined_data, aes(x = age_category, y = counts, fill = category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black")+
  geom_text(data = max_counts, size = 3,aes(label = category), vjust = -0.15, color = "black", position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = viridis::viridis(length(unique(combined_data$category)), option = "H")) +
  facet_grid(type ~ ., scales = "free_y") +
  labs(title = "Counts of BMI and Blood Glucose Categories by Age Range with Hypertensive Individual",
       x = "Age Category",
       y = "Counts of Individuals") +
  theme_bw() +
  theme(axis.text.x = element_text(hjust = 1))


# Correlation matrix plot, health-related variables
CSV %>%
  filter(target == 1)%>%
  select(age, BMI, BloodGlucose, Dyslipidemia_HDL, Dyslipidemia, Hyperglycemia, Obesity, MetabolicSyndrome) %>%
  cor(use = "complete.obs") %>%
  as.data.frame() %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE, lab_size = 3,
                         method = "circle", colors = c("red", "white", "blue"),
                         title = "Hypertension health indicator correlation matrix")

# target ↔ 5 features
data <- df %>%
  select(Dyslipidemia_HDL, Dyslipidemia, Hyperglycemia, Obesity, MetabolicSyndrome, Hypertension) %>%
  pivot_longer(cols = -Hypertension, names_to = "condition", values_to = "status") %>%
  mutate(status = factor(status, levels = c(0, 1), labels = c("No", "Yes")))
summary_data <- data %>%
  group_by(condition, status, Hypertension) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(condition, status) %>%
  mutate(total = sum(count),
         prop = count / total) 
summary_data_no <- summary_data %>%
  filter(status == "No")

plot_no <- ggplot(summary_data_no, aes(x = condition, y = count, fill = factor(Hypertension, labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = "stack",color="black") +
  geom_text(aes(label = paste(count, sprintf("\n(%.1f%%)", prop * 100))), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Health Indicators", y = "Count", title = "Good (Normal) Health Indicators vs. Hypertension Condition") +
  scale_fill_brewer(palette = "Pastel2", name = "Hypertension") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(), axis.ticks.y = element_line())
print(plot_no)


summary_data_yes <- summary_data %>%
  filter(status == "Yes")
plot_yes <- ggplot(summary_data_yes, aes(x = condition, y = count, fill = factor(Hypertension, labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = "stack",color="black") +
  geom_text(aes(label = paste(count, sprintf("\n(%.1f%%)", prop * 100))), position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Health Indicators", y = "Count", title = "Bad (Abnormal) Health Indicators Vs. Hypertension Condition") +
  scale_fill_brewer(palette = "Pastel2", name = "Hypertension") +
  theme_minimal() +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(), axis.ticks.y = element_line())
print(plot_yes)


# 5 features ↔ target
total_yes_counts <- summary_data %>%
  filter(status == "Yes") %>%
  group_by(condition) %>%
  summarise(totalcount = sum(count), .groups = 'drop')
hypertension_yes_data <- summary_data %>%
  filter(status == "Yes", Hypertension == 1) %>%
  left_join(total_yes_counts, by = "condition")
total_all_conditions <- sum(hypertension_yes_data$count)  
hypertension_yes_data <- hypertension_yes_data %>%
  mutate(correct_ratio = count / total_all_conditions * 100)
pie_chart <- ggplot(hypertension_yes_data, aes(x = "", y = correct_ratio, fill = condition)) +
  geom_bar(width = 1, stat = "identity") + 
  coord_polar(theta = "y") + 
  geom_text(aes(label = paste(condition, sprintf("\n(%.1f%%)", correct_ratio))),  
            position = position_stack(vjust = 0.4),  
            color = "black",  
            size = 3, 
            angle = 0) + 
  labs(title = "Proportion of Bad (Abnormal) Health Indicators and Have Hypertension") +
  theme_void() +  
  scale_fill_brewer(palette = "Pastel1", name = "condition") +
  guides(fill = FALSE) + 
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) 
print(pie_chart)


# 5 features - age range
df_long <- CSV %>%
  filter(target == 1) %>%
  pivot_longer(
    cols = c("Dyslipidemia_HDL", "Dyslipidemia", "Hyperglycemia", "Obesity", "MetabolicSyndrome"), 
    names_to = "Condition", values_to = "Count")
df_summary <- aggregate(Count ~ Condition + age_category, data = df_long, FUN = sum)
ggplot(df_summary, aes(x = age_category, y = Count, color = Condition,group = Condition)) +
  geom_point(size = 2)+geom_line()+
  facet_grid(rows = vars(Condition))+
  ggtitle("Count of Health Indicators in Each Age Category")+
  xlab("Age Range")+ ylab("Count")+
  theme_pubclean()+
  theme(legend.position = "none")


######################## Build Model ########################

# Split data into training and testing sets
set.seed(42) # for reproducibility
index <- createDataPartition(target, p = 0.8, list = FALSE)
X_train <- features[index, ]
X_test <- features[-index, ]
# Convert the target variables to factors ensuring they have the same levels
y_train <- target[index]
y_test <- target[-index]
levels <- sort(unique(df$Hypertension)) 
y_train <- factor(y_train, levels = levels)
y_test <- factor(y_test, levels = levels)

# Scaling the data
preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProcValues, X_train)
X_test_scaled <- predict(preProcValues, X_test)

# Training the model
model_scaled <- svm(X_train_scaled, y_train, type = 'C-classification', kernel = 'radial', probability = TRUE)
# Prediction using the model
y_pred_probs <- predict(model_scaled, X_test_scaled, probability = TRUE)

######################## Model Evaluation ########################

confusionMatrixPlot <- confusionMatrix(y_pred_probs, y_test)
fourfoldplot(confusionMatrixPlot$table,main = "Confusion Matrix")

# Evaluation metrics
conf_matrix <- confusionMatrix(y_pred_probs, y_test)
accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", accuracy))
# print("Confusion Matrix:")
# print(conf_matrix$table)
print("Classification Report:")
print(conf_matrix$byClass)

# Extract the probabilities for the positive class
probabilities <- attr(y_pred_probs, "probabilities")[,2]
# Compute the ROC curve
roc_obj <- roc(y_test, probabilities)
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

roc_curve_plot <- ggplot(data = data.frame(FPR = 1 - roc_obj$specificities, TPR = roc_obj$sensitivities), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue") +
  geom_abline(linetype = "dashed", color = "gray") +
  ggtitle(paste("ROC Curve (AUC =", round(roc_obj$auc, 4), ")")) +
  xlab("1 - Specificity") +
  ylab("Sensitivity") +
  theme_minimal()
print(roc_curve_plot)

