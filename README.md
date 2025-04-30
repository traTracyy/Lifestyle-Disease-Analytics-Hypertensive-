# Hypertension Prediction Model

## Overview
This project aims to predict hypertension based on key health indicators using machine learning techniques. The model leverages a dataset of 1,931 individuals with 27 health-related variables to identify risk factors and provide early detection capabilities for healthcare professionals.

## Key Features

### Objective
- **Primary Goal**: Predict hypertension likelihood using relevant health indicators
- **Secondary Goal**: Evaluate model performance for clinical applicability

### Scope
- Analyze health indicators associated with hypertension
- Develop and validate predictive models
- Provide actionable insights for healthcare professionals

## Data Understanding
The dataset contains 1931 observations with 27 variables. After careful evaluation, 9 key indicators were selected for modeling:

| Selected Features | Clinical Relevance |
|-------------------|--------------------|
| Age | Significant risk factor for hypertension |
| BMI | Measure of obesity (major hypertension risk) |
| BloodGlucose | Linked to insulin resistance and vascular damage |
| Dyslipidemia_HDL | Low HDL increases hypertension risk |
| Dyslipidemia | Affects arterial health and blood pressure |
| Hyperglycemia | Chronic high glucose damages blood vessels |
| Obesity | Directly increases cardiac output and BP |
| MetabolicSyndrome | Significantly raises hypertension risk |

## Data Preparation
- **Outlier Handling**: Capped extreme BMI (up to 70 → 44.25) and Blood Glucose (up to 350 → 137 mg/dL)
- **Missing Values**: No missing values detected
- **Data Quality**: No duplicate rows found
- **Feature Engineering**:
  - Categorized continuous variables (Age, BMI, BloodGlucose)
  - Converted target variable to factor

## Exploratory Analysis
Key findings from data visualization:

### Age Analysis
- Hypertension prevalence peaks in 75-79 age group
- Critical risk window starts at 50-54 years

### BMI Analysis
- Hypertensive individuals predominantly in obese categories (BMI 25-34)
- Normal weight individuals show lower hypertension rates

### Blood Glucose
- 95-119 mg/dL range shows highest hypertension correlation
- Levels above 130 mg/dL strongly associated with hypertension

### Correlation Insights
- Strong correlation (0.73) between Blood Glucose and Hyperglycemia
- Obesity and Metabolic Syndrome show 0.57 correlation

## Modeling Approach
**Algorithm**: Support Vector Machine (SVM) with Radial Basis Function kernel

### Model Performance
| Metric | Value |
|--------|-------|
| Accuracy | 74.35% |
| Sensitivity (Recall) | 91.03% |
| Specificity | 54.02% |
| F1 Score | 79.58% |
| AUC-ROC | 0.8371 |

### Confusion Matrix
| | Predicted Negative | Predicted Positive |
|----------------|---------------------|---------------------|
| Actual Negative | 193 (TN) | 80 (FP) |
| Actual Positive | 19 (FN) | 94 (TP) |

## Business Impact
1. **Early Intervention**: Enables proactive management for at-risk patients
2. **Personalized Care**: Facilitates tailored treatment plans
3. **Cost Reduction**: Potential to decrease hypertension-related complications
4. **Public Health**: Supports population health management strategies

## Recommendations
1. Focus screening efforts on individuals:
   - Aged 50+ years
   - With BMI ≥ 25
   - Blood glucose levels 95-119 mg/dL
2. Implement weight management programs
3. Monitor metabolic syndrome indicators closely
4. Consider model integration in clinical decision support systems

## Future Enhancements
1. Incorporate additional risk factors (e.g., physical activity, diet)
2. Test alternative algorithms (Random Forest, XGBoost)
3. Develop real-time prediction capabilities
4. Expand dataset for improved generalizability

---

## 👩‍💻 Author

Khor You Qi  
[LinkedIn: khor-you-qi-tracy](https://www.linkedin.com/in/khor-you-qi-tracy/)
**Technology Stack**: R, ggplot2, SVM  
**Dataset**: MSFactors.csv (1931 observations)