# FinalDegreeProject
Here you will find the scripts I have used to analyze the dataset used during my Final Degree Project.

1. HDOM_09_18.R
Contains the data imputation process of the initial dataset.csv I started working with. From merging the different sources of data into one single data frame, to transforming data and finally imputing missing values.

2. pval_computation.R
Script used to perform the statistical analyses to extract the p-values of the different categories presented in the introductory Table 1 of the report. Comparing all subjects to only readmitted and to deceased patients.

3. model_randomforest_ * .R
  - random forest algorithm to produce RM1-RM4

4. model_decisiontree_ * .R
  - decision tree algorithm to produce RM1-RM4

5. model_regression_* .R
  - logistic regression algorithm to produce RM1-RM4

6. Note: In the model_randomforest_ * .R scripts I wrote the code regarding computation of all plots:
  1) ROC curves for comparing the three models (Fig. 3)
  2) Most important variables of each model (Fig. 4 and Fig. 5)
  3) ROC curves for comparing the differences of data sources within each model (Fig. 6)
