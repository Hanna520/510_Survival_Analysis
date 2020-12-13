# 510_Survival_Analysis

## About the analysis

The aim of this analysis is to find significant factors that impact the mortality rate of critically ill patients within 90 days after they are discharged from the hospital. The findings of this study may help doctors to re-evaluate their discharge decisions to reduce the mortality rate.

This analysis uses variables from multiple data sets. Some of them are re-engineered. In addition to demographic and hospital stay information, comorbidity scores are also investigated in this study. Comorbidity scores are calculated from a R package "comorbidity", and the survival analysis is performed using the R package "survival".

## Data (Restricted by IRB)

This analysis requires 5 data sets. 

1. NOTEEVENTS.csv.gz
2. ADMISSIONS.csv.gz
3. ICUSTAYS.csv.gz
4. PATIENTS.csv.gz
5. DIAGNOSES_ICD.csv.gz

Because the data sets contain patient personal and health information, it is restricted by the IRB. Therefore, the data sets are not allowed to be posted on gitHub, and the thus you won't be able to run the code. However, from the .html file, you will see the structure of the datasets and some outputs of the code, which shows that the code works.

## Code Outputs

It will output a summary of the survival analysis (Cox Proportional Hazard survival analysis) output before and after removing insignificant variables, and a Hazard Ratio plot.

These outputs are in the Results folder.

## How to run the code

This code is NOT executable because the data sets are restricted.

Please read the .html file for the code and its outputs.
The .html file is in the Markdown Files folder
The 510_Final_Project.html file is the final version.

## Updates Since Last Submission:

  * 1. Focused only on patients who were sent home
  * 2. Added six vital sign variables to the analysis
  * 3. Added investigation of missing values and outliers in the vital sign variables


## Responses to Peer Review Comments:**

  * 1. The data is de-identified. The date of birth and date of death (if applicable) are masked. There is no other information that can easily identify the patient. Nevertheless, the data is restricted.
  * 2. The inconsistency in subsetting the data by columns has been fixed.
  * 3. About training and test sets, since this project is to identify factors that impact the mortality and to analyze the effect of the impact, it is more explanatory than predictive at this point, and thus no need to split the data into training and test sets. However, our future work will include a prediction part after investigating more variables such as topic modeling results from doctor’s notes, which will require splitting the data into training and test sets.





