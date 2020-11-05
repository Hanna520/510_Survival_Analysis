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

It will output a summary of the survival analysis (Cox Proportional Hazard survival analysis) output before and after removing insignificant variables.

These ouputs are in the Results folder.

## How to run the code

This code is NOT executable because the data sets are restricted.
Please read the .html files for the code and the ouputs.
The .html files are in the Markdown Files folder.

The 510_Midterm_Project.html file is the final version, and the 510_Project_First_Draft_Markdown.html is the first draft.


