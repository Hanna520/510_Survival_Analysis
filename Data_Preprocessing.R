# -*- coding: utf-8 -*-
"""
Survival Analysis
Data Source: https://mimic.physionet.org/gettingstarted/access/

This analysis uses variables from multiple datasets. Some of them are re-engineered.


"""
# Install packages:
library(dplyr)
library(ggplot2)
library(stringr)

# Import Noteevents:
filename = gzfile('NOTEEVENTS.csv.gz','rt')
note = read.csv(filename)

# Create a dataframe with 2 columns (HADM_ID and number of reports)
# Each patiant has multiple reports (doctor's notes)
num_reports = note %>%
  group_by(HADM_ID) %>%
  summarise(NUM_REPORTS = n(),.groups = 'drop') %>%
  select(HADM_ID, NUM_REPORTS)

# Import Addimissions:
filename = gzfile('ADMISSIONS.csv.gz','rt')
admissions = read.csv(filename)

# Merge admissions and num_reports (left join):
adm = merge(admissions, num_reports, by='HADM_ID', all.x = T)

# Import ICU Stays:
filename = gzfile('ICUSTAYS.csv.gz','rt')
icu = read.csv(filename)

# Take the last ICU unit:
icu_last = icu %>%
  select(HADM_ID,LAST_CAREUNIT, OUTTIME) %>%
  group_by(HADM_ID) %>%
  arrange(OUTTIME, .by_group = T) %>%
  mutate(LAST_ICU = last(LAST_CAREUNIT)) %>%
  select(HADM_ID,LAST_ICU) %>%
  distinct()

# Merge ICU Unit data with admission data:
adm_icu = merge(adm, icu_last, by = 'HADM_ID', all.x = T)

# Import Patients:
filename = gzfile('PATIENTS.csv.gz','rt')
patients = read.csv(filename)

# Merge patients and adm_icu:
df3 = merge(adm_icu, patients, by='SUBJECT_ID', all.x = T)

# Create EVENT and Duration columns:
df3$DOD = as.Date(df3$DOD, format = '%Y-%m-%d')

df3 = df3 %>%
  # Identify DOD's after 90 days and create Duration column
  mutate(Duration = ifelse(is.na(DOD), 300, 
          (DOD - as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S')))) %>%
  # Create EVENT (death within 90 days as 1, otherwise 0)
  mutate(EVENT = ifelse(Duration <= 90, 1, 0)) %>%
  # Remove patients who died before discharge time or within one day of discharge
  # Remove newborns
  # Remove patients who died in hospital
  filter(Duration >= 1 & ADMISSION_TYPE!='NEWBORN' & HOSPITAL_EXPIRE_FLAG == 0)
  
# Create Hospital Length of Stay:
# Impute the number of reports column with 0 where Num_reports is missing
df3$NUM_REPORTS = ifelse(is.na(df3$NUM_REPORTS), 0, df3$NUM_REPORTS)

# Create a column for hospital length of stay
df3 = df3 %>%
  mutate(HOSPITAL_LOS = (as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S') - 
           as.Date(ADMITTIME, format = '%Y-%m-%d %H:%M:%S'))) %>%
  # Remove those who stayed less than 1 day in the hospital
  filter(HOSPITAL_LOS > 1) %>%
  # Create Age at Discharge column from DOB and Discharge time columns
  mutate(age = (as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S') -
           as.Date(DOB, format = '%Y-%m-%d %H:%M:%S'))/365)

# If age is greater than 100, code it to 90
df3$age = ifelse(df3$age > 100, 90, round(df3$age,1))

########################################################################
###################### SAVE THE DATASET FOR NOW ########################
########################################################################
save(df3, file = "Preprocssing.csv")

# Regroup categorical variables:
# Regroup Marital Status:
df3$MARITAL_STATUS = case_when(df3$MARITAL_STATUS == 'SINGLE' ~ 'SINGLE',
                               df3$MARITAL_STATUS == 'WIDOWED' ~ 'WIDOWED',
                               df3$MARITAL_STATUS %in% c('MARRIED', 'LIFE PARTNER') ~ 'MARRIED'
                               df3$MARITAL_STATUS %in% c('DIVORCED','SEPARATED') ~ 'DIVORCED',
                               TRUE                      ~  "OTHER")

# Regroup Ethnicity:
df3$ETHNICITY_Cat = case_when(str_detect(df3$ETHNICITY, 'WHITE') ~ 'WHITE',
                          str_detect(df3$ETHNICITY, 'BLACK') ~ 'BLACK',
                          str_detect(df3$ETHNICITY, 'HISPANIC') ~ 'HISPANIC',
                          str_detect(df3$ETHNICITY, 'ASIAN') ~ 'ASIAN',
                          df3$ETHNICITY %in% c('UNKNOWN/NOT SPECIFIED','UNABLE TO OBTAIN','PATIENT DECLINED TO ANSWER') ~ 'UNKNOWN',
                          TRUE ~ 'OTHER')

df3['ETHNICITY'] = ['WHITE' if 'WHITE' in x 
                    else 'BLACK' if 'BLACK' in x 
                    else 'HISPANIC' if 'HISPANIC' in x
                    else 'UNKNOWN' if x in ('UNKNOWN/NOT SPECIFIED','UNABLE TO OBTAIN','PATIENT DECLINED TO ANSWER')
                    else 'ASIAN' if 'ASIAN' in x 
                    else 'OTHER' for x in df3['ETHNICITY']]

# Regroup Discharge Location:
df3['DISCHARGE_LOCATION_2']=['SNF' if 'SNF' in x else 'HOSPICE' if 'HOSPICE' in x 
                             else 'HOME HEALTH CARE' if x in ('HOME HEALTH CARE','HOME WITH HOME IV PROVIDER')
                             else x if x in ('HOME','REHAB/DISTINCT PART HOSP','LONG TERM CARE HOSPITAL')
                             else 'OTHER' for x in df3['DISCHARGE_LOCATION']]

# Regroup Language:
df3['LANGUAGE_CATEGORY'] = ['ENGL' if x =='ENGL' else 'SPAN' if x=='SPAN' 
                            else 'MISSING' if pd.isnull(x)
                            else 'OTHER' for x in df3['LANGUAGE']]

# Remove those that were discharged to hospice related locations
df3 = df3[df3['DISCHARGE_LOCATION_2']!='HOSPICE']

# Remove those who are under the age of 18:
df_adults = df3[df3['AGE_AT_DISCH']>=18]

# Make numerical variables into bins:
# Bin Hospital Length of Stay:
df_adults['HOSPITAL_LOS_2'] = ['<=5' if x<=5 else '6-10' if x<=10 
                               else '10-20' if x <=50
                               else '20+' for x in df_adults['HOSPITAL_LOS']]

# Bin Number of Reports:
df_adults['NUM_REPORTS_2'] = ['<=10' if x<=10 else '11-20' if x<=20 
                              else '20-50' if x <=50
                              else '50+' for x in df_adults['NUM_REPORTS']]

# Bin Age according to MeSH:
df_adults['AGE_AT_DISCH_3'] = ['18-44' if x<=44 else '45-64' if x<=64 
                               else '65-79' if x <=79
                               else '80+' for x in df_adults['AGE_AT_DISCH']]

# Save the dataset and then Drop columns not needed for model:
# Save dataset: (default location: C:\Users\520ha, moved to Medical Notes)
df_adults.to_csv('df_adults_full.csv')

# Drop columns:
df_adults.drop(['ROW_ID_x', 'SUBJECT_ID', 'ADMITTIME', 'DISCHTIME',
                'DEATHTIME',  'ADMISSION_LOCATION',
                'DISCHARGE_LOCATION', 'LANGUAGE', 'RELIGION',
                'MARITAL_STATUS', 'ETHNICITY', 'EDREGTIME', 'EDOUTTIME', 'DIAGNOSIS',
                'HOSPITAL_EXPIRE_FLAG', 'HAS_CHARTEVENTS_DATA', 'NUM_REPORTS',
                'ROW_ID_y', 'DOB', 'DOD', 'DOD_HOSP', 'DOD_SSN',
                'EXPIRE_FLAG', 'followup',  'imputed', 'DOD_IMPUTED',
                'AGE_AT_DISCH', 'HOSPITAL_LOS'], axis = 1, inplace = True)

# Get ICD Category:
# Import ICD Diagnosese:
filename = 'C:\\Users\\520ha\\Desktop\\Chapman\\Research_2\\Medical Notes\\D_ICD_DIAGNOSES.csv.gz'
f = gzip.open(filename)
with gzip.open(filename) as f:
  diagnoses_code = pd.read_csv(f, low_memory = True)

# Recode icd codes by taking the first 3 characters except for Supplemental:
diagnoses_code['icd_coded'] = [1000 if (x.startswith('V')) or (x.startswith('E')) 
                               else int(x[:3]) for  x in diagnoses_code['ICD9_CODE']]

# Group new icd codes into major categories:
diagnoses_code['icd_category'] = ['INFECTIOUS AND PARASITIC DISEASES' if (x>0 and x <= 139) 
  else 'NEOPLASMS' if x <= 239 
  else 'ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS' if x<=279 
  else 'DISEASES OF BLOOD AND BLOOD-FORMING ORGANS' if x<=289
  else 'MENTAL DISORDERS' if x<=319
  else 'DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS' if x<=389
  else 'DISEASES OF THE CIRCULATORY SYSTEM' if x<= 459
  else 'DISEASES OF THE RESPIRATORY SYSTEM' if x<= 519
  else 'DISEASES OF THE DIGESTIVE SYSTEM' if x<= 579
  else 'DISEASES OF THE GENITOURINARY SYSTEM' if x<= 629
  else 'COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM' if x<= 679
  else 'DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE' if x<= 709
  else 'DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE' if x<= 739
  else 'CONGENITAL ANOMALIES' if x <= 759
  else 'OTHER OR MISSING' if x <= 799
  else 'INJURY AND POISONING' if x <= 999
  else 'OTHER OR MISSING' for x in diagnoses_code['icd_coded']]

# Get a list of unique icd_coded and its corresponding category:
icd_category = diagnoses_code.groupby(['icd_coded']).first()['icd_category']
code = icd_category.index
category = list(icd_category.values)

# Make the list into a data frame:
code_dict = {'icd_coded': [str(x) for x in code], 'category': category}
icd_cat = pd.DataFrame(code_dict)

# Import patient diagnoses data:
filename = 'C:\\Users\\520ha\\Desktop\\Chapman\\Research_2\\Medical Notes\\DIAGNOSES_ICD.csv.gz'
f = gzip.open(filename)
with gzip.open(filename) as f:
  diagnoses = pd.read_csv(f, low_memory = True)

# Recode the ICD code the same way as earlier:
diagnoses['icd_coded'] = ['missing' if pd.isnull(x) 
                          else '1000' if (x.startswith('V')) or (x.startswith('E')) 
                          else str(int(str(x)[:3])) for  x in diagnoses['ICD9_CODE']]

# Merge the patient diagnoses code with the code-category list:
diag_code = pd.merge(diagnoses, icd_cat, on = 'icd_coded', how = 'inner')

# Get the number of diagnoses under each major category for each HADM_ID:
# Group diag_code by the combination of HADM_ID and category and get the count:
count = diag_code.groupby(['HADM_ID','category'])['category'].count()
count = pd.DataFrame({'count' : count}).reset_index()

# Make icd categories as columns by pivoting count:
count_wide = count.pivot(columns='category',values='count')

# Get HADM_ID back to the data
diag_count = pd.concat([count, count_wide],axis=1)

# Replace NAs with 0
diag_count.fillna(0,inplace=True)

# Remove the duplicated HADM_IDs and comobine the counts for each category into one row:
diag = diag_count.groupby('HADM_ID').sum().reset_index()
diag.drop('count',axis=1, inplace=True)

# Merge with the main dataset:
adults = pd.merge(df_adults, diag, on = 'HADM_ID', how = 'inner')

# Save the dataset:
adults.to_csv('adults_with_ICU_Diagoses.csv')