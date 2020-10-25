
#Survival Analysis
#Data Source: https://mimic.physionet.org/gettingstarted/access/

#This analysis uses variables from multiple datasets. Some of them are re-engineered.



# Install packages:
library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(comorbidity)
library(survival)

########################################################################
########################## IMPORT NOTEEVENTS ###########################
########################################################################
# Import Noteevents:
filename = gzfile('NOTEEVENTS.csv.gz','rt')
note = read.csv(filename)

# Create a dataframe with 2 columns (HADM_ID and number of reports)
# Each patiant has multiple reports (doctor's notes)
num_reports = note %>%
  group_by(HADM_ID) %>%
  summarise(NUM_REPORTS = n(),.groups = 'drop') %>%
  select(HADM_ID, NUM_REPORTS)

########################################################################
########################## IMPORT ADMISSIONS ###########################
########################################################################
# Import Admissions:
filename = gzfile('ADMISSIONS.csv.gz','rt')
admissions = read.csv(filename)

# Merge admissions and num_reports (left join):
adm = merge(admissions, num_reports, by='HADM_ID', all.x = T)

########################################################################
########################## IMPORT ICUSTAYS #############################
########################################################################
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

########################################################################
########################## IMPORT PATIENTS #############################
########################################################################
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

# Regroup categorical variables:
# Regroup Marital Status:
df3$MARITAL_STATUS_CAT = case_when(df3$MARITAL_STATUS == 'SINGLE' ~ 'SINGLE',
                               df3$MARITAL_STATUS == 'WIDOWED' ~ 'WIDOWED',
                               df3$MARITAL_STATUS %in% c('MARRIED', 'LIFE PARTNER') ~ 'MARRIED',
                               df3$MARITAL_STATUS %in% c('DIVORCED','SEPARATED') ~ 'DIVORCED',
                               TRUE                      ~  'UNKNOWN')
df3$MARITAL_STATUS_CAT = as.factor(df3$MARITAL_STATUS_CAT)

# Regroup Ethnicity:
df3$ETHNICITY_CAT = case_when(str_detect(df3$ETHNICITY, 'WHITE') ~ 'WHITE',
                          str_detect(df3$ETHNICITY, 'BLACK') ~ 'BLACK',
                          str_detect(df3$ETHNICITY, 'HISPANIC') ~ 'HISPANIC',
                          str_detect(df3$ETHNICITY, 'ASIAN') ~ 'ASIAN',
                          df3$ETHNICITY %in% c('UNKNOWN/NOT SPECIFIED','UNABLE TO OBTAIN','PATIENT DECLINED TO ANSWER') ~ 'UNKNOWN',
                          TRUE ~ 'OTHER')
df3$ETHNICITY_CAT = as.factor(df3$ETHNICITY_CAT)

# Regroup Discharge Location:
df3$DISCH_LOC_CAT = case_when(str_detect(df3$DISCHARGE_LOCATION, 'HOSPICE') ~ 'HOSPICE',
                              str_detect(df3$DISCHARGE_LOCATION, 'SNF') ~ 'SNF',
                              df3$DISCHARGE_LOCATION %in% c('HOME HEALTH CARE','HOME WITH HOME IV PROVIDER') ~ 'HOME HEALTH CARE',
                              df3$DISCHARGE_LOCATION %in% c('HOME','REHAB/DISTINCT PART HOSP','LONG TERM CARE HOSPITAL') ~ df3$DISCHARGE_LOCATION,
                              TRUE ~ 'OTHER')
df3$DISCH_LOC_CAT = as.factor(df3$DISCH_LOC_CAT)



# Remove those that were discharged to hospice related locations
df3 = filter(df3, DISCH_LOC_CAT != 'HOSPICE')

# Remove those who are under the age of 18:
df_adults = filter(df3, age >= 18)

# Regroup insurance:
df_adults$INSURANCE_CAT = ifelse(df_adults$INSURANCE %in% c("Government","Medicaid","Medicare","Private"), 
                                 df_adults$INSURANCE, 'Private')
df_adults$INSURANCE_CAT = as.factor(df_adults$INSURANCE)

# Make numerical variables into bins:

df4=within(df_adults,{
  # Bin Hospital Length of Stay:
  HOSPITAL_LOS_CAT = cut(as.numeric(HOSPITAL_LOS), breaks=c(-Inf,5,10,20,Inf),labels = c("<=5","6-10","11-20",">20"))
  # Bin Number of Reports:
  NUM_REPORTS_CAT = cut(NUM_REPORTS, breaks=c(-Inf,10,20,50, Inf),labels = c("<=10","11-20", "21-50", ">50"))
  # Bin Age according to MeSH:
  AGE_CAT = cut(round(age), breaks=c(-Inf,44,64,79, Inf),labels = c("<=44","45-64", "65-79", ">=80"))
  })

########################################################################
######################### SAVE THE DATASET #############################
########################################################################

# Save the dataset and then Drop columns not needed for model:
# Save dataset: (location: C:\Users\520ha\Desktop\Chapman\510_Survival_Analysis)
write.csv(df4, 'Adults_full_R.csv')

# Select columns that will be used in the model:
df4 = df4 %>%
  select(HADM_ID,ADMISSION_TYPE,INSURANCE,INSURANCE_CAT,DIAGNOSIS,LAST_ICU,
         GENDER, MARITAL_STATUS_CAT, ETHNICITY_CAT, DISCH_LOC_CAT, AGE_CAT,
         NUM_REPORTS_CAT,HOSPITAL_LOS_CAT, Duration,EVENT)



########################################################################
######################## IMPORT DIAGNOSES_ICD ##########################
########################################################################

# Import patient diagnoses data:
filename = gzfile('DIAGNOSES_ICD.csv.gz','rt')
diagnoses = read.csv(filename)

diagnoses = diagnoses[,c("HADM_ID","ICD9_CODE")]

df_com = merge(df4, diagnoses, by = 'HADM_ID', all.x = T)

########################################################################
########################## COMORBIDITY SCORES ##########################
########################################################################

# Charlson score based on ICD-10 diagnostic codes:
df_comorbidity = comorbidity(x = df_com, id = "HADM_ID", code = "ICD9_CODE", 
                             score = "charlson", assign0 = FALSE, icd = 'icd9')

# Remove the last four items from the comorbidity output
df_comorbidity1 = df_comorbidity[,!names(df_comorbidity) %in% 
                                   c("score", "index","wscore","windex")]

# Or keep only wscore from the comorbidity output
df_comorbidity2 = subset(df_comorbidity, select = c(HADM_ID,wscore))

# Remove the icd code column from original data frame:
com_no_icd = df_com[,names(df_com) != 'ICD9_CODE']

# Remove duplicates from original data frame:
unique_com = unique(com_no_icd)

# Merge the main dataset and the comorbidity:
com_all = merge(unique_com, df_comorbidity1, by="HADM_ID")
# com_all2= merge(unique_com, df_comorbidity2, by="HADM_ID")

# Remove "INSURANCE":
com_all <- com_all[, !names(com_all) %in% c('INSURANCE','INSURANCE_CAT')]

# Take only those who were sent home:
home = com_all[com_all$DISCH_LOC_CAT=='HOME',]
home = home[,names(home) != 'DISCH_LOC_CAT']

# Import ventilation information and join with the home dataset
# vent = read.csv('ventilation.csv')
# vent = select(vent, -"X")
# home_vent = left_join(home,vent,by='HADM_ID')

########################################################################
########################### SURVIVAL ANALYSIS ##########################
########################################################################

# Remove insignificant variables:
home=select(home,-c(diab,dementia,cevd,pud))
home=select(home,-c(rheumd,diabwc))
home=select(home,-ami)
home=select(home,-LANGUAGE_CATEGORY)
home=select(home,-GENDER)

# home_vent=select(home_vent,-c(GENDER,diab,dementia,cevd,pud,rheumd,diabwc,ami))


# For Survival Analysis without ventilation:
time = home$Duration
event = home$EVENT
x <- subset(home, select = -c(HADM_ID, Duration, EVENT))
dim(x)

# kp = survfit(Surv(time, event) ~1, data=dataname)

# Survival Analysis with ventilation
# time = home_vent$Duration
# event = home_vent$EVENT
# # x <- subset(home, select = -c(HADM_ID, Duration, EVENT, wscore))
# x <- subset(home_vent, select = -c(HADM_ID, Duration, EVENT))
# dim(x)

coxph = coxph(Surv(time,event) ~ ., data = x)
summary(coxph)

# reg=survreg(Surv(time,event) ~ ., data=x, dist="lognormal")
# summary(reg)

# Plot
install.packages('survminer')
library(survminer)
plt = ggforest(coxph, data = home)

# Save plot:
tiff(file="saving_plot3.tiff",
     width=8, height=10, units="in", res=300)
ggforest(coxph, data = home, main="Hazard Ratio for Patients Discharged Home")
dev.off()

# Save model output:
sink(file = 'cox_cmbdty_Home.txt')
summary(coxph)
sink(file=NULL)


####################################################
kp = survfit(Surv(time, event) ~as.factor(AGE_AT_DISCH_3), data=home)
# plot(kp,xlab="Time (days)",xlim=c(0,90),ylab="Survival probability",
#      col=c(1,2,3,4),conf.int=F, main="Kaplan Meier Graph")

# Need to find a way to extract the number of observations from diff
diff = survdiff(Surv(time, event)~AGE_AT_DISCH_3+ADMISSION_TYPE, data= home)


survdiff(Surv(time, event)~AGE_AT_DISCH_3+ADMISSION_TYPE, data= home)
require(stringr)
require(tidyr)
nn=str_extract(diff$n, "{1,4}")


########################################################################
# The following are for Num_Diagnoses. Not finished. ###################
########################################################################

########################################################################
######################## IMPORT D_ICD_DIAGNOSES ########################
########################################################################
# Import ICD Diagnoses:
filename = gzfile('D_ICD_DIAGNOSES.csv.gz','rt')
diagnoses_code = read.csv(filename)

# Get ICD Category:
# Recode icd codes by taking the first 3 characters except for Supplemental:
diagnoses_code = diagnoses_code %>%
  mutate(icd_coded = ifelse(substr(ICD9_CODE,1,1) %in% c('V','E'), 1000,
                            substr(ICD9_CODE,1,3))) %>%
  # Group new icd codes into major categories:
  mutate(icd_category = cut(as.numeric(icd_coded), 
                            breaks = c(0,139,239,279,289,319,389,459,519,579,
                                       629,679,709,739,759,779,799,999,Inf),
                            labels = c('INFECTIOUS AND PARASITIC DISEASES',
                                       'NEOPLASMS',
                                       'ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES, AND IMMUNITY DISORDERS',
                                       'DISEASES OF BLOOD AND BLOOD-FORMING ORGANS',
                                       'MENTAL DISORDERS',
                                       'DISEASES OF THE NERVOUS SYSTEM AND SENSE ORGANS',
                                       'DISEASES OF THE CIRCULATORY SYSTEM',
                                       'DISEASES OF THE RESPIRATORY SYSTEM',
                                       'DISEASES OF THE DIGESTIVE SYSTEM',
                                       'DISEASES OF THE GENITOURINARY SYSTEM',
                                       'COMPLICATIONS OF PREGNANCY, CHILDBIRTH, AND THE PUERPERIUM',
                                       'DISEASES OF THE SKIN AND SUBCUTANEOUS TISSUE',
                                       'DISEASES OF THE MUSCULOSKELETAL SYSTEM AND CONNECTIVE TISSUE',
                                       'CONGENITAL ANOMALIES',
                                       'CERTAIN CONDITIONS ORIGINATING IN THE PERINATAL PERIOD',
                                       'SYMPTOMS, SIGNS, AND ILL-DEFINED CONDITIONS',
                                       'INJURY AND POISONING',
                                       'SUPPLEMENTAL OR MISSING')))

# Get a list of unique icd_coded and its corresponding category:
category = diagnoses_code %>%
  select(icd_coded, icd_category) %>%
  group_by(icd_coded) %>%
  distinct()


# Recode the ICD code the same way as earlier:
# diagnoses = diagnoses %>%
#   mutate(icd_coded = ifelse(substr(ICD9_CODE,1,1) %in% c('V','E'), 1000,
#                             substr(ICD9_CODE,1,3)))
# 
# # Merge the patient diagnoses code with the code-category list:
# diag_code = merge(diagnoses, category, by = 'icd_coded')
# 
# # Get the number of diagnoses under each major category for each HADM_ID:
# # Group diag_code by the combination of HADM_ID and category and get the count:
# count = diag_code %>%
#   group_by(HADM_ID,icd_category) %>%
#   mutate(NUM_DIAG = n()) 
# 
# # Make icd categories as columns by pivoting count:
# count_wide = count %>%
#   select(HADM_ID,icd_category,NUM_DIAG) %>%
#   dcast(HADM_ID ~ icd_category, value.var = 'NUM_DIAG')




# Merge with the main dataset:
adults = pd.merge(df_adults, diag, on = 'HADM_ID', how = 'inner')

# Save the dataset:
adults.to_csv('adults_with_ICU_Diagoses.csv')