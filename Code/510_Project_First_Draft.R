
#Survival Analysis
#Data Source: https://mimic.physionet.org/gettingstarted/access/

#This analysis uses variables from multiple datasets. Some of them are re-engineered.



# Install packages:
library(dplyr)
library(stringr)
library(reshape2)
library(comorbidity)
library(survival)
library(ggplot2)

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

write.csv(num_reports,'num_reports.csv')

########################################################################
########################## IMPORT ADMISSIONS ###########################
########################################################################
# Import Admissions:
filename = gzfile('ADMISSIONS.csv.gz','rt')
admissions = read.csv(filename)

# Merge admissions and num_reports (left join):
adm = merge(admissions, num_reports, by='HADM_ID')

########################################################################
########################## IMPORT ICUSTAYS #############################
########################################################################
# Import ICU Stays:
filename = gzfile('ICUSTAYS.csv.gz','rt')
icu = read.csv(filename)

# Take the last ICU unit and total Length of Stay:
icu_last = icu %>%
  select(HADM_ID, LAST_CAREUNIT, OUTTIME, LOS) %>%
  group_by(HADM_ID) %>%
  mutate(LAST_ICU = last(LAST_CAREUNIT), TOT_LOS = sum(LOS)) %>%
  select(HADM_ID, LAST_ICU, TOT_LOS) %>%
  distinct()

# Merge ICU Unit data with admission data:
adm_icu = merge(adm, icu_last, by = 'HADM_ID')


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
  # Identify DODs after 90 days and create Duration column
  mutate(Duration = ifelse(is.na(DOD), 300, 
                           (DOD - as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S')))) %>%
  # Create EVENT (death within 90 days of discharge as 1, otherwise 0)
  mutate(EVENT = ifelse(Duration <= 90, 1, 0)) %>%
  # Remove patients who died before discharge or within one day of discharge
  # Remove newborns
  # Remove patients who died in hospital
  filter(Duration >= 1 & ADMISSION_TYPE!='NEWBORN' & HOSPITAL_EXPIRE_FLAG == 0)

# Create Hospital Length of Stay:
# Impute the number of reports column with 0 where Num_reports is missing
df3$NUM_REPORTS = ifelse(is.na(df3$NUM_REPORTS), 0, df3$NUM_REPORTS)


df3 = df3 %>%
  # Create a column for hospital length of stay
  mutate(HOSPITAL_LOS = (as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S') - 
                           as.Date(ADMITTIME, format = '%Y-%m-%d %H:%M:%S'))) %>%
  # Remove those who stayed less than 1 day in the hospital
  filter(HOSPITAL_LOS > 1) %>%
  # Create Age at Discharge column from DOB and Discharge time columns
  mutate(age = (as.Date(DISCHTIME, format = '%Y-%m-%d %H:%M:%S') -
                  as.Date(DOB, format = '%Y-%m-%d %H:%M:%S'))/365)

# If age is greater than 100, code it to 90
# In the MIMIC database, patients older than 89 were coded as 300 years old
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

# Regroup insurance:
df3$INSURANCE_CAT = ifelse(df3$INSURANCE %in% c("Government","Medicaid","Medicare"), 
                                 'Government',df3$INSURANCE)
df3$INSURANCE_CAT = as.factor(df3$INSURANCE_CAT)


# Remove those that were discharged to hospice related locations
df3 = filter(df3, DISCH_LOC_CAT != 'HOSPICE')

# Remove those who are under the age of 18:
df_adults = filter(df3, age >= 18)


# Plot of numerical variables
ggplot(df_adults, aes(TOT_LOS)) + geom_histogram() + xlim(c(0,10))


# Make numerical variables into bins:
df4=within(df_adults,{
  # Bin Hospital Length of Stay:
  HOSPITAL_LOS_CAT = cut(as.numeric(HOSPITAL_LOS), breaks=c(-Inf,5,10,20,Inf),
                         labels = c("<=5","6-10","11-20",">20"))
  # Bin ICU Length of Stay:
  TOT_LOS_CAT = cut(round(TOT_LOS), breaks = c(-Inf, 2, 5, 10,Inf),
                    labels = c("<=2","3-5","6-10",">10"))
  # Bin Number of Reports:
  NUM_REPORTS_CAT = cut(NUM_REPORTS, breaks=c(-Inf,10,20,50, Inf),
                        labels = c("<=10","11-20", "21-50", ">50"))
  # Bin Age according to MeSH:
  AGE_CAT = cut(round(age), breaks=c(-Inf,44,64,79, Inf),
                labels = c("<=44","45-64", "65-79", ">=80"))
})

########################################################################
######################### SAVE THE DATASET #############################
########################################################################

# Save the dataset and then Drop columns not needed for model:
# Save dataset: (location: C:\Users\520ha\Desktop\Chapman\510_Survival_Analysis)
write.csv(df4, 'Adults_full_R.csv')

df4 = read.csv('Adults_full_R.csv')

# Select columns that will be used in the model:
df4 = df4 %>%
  select(HADM_ID,ADMISSION_TYPE,INSURANCE,INSURANCE_CAT,DIAGNOSIS,LAST_ICU,
         GENDER, MARITAL_STATUS_CAT, ETHNICITY_CAT, DISCH_LOC_CAT, AGE_CAT,
         NUM_REPORTS_CAT,HOSPITAL_LOS_CAT,TOT_LOS_CAT, Duration,EVENT)


########################################################################
############# testing #################################################
hm = df4[df4$DISCHARGE_LOCATION=="HOME",]

# Reorder the levels of the new categorical variables:
hm$HOSPITAL_LOS_CAT = factor(hm$HOSPITAL_LOS_CAT,
                                 levels = c("<=5","6-10","11-20",">20"))

hm$NUM_REPORTS_CAT = factor(hm$NUM_REPORTS_CAT, 
                                levels = c("<=10","11-20", "21-50", ">50" ))

hm$AGE_CAT = factor(hm$AGE_CAT, 
                        levels = c("<=44", "45-64", "65-79", ">=80"))

df4$HOSPITAL_LOS_CAT = factor(df4$HOSPITAL_LOS_CAT,
                             levels = c("<=5","6-10","11-20",">20"))

df4$NUM_REPORTS_CAT = factor(df4$NUM_REPORTS_CAT, 
                            levels = c("<=10","11-20", "21-50", ">50" ))

df4$AGE_CAT = factor(df4$AGE_CAT, 
                    levels = c("<=44", "45-64", "65-79", ">=80"))

# Contingency tables:
# Home
hm_table_los = table(factor(hm$EVENT),factor(hm$HOSPITAL_LOS_CAT))
round(prop.table(hm_table_los,2),2)

hm_table_notes = table(factor(hm$EVENT),factor(hm$NUM_REPORTS_CAT))
round(prop.table(hm_table_notes,2),2)

hm_table_age = table(factor(hm$EVENT),factor(hm$AGE_CAT))
round(prop.table(hm_table_age,2),2)



# All
all_table_los = table(factor(df4$EVENT),factor(df4$HOSPITAL_LOS_CAT))
round(prop.table(all_table_los,2),2)

all_table_notes = table(factor(df4$EVENT),factor(df4$NUM_REPORTS_CAT))
round(prop.table(all_table_notes,2),2)

all_table_age = table(factor(df4$EVENT),factor(df4$AGE_CAT))
round(prop.table(all_table_age,2),2)


ggplot(hm, aes(HOSPITAL_LOS_CAT, fill = factor(EVENT))) +
  geom_bar(width=0.5)+
  ggtitle('Hospital Length of Stay by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = 'Hospital Length of Stay (days)', y = 'Number of Patients', 
       fill = 'Event')


ggplot(hm) + 
  geom_bar(aes(x = NUM_REPORTS_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Number of Reports by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Number of Reports', y = 'Number of Patients', fill = 'Event')

ggplot(hm) + 
  geom_bar(aes(x = AGE_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Age by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Age Group', y = 'Number of Patients', fill = 'Event')

ggplot(hm) + 
  geom_bar(aes(x = INSURANCE, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Insurance by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45,  hjust=1)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Insurance', y = 'Number of Patients', fill = 'Event')

ggplot(hm) + 
  geom_bar(aes(x = INSURANCE_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Age by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Age Group', y = 'Number of Patients', fill = 'Event')

ggplot(hm) + 
  geom_bar(aes(x = INSURANCE, fill = factor(EVENT)), width = 0.5) +
  ggtitle('INSURANCE by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'INSURANCE-Home', y = 'Number of Patients', fill = 'Event')

ggplot(df4) + 
  geom_bar(aes(x = INSURANCE_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('INSURANCE by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'INSURANCE', y = 'Number of Patients', fill = 'Event')

ggplot(hm) + 
  geom_bar(aes(x = INSURANCE_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Insurance by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Insurance', y = 'Number of Patients', fill = 'Event')

############# testing #################################################
########################################################################

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

# Remove the icd code column from original data frame:
com_no_icd = df_com[,names(df_com) != 'ICD9_CODE']

# Remove duplicates from original data frame:
unique_com = unique(com_no_icd)

# Merge the main dataset and the comorbidity:
com_all = merge(unique_com, df_comorbidity1, by="HADM_ID")
# com_all2= merge(unique_com, df_comorbidity2, by="HADM_ID")

# Remove "INSURANCE":
com_all <- com_all[, !names(com_all) %in% c('INSURANCE','DIAGNOSIS')]

# Take only those who were sent home:
home = com_all[com_all$DISCH_LOC_CAT=='HOME',]
home = home[,names(home) != 'DISCH_LOC_CAT']


########################################################################
########################### SURVIVAL ANALYSIS ##########################
########################################################################

# Remove insignificant variables:
home=select(home,-INSURANCE_CAT)
home=select(home,-MARITAL_STATUS_CAT)
home=select(home,-ETHNICITY_CAT)
home=select(home,-NUM_REPORTS_CAT)
home=select(home,-c(diab,dementia,cevd,pud))
home=select(home,-c(rheumd,diabwc))
home=select(home,-ami)
home=select(home,-aids)
home=select(home,-c(copd,mld))
home=select(home,-GENDER)

# home_vent=select(home_vent,-c(GENDER,diab,dementia,cevd,pud,rheumd,diabwc,ami))


# For Survival Analysis without ventilation:
time = home$Duration
event = home$EVENT
x <- subset(home, select = -c(HADM_ID, Duration, EVENT))
dim(x)

coxph = coxph(Surv(time,event) ~ ., data = x)
summary(coxph)

# Delete insignificant variables
x=select(x,-c(diab,dementia,cevd,pud))
x=select(x,-c(rheumd,diabwc))
x=select(x,-MARITAL_STATUS_CAT)
x=select(x,-ETHNICITY_CAT)
x=select(x,-TOT_LOS_CAT)
x=select(x,-ami)
x=select(x,-aids)
x=select(x,-c(copd,mld))
