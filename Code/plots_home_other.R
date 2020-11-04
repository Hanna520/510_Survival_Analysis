require(ggplot2)

df_all <- read.csv('df_all.csv')
home<-df_all[df_all$DISCHARGE_LOCATION=='HOME',]

ggplot(home) + 
  geom_bar(aes(x = HOSPITAL_LOS_CAT, fill = factor(EVENT)),width = 0.5) +
  ggtitle('Hospital Length of Stay by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = 'Hospital Length of Stay (days)', y = 'Number of Patients', 
       fill = 'Event')

ggplot(df_all) + 
  geom_bar(aes(x = HOSPITAL_LOS_CAT, fill = factor(EVENT)),width = 0.5) +
  ggtitle('Hospital Length of Stay by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  labs(x = 'Hospital Length of Stay (days)', y = 'Number of Patients', 
       fill = 'Event')

ggplot(home) + 
  geom_bar(aes(x = AGE_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Age by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Age Group', y = 'Number of Patients', fill = 'Event')

ggplot(df_all) + 
  geom_bar(aes(x = AGE_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Age by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Age Group', y = 'Number of Patients', fill = 'Event')

ggplot(home) + 
  geom_bar(aes(x = NUM_REPORTS_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Number of Reports by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Number of Reports', y = 'Number of Patients', fill = 'Event')

ggplot(df_all) + 
  geom_bar(aes(x = NUM_REPORTS_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Number of Reports by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Number of Reports', y = 'Number of Patients', fill = 'Event')

ggplot(df_all) + 
  geom_bar(aes(x = ETHNICITY_CAT, fill = factor(EVENT)), width = 0.5) +
  ggtitle('Ethnicity by Event') + 
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, vjust=1, hjust=1)) +
  scale_color_discrete(name = 'Event') +
  labs(x = 'Ethnicity', y = 'Number of Patients', fill = 'Event')

# plots LOS
xx = table(round(d$LOS), d$returnED72hr)
xx = cbind(xx, xx[,2]/(xx[,1]+xx[,2]))
h = 0:24
plot(h,xx[,3], xlab='Time in ED (Hours)', ylab='Proportion of Return Visits', main='Hours in ED vs Proportion of ED Returns')
lines(supsmu(h,xx[,3],bass=3),col='red')
abline(v=4,col="green") 
abline(v=8,col="green")
abline(v=16,col="green")
