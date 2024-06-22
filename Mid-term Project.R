
mydata <-read.csv("C:/Dataset_midterm_Section(B).csv", header=TRUE, sep=",")
mydata


names(mydata)


str(mydata)


summary(mydata)

library(dplyr)
mydata%>%summarise_if(is.numeric,sd)


sum(is.na(mydata))


colSums(is.na(mydata))


which(is.na(mydata$age))

install.packages("ggplot2")
library(ggplot2)
age <- mydata$age
ggplot(data = mydata, aes(x = age)) +
  geom_histogram() +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

mydata$age[is.na(mydata$age)] <- mean(mydata$age,na.rm= FALSE)
mydata$age

library(ggplot2)
ggplot(data = mydata, aes(x = "", y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age", y = "Age")

outliers <- mydata$age[mydata$age > 150]
mydata$age[mydata$age %in% outliers] <- NA
mean_age <- mean(mydata$age, na.rm = TRUE)
mydata$age[is.na(mydata$age)] <- mean_age
print(mydata$age)



which(is.na(mydata$anaemia))

mydata$anaemia[is.na(mydata$anaemia)] <- mean(mydata$anaemia,na.rm=FALSE)
mydata$anaemia



which(is.na(mydata$creatinine_phosphokinase))
sum(mydata$creatinine_phosphokinase == "")
sum(mydata$creatinine_phosphokinase == 0)

mydata$creatinine_phosphokinase <- as.numeric(mydata$creatinine_phosphokinase)

mydata <- mydata[!is.na(mydata$creatinine_phosphokinase), ]

ggplot(data = mydata, aes(x = creatinine_phosphokinase)) +
  geom_histogram() +
  labs(title = "Histogram of Creatinine Phosphokinase", x = "Creatinine Phosphokinase", y = "Frequency")

mydata$creatinine_phosphokinase[is.na(mydata$creatinine_phosphokinase)] <- mean(mydata$creatinine_phosphokinase, na.rm = TRUE)
mydata$creatinine_phosphokinase

print(mydata)

ggplot(data = mydata, aes(x = "", y = creatinine_phosphokinase)) +
  geom_boxplot() +
  labs(title = "Boxplot of Creatinine Phosphokinase", y = "Creatinine Phosphokinase")

outliers <- mydata$creatinine_phosphokinase[mydata$creatinine_phosphokinase > 6000]
mydata$creatinine_phosphokinase[mydata$creatinine_phosphokinase %in% outliers] <- NA
mean_creatinine_phosphokinase <- mean(mydata$creatinine_phosphokinase, na.rm = TRUE)
mydata$creatinine_phosphokinase[is.na(mydata$creatinine_phosphokinase)] <- mean_creatinine_phosphokinase
print(mydata$creatinine_phosphokinase)



which(is.na(mydata$diabetes))
mydata$diabetes[is.na(mydata$diabetes)] <- mean(mydata$diabetes,na.rm=TRUE)
mydata$diabetes



which(is.na(mydata$ejection_fraction))
which(is.na(mydata$high_blood_pressure))



which(is.na(mydata$platelets))

platelets <- mydata$platelets
ggplot(data = mydata, aes(x = platelets)) +
  geom_histogram() +
  labs(title = "Histogram of Platelets", x = "Platelets", y = "Frequency")

mydata$platelets[is.na(mydata$platelets)] <- mean(mydata$platelets, na.rm = TRUE)
mydata$platelets

ggplot(data = mydata, aes(x = "", y = platelets)) +
  geom_boxplot() +
  labs(title = "Boxplot of Platelets", y = "Platelets")

outliers <- mydata$platelets[mydata$platelets > 600000 | mydata$platelets < 200000]
mydata$platelets[mydata$platelets %in% outliers] <- NA
mean_platelets <- mean(mydata$platelets, na.rm = TRUE)
mydata$platelets[is.na(mydata$platelets)] <- mean_platelets
print(mydata$platelets)




which(is.na(mydata$serum_creatinine))
ggplot(data = mydata, aes(x = "", y = serum_creatinine)) +
  geom_boxplot() +
  labs(title = "Boxplot of serum_creatinine", y = "serum_creatinine")

outliers <- mydata$serum_creatinine[mydata$serum_creatinine > 5]

mydata$serum_creatinine[mydata$serum_creatinine %in% outliers] <- NA

mean_serum_creatinine <- mean(mydata$serum_creatinine, na.rm = TRUE)

mydata$serum_creatinine[is.na(mydata$serum_creatinine)] <- mean_serum_creatinine

print(mydata$serum_creatinine)



which(is.na(mydata$serum_sodium))


which(is.na(mydata$sex))
unique(mydata$sex)
mydata$sex[mydata$sex == "Maleee"] <- "Male"
mydata$sex[mydata$sex == "Femmale"] <- "Female"
unique(mydata$sex)



which(is.na(mydata$smoking))

mydata$smoking[is.na(mydata$smoking)] <- mean(mydata$smoking, na.rm = TRUE)
mydata$smoking

which(is.na(mydata$time))
which(is.na(mydata$DEATH_EVENT))+







