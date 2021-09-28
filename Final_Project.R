library("tidyverse")
library("nycflights13")
library("dplyr")
df1 <- read.csv("census_income_original_2.csv")
df2 <- transform(df1, income_c = as.numeric(income_c))
#min(df2$age)
#max(df2$age)
df3 <- df2 %>% drop_na()
dfFrom17To30 <- subset(df3,df3$age>=17 & df3$age<=30)
aver.income17_30 <- mean(dfFrom17To30$income_c)

dfFrom30To50 <- subset(df3,df3$age>30 & df3$age<=50)
aver.income30_50 <- mean(dfFrom30To50$income_c)

dfFrom50To70 <- subset(df3,df3$age>50 & df3$age<=70)
aver.income50_70 <- mean(dfFrom50To70$income_c)

dfFrom70To90 <- subset(df3,df3$age>70 & df3$age<=90)
aver.income70_90 <- mean(dfFrom70To90$income_c)

#from the four categories I see  there is no relation between age and salaries.

incomeMoreThan50 <- subset(df1 , df1$income == ">50K" )
averageAgeForMoreThan50k <- mean(incomeMoreThan50$age)

incomeLessThan50 <- subset(df1 , df1$income == "<=50K" )
averageAgeForLessTan50k <- mean(incomeLessThan50$age)

#from this information we conclude that at 36 or 37 years old peope's income is less than 50k 
#but people's income is more than 50k at 44 or 45 years old

dfGender <- within(df3, Gender[Gender == "Female" | Gender == "female" | Gender == "f" ] <- "Female")
dfGender <- within(dfGender , Gender[Gender == "Male" | Gender == "M" | Gender == "m" | Gender == "malee" | Gender == "m " | Gender == "male"] <- "Male")

dfMales <- subset(dfGender , dfGender$Gender == "Male")
dfFemales <- subset(dfGender , dfGender$Gender == "Female")
workingHoursForMales <- mean(dfMales$hours.per.week)
workingHoursForFemales <- mean(dfFemales$hours.per.week)

#male working hours is 42,5 per week and females working hours is 36,4 

AverageMaleIncome <- mean(dfMales$income_c)
AverageFemaleIncome <- mean(dfFemales$income_c)

#male income is about 10362 and female is 9832 $ , not a big difference according to the working hours.


malesFedralGov <- 669/22425*100
femalesFedralGov <- 320/10908*100


count(dfMales , dfMales$workclass)
count(dfFemales , dfFemales$workclass)

malesFedralGov <- 669/22425*100
femalesFedralGov <- (320/10908)*100

maleNeverWorked <- 5/22425*100
femaleNeverWorked <- 2/10908*100

maleWithoutPay <- 9/22425*100
femaleWithoutPay <- 5/10908

maleLocalGov <- 1300/22425*100
femaleLocalGov <- 850/10908*100

malePrivate <- 15334/22425*100
femalePrivate <- 7836/10908*100

#from numbers I believe 100% that gender equality exists. 

count(dfMales , dfMales$education)
count(dfFemales , dfFemales$education)

maleDoctorate <- 362/22425*100
femaleDoctorate <- 95/10908*100

maleBechelores <- 3899/22425*100
femaleBechelores <- 1646/10908*100

maleSome_college <- 4575/22425*100
femaleSome_college <-2834/10908*100

male1st_12th <- (647+754+290+124+255+495+371)/22425*100
female1st_12th <- (299+433+144+46+86+160+144)/10908*100

#from these data , education equality exists.

#not important note : I didnot use group by but after using it ,things became much easier. 

workClass <- df3 %>% group_by(workclass) %>% summarise(sum_income = sum(income_c), number = n() , average = mean(income_c))

#I have a question , how people without pay takes high income (27567 $)?? You will find it in workClass data frame.

workingHours <- df3 %>% group_by(hours.per.week) %>%  summarise(average = mean(income_c))

#No relation between number of hours of working and the income.
