#한국복지패널 데이터(https://www.koweps.re.kr:442/data/data/list.do)

install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
raw_welfare<-read.spss("data_spss_koweps2014.sav", to.data.frame=T)
welfare<-raw_welfare; welfare

dim(welfare)
str(welfare)
head(welfare)
summary(welfare)
View(welfare)

welfare<- rename(welfare,
                 sex= h0901_4,
                 birth= h0901_5,
                 income= h09_din)

#분석1: 성별에 따른 소득
class(welfare$sex)
summary(welfare$sex)
table(welfare$sex)

welfare$sex<-ifelse(welfare$sex==9, NA, welfare$sex)
table(is.na(welfare$sex))

welfare$sex<- ifelse(welfare$sex==1, "male", "female")
table(welfare$sex)
qplot(welfare$sex)

class(welfare$income)
summary(welfare$income)
qplot(welfare$income)+xlim(0,10000)

table(is.na(welfare$income))

sex_income<-welfare %>% 
  group_by(sex) %>% 
  summarise(mean_income=mean(income))
sex_income

ggplot(data=sex_income, aes(x=sex, y=mean_income))+geom_col()


#분석2: 나이와 소득의 관계
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

welfare$birth<-ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))

welfare$age<-2014-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

age_income<-welfare %>% 
  group_by(age) %>% 
  summarise(mean_income=mean(income))
age_income

ggplot(data=age_income, aes(x=age, y=mean_income))+geom_point()

#분석3: 연령대에 따른 소득
welfare<-welfare %>% 
  mutate(ageg = ifelse(age<30, "young",
                       ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qplot(welfare$ageg)

welfare_income<-welfare %>% 
  filter(ageg != "young") %>% 
  group_by(ageg) %>% 
  summarise(mean_income=mean(income))
welfare_income

ggplot(data=welfare_income, aes(x=ageg, y=mean_income))+ geom_col()

#분석4: 연령대 및 성별에 따른 소득
sex_income<-welfare %>% 
  filter(ageg !="young") %>% 
  group_by(ageg, sex) %>%
  summarise(mean_income=mean(income))
sex_income

ggplot(data=sex_income, aes(x=ageg, y=mean_income, fill=sex))+ geom_col(position="dodge")

