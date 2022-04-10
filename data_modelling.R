library(tidyverse)
library(feather)
library(leaps)
library(caret)
library(cvms)
library(tibble) 

df <- read_feather("investment_cleaned.feather")
# df<- read_csv("investment_cleaned.csv")
colnames(df)
str(df)

df<-df |> drop_na(status,funding_total_usd)
df$funding_rounds <- as.ordered(df$funding_rounds) 
df<-df |> filter(status=="acquired"| status=="closed" |status=="operating")
# df<-df |> mutate(status_num=case_when(status=="closed"~0, status=="acquired"~1, status=="operating"~2,))
# df$status_num<-as.factor(df$status_num)

index<-sample(c(0,1),nrow(df),replace=TRUE, prob=c(0.8,0.2))
train<-df[index==0,]
test<-df[index==1,]

lm1<-glm(status_num~funding_total_usd+market+founded_span_days+funding_rounds+first_funding_span_days, 
         family="binomial",
         data=train)
lm2<-glm(status_num~funding_total_usd+founded_span_days+first_funding_span_days, 
         family="binomial",
         data=train)

summary(lm1)
summary(lm2)
summary(rf1)












