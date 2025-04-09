#load packages

library(tidyverse)
library(gtsummary)
library(easystats)
library(gt)
library(naniar)
install.packages("xlsx")
library(xlsx)

#read data
Data<-readxl::read_excel("coded data/WASH Data.xlsx",sheet = 3)
View(Data)

#check missing values
sum(is.na(Data))
miss_var_summary(Data)

#categorize of age

Data<- Data|>
  mutate(Age_cat=case_when(
  Age>=18 &Age<30~"18-29 years",
  Age>=30&Age<50~"30-49 years",
  Age>=50~"50+"
  ))

#categorize of income
Data<- Data|>
  mutate(income_status=case_when(
    `Household Monthly Income (BDT)`<12500 ~"Lower",
    `Household Monthly Income (BDT)`>=12500&`Household Monthly Income (BDT)`<=21500~"Middle class",
    `Household Monthly Income (BDT)`>21500~"Upper class"
  ))
#export processed data as csv format

write.csv(Data,"D:/dissertation/dissertation/coded data.csv")
Data<-library(readxl)

#import processed data as excel format
Data<-readxl::read_excel("D:/dissertation/dissertation/coded data.xlsx")

# descriptive table
Data|>
  select(1:15)|>
  tbl_summary(
    statistic = list(
      all_continuous()~"{mean}±{sd}"),
    type = c(13,14,15)~"categorical"
  )|>
  as_gt()|>
  gtsave("D:/dissertation/dissertation/tables/1.decriptive table.docx")

#socio-demographic factors influencing perception score(uvregression)
Data|>
  select(1:15,`Total perception score`)|>
  tbl_uvregression(
    method = lm,
    y=`Total perception score`
    )|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("D:/dissertation/dissertation/tables/2.factors influencing total perception score.docx")

#socio-demographic factors influencing total PSQ score(uvregression)

Data|>
  select(1:15,`Total PSQ score`)|>
  tbl_uvregression(
    method = lm,
    y=`Total PSQ score`
  )|>
  bold_p(t=0.05)|>
  as_gt()|>
  gtsave("D:/dissertation/dissertation/tables/3.factors influencing total PSQ score.docx")


   