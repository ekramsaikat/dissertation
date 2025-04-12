library(tidyverse)
library(easystats)
library(gtsummary)
library(gt)
library(dplyr)

#impor Raw data
Data2<-readxl::read_excel("coded data/WASH Data.xlsx",sheet = 1)
view(Data2)
Data2%>%
  select(16:31)
dplyr::select(Data2,16:31)
Data2|>