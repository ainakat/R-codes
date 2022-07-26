#load packages 
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(magrittr)
library(plyr)


################################# PHASE 1

report.1 <- read.csv("/Users/aina/Documents/RA Projects/Career School/report.1.csv")
attach(report.1)
report.2 <- read.csv("/Users/aina/Documents/RA Projects/Career School/report.2.csv")
attach(report.2)

report.1 <- drop_na(report.1)
report.2 <- drop_na(report.2)

report.ALL <- rbind.data.frame(report.1, report.2)

report.ALL <- report.ALL %>%
  mutate("student_annual_income" = as.numeric(u_student_socioeconomics.annual_income))

write.csv(report.ALL, "/Users/aina/Documents/RA Projects/Career School/almost_clean.csv")

###################################################### PHASE 2 (removing dollar signs was tricky)

report.NEW <- read.csv("/Users/aina/Documents/RA Projects/Career School/almost_clean.csv")

report.NEW$income2 <- (gsub(",","",report.NEW$u_student_socioeconomics.annual_income))

report.NEW <- report.NEW %>%
  select(Student_Number,u_student_socioeconomics.household_size,income2) %>%
  mutate("u_student_socioeconomics.annual_income"= as.double(income2)) %>%
  drop_na() %>%
  select(Student_Number,u_student_socioeconomics.household_size,u_student_socioeconomics.annual_income)

hh.income.lessthan.15k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income <= 15000)
  
hh.income.15k.to.30k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 15000) %>%
  filter(u_student_socioeconomics.annual_income <= 30000)

hh.income.30k.to.40k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 30000) %>%
  filter(u_student_socioeconomics.annual_income <= 40000)

hh.income.40k.to.50k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 40000) %>%
  filter(u_student_socioeconomics.annual_income <= 50000)
  
hh.income.50k.to.70k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 50000) %>%
  filter(u_student_socioeconomics.annual_income <= 70000)

hh.income.70k.to.100k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 70000) %>%
  filter(u_student_socioeconomics.annual_income <= 100000)  

hh.income.100k.to.150k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 100000) %>%
  filter(u_student_socioeconomics.annual_income <= 150000)

hh.income.150k.to.200k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 150000) %>%
  filter(u_student_socioeconomics.annual_income <= 200000)

hh.income.greater.200k <- report.NEW %>%
  filter(u_student_socioeconomics.annual_income > 200000) 

sum <- 4101+5+64+119+223+380+1353+12

write.csv(hh.income.lessthan.15k, "/Users/aina/Documents/RA Projects/Career School/hh.income.lessthan.15k.csv")
write.csv(hh.income.15k.to.30k, "/Users/aina/Documents/RA Projects/Career School/hh.income.15k.to.30k.csv")
write.csv(hh.income.30k.to.40k, "/Users/aina/Documents/RA Projects/Career School/hh.income.30k.to.40k.csv")
write.csv(hh.income.40k.to.50k, "/Users/aina/Documents/RA Projects/Career School/hh.income.40k.to.50k.csv")
write.csv(hh.income.50k.to.70k, "/Users/aina/Documents/RA Projects/Career School/hh.income.50k.to.70k.csv")
write.csv(hh.income.70k.to.100k, "/Users/aina/Documents/RA Projects/Career School/hh.income.70k.to.100k.csv")
write.csv(hh.income.100k.to.150k, "/Users/aina/Documents/RA Projects/Career School/hh.income.100k.to.150k.csv")
write.csv(hh.income.150k.to.200k, "/Users/aina/Documents/RA Projects/Career School/hh.income.150k.to.200k.csv")
write.csv(hh.income.greater.200k, "/Users/aina/Documents/RA Projects/Career School/hh.income.greater.200k.csv")


number.of.students <- c(4101,1353,380,223,119,64,8,4,5)
income.brackets <- c("hh.income.lessthan.15k",
                     "hh.income.15k.to.30k",
                     "hh.income.30k.to.40k",
                     "hh.income.40k.to.50k",
                     "hh.income.50k.to.70k",
                     "hh.income.70k.to.100k",
                     "hh.income.100k.to.150k",
                     "hh.income.150k.to.200k",
                     "hh.income.greater.200k")

table <- data_frame(number.of.students,income.brackets)

write.csv(table, "/Users/aina/Documents/RA Projects/Career School/aggregate.csv")


