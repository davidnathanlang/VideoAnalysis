library(tidyverse)
rm(list=ls())
get_grades<-function(df){
  df<-df%>%select("Student ID","Cohort Name","Grade","course") 
}

pt<-read_tsv('VideoAnalysis_cloud_dir/data/processed/pretreatment_playback_speed.tsv')
course_imps<-read_csv('VideoAnalysis_cloud_dir/data/raw/ImplementationDates.csv')
ids<- read_csv("VideoAnalysis_cloud_dir/data/raw/DavidLang_Translation (1).csv")
names(ids)<-c("Student ID","anon_screen_name")
for (i in course_imps$coursegrade){
  print(i)
}

algorithms<-read_csv('VideoAnalysis_cloud_dir/data/raw/AlgorithmGrades.csv') %>%mutate(course="algorithms")
CS101<-read_csv('VideoAnalysis_cloud_dir/data/raw/CS101grades.csv')%>%mutate(course="CS101")
SQL<-read_csv('VideoAnalysis_cloud_dir/data/raw/sql_grades.csv')%>%mutate(course="SQL")
StatLearn<-read_csv('VideoAnalysis_cloud_dir/data/raw/StatLearnGrade.csv')%>%mutate(course="StatLearn")
MedStats<-read_csv('VideoAnalysis_cloud_dir/data/raw/MedStatsgrades.csv')%>%mutate(course="MedStats")



algorithms<-get_grades(algorithms)
CS101<-get_grades(CS101)
SQL<-get_grades(SQL)
StatLearn<-get_grades(StatLearn)
MedStats<-get_grades(MedStats)
#names(ids)<-c("Student ID","anon_screen_name")
#names(ids)
final_grades<-rbind(CS101,SQL,StatLearn,MedStats,algorithms)
#final_grades <- read_delim("VideoAnalysis_cloud_dir/data/raw/final_grades.txt", 
#                           "\t", escape_double = FALSE, trim_ws = TRUE)

full_join(pt,ids)->pt_ids
names(pt)
names(pt_ids)
summary(pt_ids)
left_join(pt_ids,final_grades)->pt_with_grades

pt_with_grades%>%filter(!is.na(Grade))%>%mutate(grade=Grade)->pt_with_grades
#grades
names(pt_with_grades)
lm(data=pt_with_grades,Grade~average_speed)
#install.packages('binscatter')
library(binscatter)
#install.packages('statar')
statar::stat_binmean()
library(esquisse)
library(statar)
library(tidyverse)
p<-ggplot(data = pt_with_grades) +
  aes(x = average_playback_speed, y = Grade) +
  geom_point(color = "#0c4c8a") +
  theme_minimal()
pt_with_grades%>%mutate(average_speed=replace(average_speed,is.na(average_speed),1))%>%filter(!is.na(pt_with_grades$grade))->grades_no_nulls
names(grades_no_nulls)
summary(lm(data = grades_no_nulls,formula = grade~poly(average_speed,2)))
grades_no_nulls<-grades_no_nulls%>%filter(max_speed==1) 
g<-ggplot(grades_no_nulls,aes(x=average_speed,y=grade))+stat_binmean(n=30)+ stat_smooth(method = "lm",formula =y~poly(x,2), se = TRUE)


