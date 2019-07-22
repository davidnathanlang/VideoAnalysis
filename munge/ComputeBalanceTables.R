library(tidyverse)
dnlang86 <- read_tsv("VideoAnalysis_cloud_dir/data/raw/dnlang86")

dnlang86<-dnlang86%>%unique()
#head(demos)
#write_tsv(demos,'demographics.tsv')
#read_tsv(demos)
library(cobalt)
library(tidyverse)
final_df<-NULL
for (i in list.files('VideoAnalysis_cloud_dir/data/collapsed')){
  final_df<-rbind(final_df,read_tsv(paste0('VideoAnalysis_cloud_dir/data/collapsed/',i))) 
}


library(stringr)
library(cobalt)
#install.packages('cobalt')



left_join(final_df,dnlang86)->user_chars
#for in list.files('VideoAnalysis_cloud_dir/data/collapsed/')

names(final_df)
user_chars%>%group_by(anon_screen_name,course)%>%arrange(country_name)%>%mutate(duplicates=row_number())%>%filter(duplicates==1)->dupes

#filter(user_chars,course!="CS101")%>%filter(course!="StatLearn")%>%mutate(fast=grepl(pattern = "1.25|Fast",x=Cohort))->balance_data
getwd()
write_tsv(dupes,'VideoAnalysis_cloud_dir/data/processed/user_characteristics.tsv')
pt_char<-balance_data%>%select(fast,course,curr_age,year_of_birth,level_of_education)
names(user_chars)

cobalt::bal.plot()
bal.tab(select(pt_char,-fast,-course), treat = pt_char$fast, subclass = pt_char$course ,
        method = "subclassification")->balance_table

?bal.tab
names(pt_char)
summary(lm(data=pt_char,formula =  fast~curr_age + course))->test
summary(lm(data=pt_char,formula =  fast~pt_char + course))
ftest
