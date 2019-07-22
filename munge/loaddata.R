library(breadcrumbs)
library(tidyverse)
library(lubridate)
### Purpose: Read in Data. Match it with cohorts and then filter out old or unused data.
#setwd('VideoAnalysis/')
breadcrumbs::create_project_directories("VideoAnalysis")
here::here()
#names(video_interca)
rm(list=ls())
course_imps<-read_csv('VideoAnalysis_cloud_dir/data/raw/ImplementationDates.csv')
ids<-read_csv('VideoAnalysis_cloud_dir/data/raw/DavidLang_Translation (1).csv')
ids<-unique.data.frame(ids)
cutoff_date<-lubridate::ymd('2019-03-20')
head(course_imps)
head(course_imps)
filter(course_imps,course=="CS")->onecourse ##
coursename<-onecourse$fancycoursename
#coursename<-""
coursegrade<-onecourse$coursegrade
cutoff_date<-lubridate::mdy(onecourse$date)
#read_exce
video_data<-read_csv('VideoAnalysis_cloud_dir/data/raw/VideoInteraction.csv')
names(video_data)
table(video_data$course_display_name)->courses
names(courses)
getwd()

#video_data%>%filter(time<cutoff_date)%>%write_tsv('VideoAnalysis_cloud_dir/data/processed/preinterventionvideo.tsv')
filter(video_data,course_display_name==coursename)->video_df
names(video_df)
rm(video_data)
## filter out data that has only some exposure to the experiment
video_df<-video_df%>%group_by(anon_screen_name)%>%mutate(min_time=min(time)) %>%mutate(enrolled_post_experiment=min_time>cutoff_date)
video_df<-video_df%>%group_by(anon_screen_name)%>%mutate(max_time=max(time)) %>%mutate(active_post_experiment=max_time>cutoff_date)
table(video_df$enrolled_post_experiment,video_df$active_post_experiment)
video_df<-video_df%>%filter(enrolled_post_experiment==TRUE | active_post_experiment==TRUE)
names(ids_with_cohorts) 
names(video_df)






## load activity data and filter out data that only has some exposure to the experiment
#read_csv('VideoAnalysis_cloud_dir/data/raw/DavidLang_ActivityGrade.csv')%>%filter(course_display_name==coursename)->activity_df
#activity_df<-activity_df%>%group_by(student_id)%>%mutate(min_enrollment_time=min(first_submit)) %>%mutate(enrolled_post_experiment=min_enrollment_time>cutoff_date)
#activity_df<-activity_df%>%group_by(student_id)%>%mutate(max_enrollment_time=max(first_submit)) %>%mutate(active_post_experiment=max_enrollment_time>cutoff_date)
#activity_df<-activity_df%>%filter(enrolled_post_experiment==TRUE | active_post_experiment==TRUE)
#names(ids_with_cohorts)


#summary(lm(data = activity_with_cohort,max_grade~Cohort))
#names(activity_with_cohort)
#group_by(student_id)

#table(activity_with_cohort$Cohort)
##View(activity_with_cohort)
#activity_df<-
#length(unique(activity_df$student_id))
#head(activity_df$first_submit)



### load responses
resps<-read_csv(paste0('VideoAnalysis_cloud_dir/data/raw/',coursegrade))
head(resps)
names(resps)

### Generate Attempts
attempts<-resps%>%select(contains("ID"),contains("Review"),contains('Grade'),contains('Cohort'))
names(attempts)[length(attempts)]<-'Cohort'
table(attempts$Cohort)
attempts<-attempts%>%filter(Cohort!="")
#lm(formula = Grade~Cohort,data = attempts)

names(ids)


names(ids)<-c('local_id','anon_screen_name')
names(attempts)[1]<-'local_id'
attempts$local_id<-as.numeric(attempts$local_id)
#str(ids$local_id)
right_join(ids,attempts)->ids_with_cohorts
ids_with_cohorts<-ids_with_cohorts%>%filter(Cohort!="")
table(ids_with_cohorts$Cohort)
names(ids_with_cohorts)
head(ids_with_cohorts)

full_join(video_df,ids_with_cohorts)->video_with_cohort
#rm(video_df)
#full_join(activity_df,ids_with_cohorts,by=c('student_id'='local_id'))->activity_with_cohort
#getwd()
nopath<-str_replace_all(coursename,pattern = "/",replacement = "")
#write_tsv(activity_with_cohort,paste0('VideoAnalysis_cloud_dir/data/processed/','activity_data',nopath,collapse =''))
#rm(activity_with_cohort)
#table(video_with_cohort$Cohort)
#getwd()
write_tsv(video_with_cohort,paste0('VideoAnalysis_cloud_dir/data/processed/','video_data',nopath))
write_rds(video_with_cohort,paste0('VideoAnalysis_cloud_dir/data/processed/','video_data',nopath,'.rds'))
#write_tsv(video_with_cohort,paste0('VideoAnalysis_cloud_dir/data/processed/','video_dataAlgorithms',nopatwh))
write_rds(video_with_cohort,paste0('VideoAnalysis_cloud_dir/data/processed/','video_data',nopath,'.rds'))
#write_rds(video_with_cohort,paste0('VideoAnalysisAlgorithms.rds'))

#write_rds(video_with_cohort,'Algorithms')

write_delim(video_with_cohort,'aaaaaaa',delim='\t',header)
getwd()
video_with_cohort%>%ungroup() %>%filter(Cohort!="")
video_with_cohort$enrolled_post_experiment
filter(video_with_cohort,enrolled_post_experiment==TRUE &Cohort!="")->xp

#### Video Interactions
xp%>%select(Cohort, anon_screen_name,video_code)%>%unique() %>%ungroup()%>%group_by(Cohort,anon_screen_name)%>%summarise(video_count=n())->video_interaction
video_interaction
lm(video_count~Cohort,data=video_interaction)


### Time Savings
xp%>%arr

view(xp)

