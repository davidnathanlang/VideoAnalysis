
### Purpose
#Test Video Hypotheses
library(breadcrumbs)
library(tidyverse)
library(lubridate)

rm(list=ls())
coursename<-'CS101'
course_vector<-c('StatLearn', 'video_dataHumanitiesSciencesStatLearningWinter2016')    
course_vector<-c('CS101' ,'video_dataEngineeringCS101Summer2014')
course_vector<-c('MedStats', 'video_dataMedicineMedStats-SPSelfPaced')
course_vector<-c('Algorithms', 'video_dataAlgorithms.tsv')
course_vector<-c('DBSQLSelfPaced' ,'video_dataDBSQLSelfPaced')

getwd()
coursename<-course_vector[1]
#read_csv('VideoAnalysis_cloud_dir/data/raw/')
video_data<-read_tsv(paste0('VideoAnalysis_cloud_dir/data/processed/',course_vector[2]))
table(video_data$video_new_speed)
table(video_data$active_post_experiment,video_data$Cohort)
table(video_data$Cohort)->cohorts

max_time<-group_by(video_code)%>%max
df<-group_by(anon_screen_name,video_code)
#df<-df%>%filter(Cohort=="(1.25x) Fast"|Cohort=="Normal Speed (1.0x)")
#video_data<-read_rds('VideoAnalysis_cloud_dir/data/processed/video_dataMedicineMedStats-SPSelfPaced.rds')
#write_tsv(video_data,'VideoAnalysis_cloud_dir/data/processed/video_dataAlgorithms.tsv')
table(video_data$Cohort)

df<-video_data%>%filter(Cohort!="") %>%filter(!is.na(anon_screen_name))
df%>%filter(!is.na(event_type))->df
table(df$Cohort)
names(df)
#H1 Time savings

df%>%arrange(anon_screen_name,time) ->df
table(df$anon_screen_name)

df%>% group_by(anon_screen_name,video_code) %>%mutate(time_active=as.numeric(lead(time)-time))%>%
  mutate(time_active=replace_na(time_active,0))%>%
  mutate(time_active=if_else(time_active<1800,time_active,0,missing = 0))  ->df
         
df%>%ungroup()%>%group_by(anon_screen_name,Cohort)%>%summarise(all_time=sum(time_active))         ->cumulative_time

summary(lm(all_time~Cohort,data = cumulative_time))
cumulative_time
### H2 Video Interaction
df%>%select(anon_screen_name,video_code,Cohort)%>%unique()%>%mutate(video_interaction=if_else(str_length(video_code)>0,1,0,missing = 0))%>%
group_by(anon_screen_name,Cohort)%>%summarise(video_count=sum(video_interaction))->vid_interaction
summary(lm(video_count~Cohort,data = vid_interaction))
vid_interaction%>%rename(vid_interaction=video_count)->vid_interaction
### H3 PAUSE BEHAVIOR
df$event_type
df%>%select(anon_screen_name,video_code,Cohort,event_type)%>%mutate(video_interaction=if_else(event_type=="pause_video",1,0,missing = 0))%>%
  group_by(anon_screen_name,Cohort)%>%summarise(video_count=sum(video_interaction))->vid_pause
summary(lm(video_count~Cohort,data = vid_pause))
rename(vid_pause,vid_pause=video_count)->vid_pause


### H3 PAUSE BEHAVIOR
df$event_type
df%>%select(anon_screen_name,video_code,Cohort,event_type)%>%mutate(video_interaction=if_else(event_type=="pause_video",1,0,missing = 0))%>%
  group_by(anon_screen_name,Cohort)%>%summarise(video_count=sum(video_interaction))->vid_pause
summary(lm(video_count~Cohort,data = vid_pause))
rename(vid_pause,vid_pause=video_count)->vid_pause

### H4 REWIND_BEHAVIOR
df$event_type
df%>%
  mutate(video_interaction=if_else(event_type=="seek_video" && video_old_time>video_new_time,
    1,0,missing = 0))%>%
  group_by(anon_screen_name,Cohort)%>%summarise(video_count=sum(video_interaction))->vid_rewind
summary(lm(video_count~Cohort,data = vid_rewind))
vid_rewind%>%rename(video_rewind=video_count)

names(df)
### H5 PAUSE and REWIND
#pause_rewind<-rbind(vid_rewind,vid_pause) %>%group_by(anon_screen_name,Cohort)%>%summarise(vids=sum(video_count))
#summary(lm(vids~Cohort,data = pause_rewind))
#pause_rewind<-pause_rewind%>%rename(rewinds_pause=vids)


### H6 SKIP_BEHAVIOR
df$event_type
df%>%
  mutate(video_interaction=if_else(event_type=="seek_video" && video_old_time<video_new_time,
                                   1,0,missing = 0))%>%
  group_by(anon_screen_name,Cohort)%>%summarise(video_count=sum(video_interaction))->video_skip
summary(lm(video_count~Cohort,data = video_skip))
video_skip<-video_skip%>%rename(video_skip=video_count)
### H6 VIDEO_COMPLETIOn
df$event_type
df%>%select(anon_screen_name,video_code,Cohort,event_type)%>%ungroup()%>%
  mutate(video_interaction=if_else(event_type=="stop_video",
                                   1,0,missing = 0))%>%
  group_by(anon_screen_name,Cohort)%>%summarise(video_count=sum(video_interaction))->vid_stop
summary(lm(video_count~Cohort,data = vid_stop))
vid_stop<-vid_stop%>%rename(video_completion=video_count)

names(df)
df%>%select(video_code,video_current_time)%>%group_by(video_code)%>%summarise(max_video_time=max(as.numeric(video_current_time),na.rm = T))->max_video_time
max_video_time<-max_video_time%>%filter(max_video_time>0)
df$max
left_join(df,max_video_time)%>%
  mutate(percent_completion=as.numeric(video_current_time)/max_video_time) %>%mutate(video_completion=if_else(percent_completion>.9,1,0,0))%>%
  group_by(anon_screen_name,video_code) %>%summarise(max_percent=max(percent_completion))->video_comp

summary(lm(vids_complete~Cohort,data = video_comp))





### H7 
df%>%ungroup()%>%select(anon_screen_name,Cohort,Grade) %>%unique() ->grades
summary(lm(Grade~Cohort,data=grades))
names(vid_rewind)


final_df<-cumulative_time%>%left_join(vid_pause)%>%
left_join(vid_rewind)%>%
  left_join(video_skip)%>%
  left_join(grades)%>%
  left_join(vid_interaction)
final_df<-final_df%>%mutate(course=coursename)
#mkdir('VideoAnalysis_cloud_dir/processed/finaldata')
write_tsv(final_df,paste0('VideoAnalysis_cloud_dir/data/collapsed/finaldata',coursename))
names(video_comp)
table(video_comp$Cohort)
