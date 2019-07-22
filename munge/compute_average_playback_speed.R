library(tidyverse)

df<-read_tsv('VideoAnalysis_cloud_dir/data/processed/preinterventionvideo.tsv')

df%>% arrange(anon_screen_name, time)

table(df$event_type)
df<-df%>%mutate(changes_speed=event_type=="speed_change_video")
#df$ch
speed_change<-df%>%select(anon_screen_name,changes_speed)

speed_change%>%group_by(anon_screen_name)%>%summarise(max_speed=max(changes_speed))->final
summary(final$max_speed)
head(speed_change)

max(c(FALSE,FALSE))


affected_users<-inner_join(df,final%>%filter(max_speed==1))

affected_users<-affected_users%>%mutate(dl_speed=video_new_speed)
affected_users<-affected_users%>%fill(dl_speed)
affected_users<-affected_users%>%fill(video_old_speed,.direction = "up")
affected_users$dl_speed<-ifelse(is.na(affected_users$dl_speed),affected_users$video_old_speed,affected_users$dl_speed)

summary(affected_users$dl_speed)

average_user<-affected_users%>%group_by(anon_screen_name)%>%summarise(average_speed=mean(dl_speed))

summary(average_user)

average_playback_speed<-ggplot(average_user, aes(average_speed)) + stat_ecdf(geom = "step") +xlab('Average Speed') +ylab('Percent') +ggtitle('Average Playback Speed by User')
ggsave('C:\\Users\\dnlang86\\Dropbox\\Apps\\Overleaf\\Dissertation Proposal David Lang\\Attr\\Figures\\playbackspeed.jpg',average_playback_speed)
?ggsave

left_join(final,average_user)->pretreatment_playback_speed
write_tsv(pretreatment_playback_speed,'VideoAnalysis_cloud_dir/data/processed/pretreatment_playback_speed.tsv')


