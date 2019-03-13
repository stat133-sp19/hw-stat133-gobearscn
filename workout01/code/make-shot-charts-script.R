library(jpeg)
library(grid)
library(ggplot2)
klay_scaterplot<-ggplot(data=thompson)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))
court_file<-'../images/nba-court.jpg'
court_image<-rasterGrob(
  readJPEG(court_file),
  width=unit(1,'npc'),
  height=unit(1,'npc'))
klay_shot_chart<-ggplot(data=thompson)+
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('Shot Chart:Klay Thompson(2016 season)')+
  theme_minimal()
curry_shot_chart<-ggplot(data=curry)+
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('Shot Chart:Stephen Curry (2016 season)')+
  theme_minimal()
durant_shot_chart<-ggplot(data=durant)+
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('Shot Chart:Kevin Durant (2016 season)')+
  theme_minimal()
green_shot_chart<-ggplot(data=green)+
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('Shot Chart:Draymond Green (2016 season)')+
  theme_minimal()
iguodala_shot_chart<-ggplot(data=iguodala)+
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('Shot Chart:Andre Iguodala (2016 season)')+
  theme_minimal()

pdf("../images/stephen-curry-shot-chart.pdf",width=6.5,height=5)
plot(curry_shot_chart)
dev.off()


pdf("../images/kevin-durant-shot-chart.pdf",width=6.5,height=5)
plot(durant_shot_chart)
dev.off()

pdf("../images/Draymond-Green-shot-chart.pdf",width=6.5,height=5)
plot(green_shot_chart)
dev.off()

pdf("../images/Klay-Thompson-shot-chart.pdf",width=6.5,height=5)
plot(klay_shot_chart)
dev.off()  

pdf("../images/Andre-Iguodala-shot-chart.pdf",width=6.5,height=5)
plot(iguodala_shot_chart)
dev.off()  

gsw_shot_chart<-ggplot(data=shots_data)+
  annotation_custom(court_image,-250,250,-50,420)+
  geom_point(aes(x=x,y=y,color=shot_made_flag))+
  ylim(-50,420)+
  ggtitle('Shot Chart: GSW (2016 season)')+
  theme_minimal()+
  facet_wrap(~ player)
pdf("../images/gsw-shot-chart.pdf",width=8,height=7)
plot(gsw_shot_chart)
dev.off() 
png("../images/gsw-shot-chart.png",width=1600,height=1400)
plot(gsw_shot_chart)
dev.off() 
