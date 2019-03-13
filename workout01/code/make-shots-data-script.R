#Data Preparation
##This script is meant to deal with data preparation in order for further analysis.
###inputs:The input of this script is the raw datafrom the five players
###outputs:The output of this script is summary of the five player and the shots-data of the five players

library(dplyr)
curry<-read.csv('C://Users/26073/Desktop/berkeley/classes/stat133/workout01/data/stephen-curry.csv',stringsAsFactors = FALSE)
durant<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//kevin-durant.csv',stringsAsFactors = FALSE)
green<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//draymond-green.csv',stringsAsFactors = FALSE)
thompson<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//klay-thompson.csv',stringsAsFactors = FALSE)
iguodala<-read.csv('C://Users//26073//Desktop//berkeley//classes//stat133//workout01//data//andre-iguodala.csv',stringsAsFactors = FALSE)
str(curry)

curry$shot_made_flag[curry$shot_made_flag=='n']<-'shot_no'
curry$shot_made_flag[curry$shot_made_flag=='y']<-'shot_yes'
durant$shot_made_flag[durant$shot_made_flag==c('n')]<-'shot_no'
durant$shot_made_flag[durant$shot_made_flag==c('y')]<-'shot_yes'
green$shot_made_flag[green$shot_made_flag==c('n')]<-'shot_no'
green$shot_made_flag[green$shot_made_flag==c('y')]<-'shot_yes'
thompson$shot_made_flag[thompson$shot_made_flag==c('n')]<-'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag==c('y')]<-'shot_yes'
iguodala$shot_made_flag[iguodala$shot_made_flag==c('n')]<-'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag==c('y')]<-'shot_yes'

curry=mutate(curry,minute=(12*(period-1))+12-minutes_remaining,player='Stephen Curry')
durant=mutate(durant,minute=12*(period-1)+12-minutes_remaining,player='Kevin Durant')
green=mutate(green,minute=12*(period-1)+12-minutes_remaining,player='Draymond Green')
thompson=mutate(thompson,minute=12*(period-1)+12-minutes_remaining,player='Klay Thompson')
iguodala=mutate(iguodala,minute=12*(period-1)+12-minutes_remaining,player='Andre Iguodala')

sink(file = '..//output/stephen-curry-summary.txt')
  summary(curry)
sink()


sink(file = '..//output/kevin-durant-summary.txt')
summary(durant)
sink()


sink(file = '..//output/Draymond-Green-summary.txt')
summary(green)
sink()


sink(file = '..//output/Klay-Thompson-summary.txt')
summary(thompson)
sink()


sink(file = '..//output/Andre-Iguodala-summary.txt')
summary(iguodala)
sink()

shots_data=rbind(curry,durant,green,thompson,iguodala)
write.csv(x=shots_data,
  file = 'shots-data.csv'
)

sink(file='..//output/shots-data-summary.txt')
summary(shots_data)
sink()

