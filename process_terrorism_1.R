
source('~/Documents/code/R/strReverse.R')
source('~/Documents/code/R/helper.R')
library(lme4)
library(ggplot2)
library(reshape2)
library(afex)
# set your working directory and read in the data:
setwd('~/Documents/projects/krause_terrorism')
d=read.csv('Terrorism_Survey_KrauseYoung.csv')
colnames(d)
# if you havent yet removed the question-text row:
d=d[2:nrow(d),]

#remove practice rows
d=d[5:nrow(d),]
initialN=nrow(d)
# initial N: 214

# remove people who didn't see either condition
d=nconv(d,'group_assignment')
d=d[!is.na(d$group_assignment),]
nrow(d)
novideoN=initialN-nrow(d)
# novideoN=11
 
# remove people who did not respond to attention check at all
# d=nconv(d,c('Q64','Q66'))
# d0=d[(is.na(d$Q66)),]
# norespN=novideoN-nrows(d)
# respondedN=nrows(d)
# failed to respond: 0

# remove people who failed attention check
d_pass=d[((d$Q64==6 & !is.na(d$Q64))|(d$Q66==6 & !is.na(d$Q66))),]
passedcheckN=nrow(d_pass)
# passed attention check: 105

# remove people who could not recall any question
d$Q30_post
remove=c(156,202)
d=d[-remove,]
# failed recall check: 
d_pass$Q30_post
d=d[-remove,]
# all who passed attention check passed recall check
 
remainingN=nrow(d)
removedN=initialN-remainingN

# initial N: 214
# remaining: 201
# passed attention check: 105

d$SubjID=as.factor(1:nrow(d))
write.csv(d,'krause_terrorism_1_clean.csv')
#######################################

#How many failed the attention check for each group?
passed_econ=nrow(d_pass[d_pass$group_assignment==0,])
total_econ=nrow(d[d$group_assignment==0,])
# econ: 44% pass rate
passed_terr=nrow(d_pass[d_pass$group_assignment==1,])
total_terr=nrow(d[d$group_assignment==1,])
# econ: 59% pass rate

#######################################

# How many terrorist groups can they name?
pre=grep('Q6_pre*',colnames(d))
post=grep('Q105_1*',colnames(d))
d=cconv(d,pre)
d=cconv(d,post)

pre=d[,pre]
post=d[,post]
d$pre_name = 0
d$post_name = 0
for (i in 1:nrow(pre)) {
	count=0
	for (j in 1:ncol(pre)) {
		if (pre[i,j]!='') {
			count=count+1
		}
	}
	d$pre_name[i]=count
	count=0
	for (j in 1:ncol(post)) {
		if (post[i,j]!='') {
			count=count+1
		}
	}
	d$post_name[i]=count
}

a=aggregate(cbind(pre_name,post_name)~group_assignment,d,mean)
a1=melt(a,id.vars='group_assignment')
ggplot(a1,aes(x=factor(group_assignment),y=value,fill=factor(variable)))+geom_bar(stat='identity',position='dodge')+ylab('Average Number of Terrorist Groups Named')+xlab('Control vs Treatment')

d$delta_name=d$post_name-d$pre_name

m=lm(delta_name~group_assignment,d)
summary(m) #p<0.001, t=7.33
#######################################
# Current knowledge about terrorism

pre=grep('Q3_pre*',colnames(d))
post=grep('Q101',colnames(d))
d0=d[,c(pre,post)]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('pre','post','group','SubjID')

a=aggregate(cbind(pre,post)~group,d0,mean)
a=melt(a,id.vars='group')
ggplot(a,aes(x=factor(group),y=value,fill=factor(variable)))+geom_bar(stat='identity',position='dodge')+ylab('Current Knowledge About Terrorism')+xlab('Control vs Treatment')

d0=nconv(d0,c('post','pre'))
d0$delta=d0$post-d$pre
m=lm(delta~group,d0)
summary(m) #n.s.
#######################################
# Level of interest in terrorism

pre=grep('Q4_pre*',colnames(d))
post=grep('Q102',colnames(d))
d0=d[,c(pre,post)]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('pre','post','group','SubjID')

a=aggregate(cbind(pre,post)~group,d0,mean)
a=melt(a,id.vars='group')
ggplot(a,aes(x=factor(group),y=value,fill=factor(variable)))+geom_bar(stat='identity',position='dodge')+ylab('Level of Interest in Terrorism')+xlab('Control vs Treatment')

d0=nconv(d0,c('post','pre'))
d0$delta=d0$post-d$pre
m=lm(delta~group,d0)
summary(m) #n.s.
#######################################
# How effective is terrorism as a tactic?
post=grep('Q10_pre*',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('How effective is terrorism as a tactic? (Post-video)')+xlab('Control vs Treatment')+ylab('Rated effectiveness')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# Threat to personal safety
post=grep('Q14_pre*',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Size of terrorist threat to personal safety (Post-video)')+xlab('Control vs Treatment')+ylab('Rated threat')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
m=lm(post~group,d0)
d0=as.data.frame(d0)
summary(m) #p<0.001,t=-4.34
#######################################
# Threat to US safety
post=grep('Q15_pre*',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Size of terrorist threat to United States (Post-video)')+xlab('Control vs Treatment')+ylab('Rated threat')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #p<0.001,t=-7.60
#######################################
# Rank of deaths from these causes
post=grep('Q18_pre*',colnames(d))
d0=d[,post]
d0=d0[,-6]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('terrorists','car accidents','influenza','lightning','domestic violence','group','SubjID')

d0=melt(d0,id.vars=c('group','SubjID')
d0=nconv(d0,'value')
a=aggregate(value~group+variable,d0,mean)
ggplot(a,aes(x=factor(variable),y=value,fill=factor(group)))+geom_bar(stat='identity',position='dodge')+ggtitle('Mean ranking of number of deaths caused (Post-video)')+xlab('Cause of Deaths')+ylab('Mean rank')+scale_fill_manual('Control Vs Treatment',labels=c('Control','Treatment'),values=c('darkgrey','black'))

d0=nconv(d0,'post')
d0=as.data.frame(d0)
m=lm(value~variable*group,d0)
summary(m) 
#Effect of treat vs control at terrorism: p=0.02,t=2.30

anov=aov(value~variable*group,data=d0)
summary(anov)
# Group x Cause interaction: p<0.001, F=5.20
tuk=TukeyHSD(anov)
# Terrorists (Control) vs Terrorists (Treatment): n.s.
# none of the within-variable cross-group comparisons significant
#######################################
# How much would you like the terrorist?
post=grep('Q25_pre_1{1}',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Liking of terrorist (Post-video)')+xlab('Control vs Treatment')+ylab('How much would you like this person?')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# How much would you like to interact with terrorist?
post=grep('Q25_pre_2{1}',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Interaction with terrorist (Post-video)')+xlab('Control vs Treatment')+ylab('How much would you like to interact with this person?')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# How much would you get along with terrorist?
post=grep('Q25_pre_3{1}',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Getting along with terrorist (Post-video)')+xlab('Control vs Treatment')+ylab('How much would you get along with this person?')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# How similar would terrorist be to you?
post=grep('Q25_pre_4{1}',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('How similar to terrorist (Post-video)')+xlab('Control vs Treatment')+ylab('How similar would this person be to you?')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #t=1.90, p=0.059
#######################################
# Agree: terrorists can't change basic characteristics
post=grep('Q72',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Unchangeability of terrorists (Post-video)')+xlab('Control vs Treatment')+ylab('Agree: Terrorists can\'t change basic characteristics')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #t=1.90, p=0.059
#######################################
# How surprising was video information?
post=grep('Q53',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Surprise (Post-video)')+xlab('Control vs Treatment')+ylab('Rated surprisingness of video information')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# Confidence in opinions about terrorism
post=grep('Q31_pre',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Confidence in opinions of terrorism (Post-video)')+xlab('Control vs Treatment')+ylab('Rated confidence')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# Likelihood of changing opinion
post=grep('Q32_pre',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$SubjID)
colnames(d0)=c('post','group','SubjID')

a=aggregate(post~group,d0,mean)
ggplot(a,aes(x=factor(group),y=post))+geom_bar(stat='identity',position='dodge')+ggtitle('Likelihood of changing opinion on terrorism (Post-video)')+xlab('Control vs Treatment')+ylab('Rated likelihood of changing opinion')

d0=nconv(d0,'post')
d0=fconv(d0,'group')
d0=as.data.frame(d0)
m=lm(post~group,d0)
summary(m) #n.s.
#######################################
# Demographic distributions
d=nconv(d,'Q26_pre')
hist(d$Q26_pre,main='Age Distribution',xlab='age')

d=nconv(d,'Q27_pre')
hist(d$Q27_pre,main='Gender Distribution',xlab='gender',labels=c('male','female'))

d=nconv(d,'Q28_pre')
hist(d$Q28_pre,main='Political Distribution',xlab='political orientation')

d=nconv(d,'Q29_pre')
hist(d$Q29_pre,main='Religious Distribution',xlab='religious affiliation')
#######################################
Do demographics differ across groups?
d=fconv(d,'group_assignment')
t.test(Q26_pre~group_assignment,d)
# n.s.

t.test(Q27_pre~group_assignment,d)
t.test(Q28_pre~group_assignment,d)
t.test(Q29_pre~group_assignment,d)
# all n.s.
