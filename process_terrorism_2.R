
source('~/Documents/code/R/strReverse.R')
source('~/Documents/code/R/helper.R')
library(lme4)
library(ggplot2)
library(reshape2)
library(afex)
# set your working directory and read in the data:
setwd('~/Documents/projects/krause_terrorism/raw_data')
d=read.csv('Terrorism_Survey_KrauseYoung_2.csv')
colnames(d)
# if you havent yet removed the question-text row:
d=d[2:nrow(d),]

# remove test run
# d=d[2:nrow(d),]

initialN=nrow(d)
# initial N: 406

# remove people who didn't see either condition
d=nconv(d,'group_assignment')
d=d[!is.na(d$group_assignment),]
nrow(d)
# N=406
 

# remove people who could not recall any question
d$Q30_post
remove=c(34,126,146,180,197)
d=d[-remove,]
# passed recall check: 401

d=nconv(d,c('Q64','Q66'))
# remove people who failed attention check
d_pass=d[((d$Q64==4 & !is.na(d$Q64))|(d$Q66==4 & !is.na(d$Q66))),]
passedcheckN=nrow(d_pass)
# passed attention check & recall check: 299
 

# initial N: 406
# remaining: 401
# passed attention/recall check: 299

d$SubjID=as.factor(1:nrow(d))

write.csv(d,'krause_terrorism_2_clean.csv')
write.csv(d_pass,'krause_terrorism_2_goodsubs_clean.csv')

#######################################

#How many failed the attention check for each group?
passed_econ=nrow(d_pass[d_pass$group_assignment==0,])
total_econ=nrow(d[d$group_assignment==0,])
# econ: 67% pass rate
passed_terr=nrow(d_pass[d_pass$group_assignment==1,])
total_terr=nrow(d[d$group_assignment==1,])
# terr: 82% pass rate

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

a=aggregate(cbind(pre_name,post_name)~group_assignment*medium_assignment,d,mean)
a1=melt(a,id.vars=c('group_assignment','medium_assignment'))
labs=c('0'='Text','1'='Audio')
ggplot(a1,aes(x=factor(group_assignment),y=value,fill=factor(variable)))+geom_bar(stat='identity',position='dodge')+ylab('Average Number of Terrorist Groups Named')+xlab('Control vs Treatment')+ylim(0,10)+scale_fill_discrete(name='Timepoint',labels=c('Pre','Post'))+facet_grid(.~medium_assignment,labeller=as_labeller(labs))

d$delta_name=d$post_name-d$pre_name

m=lm(delta_name~group_assignment*medium_assignment,d)
summary(m) 
#group: p<0.001, t=7.05
#interaction: p=0.067
#######################################
# Current knowledge about terrorism

pre=grep('Q3_pre*',colnames(d))
post=grep('Q101',colnames(d))
d0=d[,c(pre,post)]
d0=cbind(d0,d$group_assignment,d$medium_assignment,d$SubjID)
colnames(d0)=c('pre','post','group','medium','SubjID')

a=aggregate(cbind(pre,post)~group*medium,d0,mean)
a=melt(a,id.vars=c('group','medium'))
ggplot(a,aes(x=factor(group),y=value,fill=factor(variable)))+geom_bar(stat='identity',position='dodge')+ylab('Current Knowledge About Terrorism')+xlab('Control vs Treatment')+ylim(0,7)+scale_fill_discrete(name='Timepoint',labels=c('Pre','Post'))+scale_x_discrete(labels=c('0'='Control','1'='Treatment'))+facet_grid(.~medium,labeller=as_labeller(labs))

d0=nconv(d0,c('post','pre'))
d0$delta=d0$post-d$pre
m=lm(delta~group*medium,d0)
summary(m) #n.s.
#######################################
# Level of interest in terrorism

pre=grep('Q4_pre*',colnames(d))
post=grep('Q102',colnames(d))
d0=d[,c(pre,post)]
d0=cbind(d0,d$group_assignment,d$medium_assignment,d$SubjID)
colnames(d0)=c('pre','post','group','medium','SubjID')

a=aggregate(cbind(pre,post)~group*medium,d0,mean)
a=melt(a,id.vars=c('group','medium'))
ggplot(a,aes(x=factor(group),y=value,fill=factor(variable)))+geom_bar(stat='identity',position='dodge')+ylab('Level of Interest in Terrorism')+xlab('Control vs Treatment')+ylim(0,7)+scale_fill_discrete(name='Timepoint',labels=c('Pre','Post'))+scale_x_discrete(labels=c('0'='Control','1'='Treatment'))+facet_grid(.~medium,labeller=as_labeller(labs))


d0=nconv(d0,c('post','pre'))
d0$delta=d0$post-d$pre
m=lm(delta~group,d0)
summary(m) #n.s.
#######################################
# How effective is terrorism as a tactic?
post=grep('Q10_pre*',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$medium_assignment,d$SubjID)
colnames(d0)=c('post','group','medium','SubjID')
d0[,1]=d0[,1]-1
d0=d0[d0[,1]!=0,]

a=aggregate(post~group*medium,d0,mean)
ggplot(a,aes(x=factor(group),y=post,fill=factor(medium)))+geom_bar(stat='identity',position='dodge')+ggtitle('How effective is terrorism as a tactic? (Post-stimulus)')+xlab('Control vs Treatment')+ylab('Rated effectiveness')+ylim(0,7)+scale_x_discrete(labels=c('0'='Control','1'='Treatment'))+scale_fill_discrete(name='Medium',labels=c('Text','Audio'))

d0=nconv(d0,'post')
d0=fconv(d0,c('group','medium'))
d0=as.data.frame(d0)
m=lm(post~group*medium,d0)
summary(m) #n.s.
#######################################
# Threat to personal safety
post=grep('Q14_pre*',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$medium_assignment,d$SubjID)
colnames(d0)=c('post','group','medium','SubjID')
d0[,1]=d0[,1]-1
d0=d0[d0[,1]!=0,]

a=aggregate(post~group*medium,d0,mean)
ggplot(a,aes(x=factor(group),y=post,fill=factor(medium)))+geom_bar(stat='identity',position='dodge')+ggtitle('Size of terrorist threat to personal safety (Post-stimulus)')+xlab('Control vs Treatment')+ylab('Rated threat')+ylim(0,7)+scale_x_discrete(labels=c('0'='Control','1'='Treatment'))+scale_fill_discrete(name='Medium',labels=c('Text','Audio'))


d0=nconv(d0,'post')
d0=fconv(d0,c('group','medium'))
d0=as.data.frame(d0)
m=lm(post~group*medium,d0)
summary(m) #n.s.
#######################################
# Threat to US safety
post=grep('Q15_pre*',colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$medium_assignment,d$SubjID)
colnames(d0)=c('post','group','medium','SubjID')
d0[,1]=d0[,1]-1
d0=d0[d0[,1]!=0,]

a=aggregate(post~group*medium,d0,mean)
ggplot(a,aes(x=factor(group),y=post,fill=factor(medium)))+geom_bar(stat='identity',position='dodge')+ggtitle('Size of terrorist threat to United States (Post-stimulus)')+xlab('Control vs Treatment')+ylab('Rated threat')+ylim(0,7)+scale_x_discrete(labels=c('0'='Control','1'='Treatment'))+scale_fill_discrete(name='Medium',labels=c('Text','Audio'))


d0=nconv(d0,'post')
d0=fconv(d0,c('group','medium'))
d0=as.data.frame(d0)
m=lm(post~group*medium,d0)
summary(m) #n.s.

#######################################
# Rank of deaths from these causes
post=grep('Q18_pre*',colnames(d))
d0=d[,post]
d0=d0[,-6]
d0=cbind(d0,d$group_assignment,d$medium_assignment)
colnames(d0)=c('terrorists','car accidents','influenza','lightning','domestic violence','group','medium')

d0=melt(d0,id.vars=c('group','medium'))
d0=nconv(d0,'value')
a=aggregate(value~group+medium+variable,d0,mean)
ggplot(a,aes(x=factor(variable),y=value,fill=factor(group)))+geom_bar(stat='identity',position='dodge')+ggtitle('Mean ranking of number of deaths caused (Post-stimulus)')+xlab('Cause of Deaths')+ylab('Mean rank')+ylim(0,5)+scale_fill_manual('Control Vs Treatment',labels=c('Control','Treatment'),values=c('darkgrey','black'))+facet_grid(.~medium,labeller=as_labeller(labs))

d0=nconv(d0,'post')
d0=fconv(d0,c('group','medium','variable'))
d0=as.data.frame(d0)
m=lm(value~variable*group*medium,d0)
summary(m) 
#Effect of treat vs control at terrorism, text: p=0.004,t=2.86
#Effect of treat vs control at terrorism, audio: n.s. (i.e. no interaction)

anov=aov(value~variable*group*medium,data=d0)
summary(anov)
# Group x Variable interaction: p=0.03, F=2.66
tuk=TukeyHSD(anov)

#######################################
# How much would you like the terrorist?
p=krause_process_variable(d,'Q25_pre_1{1}','Liking of terrorist (Post-stimulus)','How much would you like this person?')
#n.s.
#######################################
# How much would you like to interact with terrorist?
p=krause_process_variable(d,'Q25_pre_2{1}','Interaction with terrorist (Post-stimulus)','How much would you like to interact with this person?')
#n.s.
#######################################
# How much would you get along with terrorist?
p=krause_process_variable(d,'Q25_pre_3{1}','Getting along with terrorist (Post-stimulus)','How much would you get along with this person?')

#######################################
# How similar would terrorist be to you?
p=krause_process_variable(d,'Q25_pre_4{1}','How similar to terrorist (Post-stimulus)','How similar would this person be to you?')

 #n.s.
#######################################
# Agree: terrorists can't change basic characteristics

p=krause_process_variable(d,'Q72','Unchangeability of terrorists (Post-stimulus)','Agree: Terrorists can\'t change basic characteristics')
 #n.s.
#######################################
# How surprising was video information?
p=krause_process_variable(d,'Q53','Surprise (Post-stimulus)','Rated surprisingness of stimulus information')
#n.s.
#######################################
# Confidence in opinions about terrorism
p=krause_process_variable(d,'Q31_pre','Confidence in opinions of terrorism (Post-stimulus)','Rated confidence')
 #n.s.
#######################################
# Likelihood of changing opinion
p=krause_process_variable(d,'Q32_pre','Likelihood of changing opinion on terrorism (Post-stimulus)','Rated likelihood of changing opinion')
 #n.s.
#######################################
# Demographic distributions
krause_process_demographic(d,'Q26_pre','Age Distribution','Age')
krause_process_demographic(d,'Q28_pre','Political Distribution','Political Orientation')

d=nconv(d,'Q27_pre')
gender=d$Q27_pre[d$Q27_pre<3]
hist(gender,main='Gender Distribution',xlab='gender',breaks=2,labels=c('male','female'))

d=nconv(d,'Q29_pre')
hist(d$Q29_pre,main='Religious Distribution',xlab='religious affiliation')
#######################################
Do demographics differ across groups?
d=fconv(d,'group_assignment')
d=nconv(d,'Q27_pre')

t.test(Q26_pre~group_assignment,d)
t.test(Q27_pre~group_assignment,d)
t.test(Q28_pre~group_assignment,d)
t.test(Q29_pre~group_assignment,d)
# all n.s.
