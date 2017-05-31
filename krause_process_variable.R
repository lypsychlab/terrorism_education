krause_process_variable=function(d,pat,title,q) {
post=grep(pat,colnames(d))
d0=d[,post]
d0=cbind(d0,d$group_assignment,d$medium_assignment)
colnames(d0)=c('post','group','medium')

a=aggregate(post~group*medium,d0,mean)
g=ggplot(a,aes(x=factor(group),y=post,fill=factor(medium)))+geom_bar(stat='identity',position='dodge')+ggtitle(title)+xlab('Control vs Treatment')+scale_x_discrete(labels=c('0'='Control','1'='Treatment'))+ylab(q)+ylim(0,7)+scale_fill_discrete(name='Medium',labels=c('Text','Audio'))

d0=nconv(d0,'post')
d0=fconv(d0,c('group','medium'))
d0=as.data.frame(d0)
m=lm(post~group*medium,d0)
print(summary(m))
return(g)
}

krause_process_demographic=function(d,colname,title,xtitle) {
	d=nconv(d,colname)
	c=grep(colname,colnames(d))
	hist(d[,c],main=title,xlab=xtitle)
}