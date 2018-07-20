global<-read.csv('C:/Users/Kevin/Desktop/Sem 4/Research Dataset/LOL/monsters.csv',sep=',')
any(is.na(global))
View(global)
ed=global[,"rDragon"]
ed=as.data.frame(global)

ed=data.table(ed)
a=c()
j=1;
rw=rownames(global)
rw
n
global[,2]

where
rdragon=global[which(global$Team=="rDragons"),]



str(monsters)

View(rdragon)


fTeam=as.factor(global$Team)

View(fTeam)

global


for(i in i:rw)
{
  if(global[,2]=="rDragon")
  print(rw)
}

n
a=c(1:44248)
i=0
for (i in 1:44248)
{
  a[i]=1
}
a
help(cbind)
cbind(global,a)
ed[ed[],]




global<-read.csv('C:/Users/Kevin/Desktop/Sem 4/Research Dataset/LOL/bans.csv',sep=',')
any(is.na(global))
View(global)
ed=as.data.frame(global)

View(ed)
tbl <- with(ed, table(ban_1))
View(tbl)

tbl1=data.frame(tbl)

#my stuff

#my stuff



tbl1$freq


nrow(tbl1$ban1)
tbl1$Freq
View(tbl1$Freq>90)

tbl2=subset(tbl1,tbl1$Freq>=90)
count(tbl2)
tbl3=data.frame(tbl2)

barplot(tbl3$ban_1)


View(tbl2)

library(plyr)
count(tbl,tbl$'ban_1')

barplot(tbl3, beside = TRUE, legend = TRUE)


View(barplot(tbl, beside = TRUE, legend = TRUE,horiz = TRUE))
