library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(matrixStats)
library(RColorBrewer)

global<-read.csv('D:/Studies/Research/Dataset - Kaggle/LeagueofLegends.csv',sep=',')
any(is.na(global))
help(geom_histogram)

##Number of games per `League` and `Year`
g1<-global %>% 
  group_by(blueTeamTag) 
View(g1)
g1<-global %>% 
  group_by(League, Season, Year) %>% summarise(number=n()) %>% 
  ggplot(aes(x=Year,y=number,fill=League)) + 
  geom_histogram(stat='identity',position='stack') + 
  scale_fill_manual(values=c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00","#9A8822", "#F5CDB4", "#F8AFA8","#713E5A","#63A375","#EDC79B","#D57A66","#CA6680","#48E5C2","#A1E8CC")) + 
  theme(legend.position='none')  + xlab('') + ylab('number of games')

g2<-global %>% 
  group_by(League) %>% summarise(number=n()) %>% 
  ggplot(aes(x=reorder(League,-number),y=number,fill=League)) + 
  geom_histogram(stat='identity') + 
  scale_fill_manual(values=c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00","#9A8822", "#F5CDB4", "#F8AFA8","#713E5A","#63A375","#EDC79B","#D57A66","#CA6680","#48E5C2","#A1E8CC")) + 
  theme(legend.position='top',axis.text.x = element_text(angle=45, hjust=1),legend.text=element_text(size=8),legend.key.size = unit(.3, "cm"))  + xlab('') + ylab('number of games')

grid.arrange(g1,g2,ncol=2)





##Number of games per Season, breakdown per League, Year
```{r}
global %>% 
  group_by(League,Season,Year) %>% summarise(number=n()) %>% 
  ggplot(aes(x=Year,y=League, size=number,color=Season)) + 
  geom_point() + facet_wrap(~Season) + 
  theme(legend.position="top",legend.text=element_text(size=8),legend.key.size = unit(.4, "cm")) + 
  scale_color_manual(values=c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00","#9A8822", "#F5CDB4", "#F8AFA8"))

# `Winter_Playoffs` and `Winter_Season` occur only for the Brazilian league (`CBLOL`)
# `Spring` and `Summer_Season` are the biggest events so far

##Average length of games
{r}
global %>% 
  dplyr::select(League, Season, gamelength) %>% 
  group_by(League) %>% 
  ggplot(aes(x=gamelength)) + 
  geom_density(aes(fill=League),alpha=.5) + 
  facet_wrap(~League,ncol=4) + 
  theme(legend.position='top') +
  scale_fill_manual(values=c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00","#9A8822", "#F5CDB4", "#F8AFA8")) + xlab('[minutes]') + ylab('')


# The small bump for `Season_World_Championship` is interesting and inindicates that games during this championship (the most prestigious) are more tight.

##Number of wins as Blue/Red teams vs. `Year`
{r}
global %>% 
  dplyr::select(blueTeamTag, bResult,Year) %>% 
  dplyr::filter(bResult==1) %>% 
  dplyr::group_by(blueTeamTag,Year) %>% 
  summarise(number=n()) %>% 
  ggplot(aes(x=blueTeamTag,y=number,fill=number)) + 
  geom_bar(stat='identity',width = 0.75) + 
  theme(legend.position='top',axis.text.x = element_text(angle=45, hjust=1,size=7)) + 
  facet_wrap(~Year,ncol=1) + 
  xlab('') + ylab('') + scale_fill_gradient(name='# of wins as Blue Team',low="#85D4E3", high="#3B9AB2")

global %>% 
  dplyr::select(redTeamTag, rResult,Year) %>% 
  dplyr::filter(rResult==1) %>% 
  dplyr::group_by(redTeamTag,Year) %>% 
  summarise(number=n()) %>% 
  ggplot(aes(x=redTeamTag,y=number,fill=number)) + 
  geom_bar(stat='identity',width = 0.75) + 
  theme(legend.position='top',axis.text.x = element_text(angle=45, hjust=1,size=7)) + 
  facet_wrap(~Year,ncol=1) + 
  xlab('') + ylab('') + scale_fill_gradient(name='# of wins as Red Team',low="#F8AFA8", high="#F21A00")

# The team with the most number of wins as blue and red Team is `SKT`( _SK Telecom T1 is a Korean a professional gaming team based in South Korea_ )

#Details per Match
#Load data files
#{r}
gold<-read.csv('C:/Users/Kevin/Desktop/Sem 4/Research Dataset/LOL/gold.csv',sep=',')
death<-read.csv('C:/Users/Kevin/Desktop/Sem 4/Research Dataset/LOL/kills.csv',sep=',')
objects<-read.csv('C:/Users/Kevin/Desktop/Sem 4/Research Dataset/LOL/objValues.csv',sep=',')
bans<-read.csv('C:/Users/Kevin/Desktop/Sem 4/Research Dataset/LOL/bans.csv',sep=',')

##Example : first game{.tabset .tabset-fade .tabset-pills}

game_id<-1
###Gold difference vs. time played
#From the previous dataset, it looks like the feature `golddfiff` is the difference of gold from the `blue` team and the `red` team : `golddiff = gold_blue - gold_red`

#Quick explanation of the procedure :
  
game1<-data.frame(t(gold[game_id,3:83]))
colnames(game1)<-'gold_diff'
features<-rownames(game1)
rownames(game1)<-1:nrow(game1)
game1$time<-rep(1:nrow(game1))
game1$MatchHistory<-rep(global$MatchHistory[1],nrow(game1))
res$blueTeam<-ifelse(res$bResult==1,"BLUE:W ; RED:L","BLUE:L ; RED:W")

res %>% na.omit() %>% 
  ggplot(aes(x=time,y=gold_diff,shape=blueTeam,fill=ifelse(gold_diff>0,"blue","red"))) + 
  geom_histogram(stat='identity',size=1) + 
  scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + 
  scale_shape_manual(name='Result',values = c(19)) + 
  theme(legend.position='none') + ggtitle(res$blueTeam) + 
  xlab('minutes') + ylab('Gold difference btw Blue and Red team')

###Number of kills vs. time played
#This dataset gives the number of kills from a team to the one so the plot below shows :
  
#   +1 for a kill from the `blue` team to the `red` team
# -1 for a kill from the `red` team to the `blue` team
# I also join this dataset with the `global` one in order to get the length of the game

gameIndex<-global$MatchHistory[game_id]
# death %>% filter(MatchHistory==global$MatchHistory[game_id]) %>% 
#   merge(global %>% select(MatchHistory,League,Season,Year,blueTeamTag,bResult, redTeamTag,rResult,gamelength),by='MatchHistory',y.all=T) %>%
#   ggplot(aes(x=Time,y=ifelse(TeamColor=='Blue',1,-1),fill=ifelse(TeamColor=='Blue','blue','red'))) + 
#   geom_histogram(stat='identity',width=.25,alpha=.5) + 
#   scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + 
#   xlab('minutes') + ylab('Kill occurence') +
#   theme(legend.position='none') + ggtitle(res$blueTeam) + xlim(0,gamelength[1])

resD <- death %>% 
  filter(MatchHistory==global$MatchHistory[game_id]) %>% 
  merge(global %>% select(MatchHistory,League,Season,Year,blueTeamTag,bResult,redTeamTag,rResult,gamelength),by='MatchHistory',y.all=T)

ggplot(data=resD,aes(x=Time,y=ifelse(TeamColor=='Blue',1,-1),fill=ifelse(TeamColor=='Blue','blue','red'))) + 
  geom_histogram(stat='identity',width=.25,alpha=.5) +
  scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) +
  xlab('minutes') + ylab('Kill occurence') +
  theme(legend.position='none') + xlim(0,resD$gamelength[1])

###Neutral monsters kills
#The idea here is to make 2 separate dataframes(`blue` and `red`) and then `rbind` again. It allows to create another label for `blue` and `red` separately
#red
tempR<-as.data.frame(objects %>% filter(MatchHistory==global$MatchHistory[game_id] & (grepl("^r",as.character(ObjType)))) %>% select(-MatchHistory, -ObjType) %>% t())
tempR$Team<-rep('red',nrow(tempR))
colnames(tempR)<-c('Towers', 'Inhibs', 'Dragons' ,'Barons', 'Heralds','Team')
#blue
tempB<-as.data.frame(objects %>% filter(MatchHistory==global$MatchHistory[game_id] & (grepl("^b",as.character(ObjType)))) %>% select(-MatchHistory, -ObjType) %>% t())
tempB$Team<-rep('blue',nrow(tempB))
colnames(tempB)<-c('Towers', 'Inhibs', 'Dragons' ,'Barons', 'Heralds','Team')
#merging
resObjects<-as.data.frame(rbind(tempB,tempR))

#plot
k1<-ggplot(data=resObjects,aes(x=Towers,y=ifelse(Team=='blue',1,-1),fill=ifelse(Team=='blue','blue','red'))) + geom_bar(stat='identity',width=.5,alpha=.5) + scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + xlab('') + ylab('Towers') + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') + xlim(0,resD$gamelength[1])

k2<-ggplot(data=resObjects,aes(x=Inhibs,y=ifelse(Team=='blue',1,-1),fill=ifelse(Team=='blue','blue','red'))) + geom_bar(stat='identity',width=.5,alpha=.5) + scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + xlab('') + ylab('Inhibs') + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') + xlim(0,resD$gamelength[1])

k3<-ggplot(data=resObjects,aes(x=Dragons,y=ifelse(Team=='blue',1,-1),fill=ifelse(Team=='blue','blue','red'))) + geom_bar(stat='identity',width=.5,alpha=.5) + scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + xlab('') + ylab('Dragons') + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') + xlim(0,resD$gamelength[1])

k4<-ggplot(data=resObjects,aes(x=Barons,y=ifelse(Team=='blue',1,-1),fill=ifelse(Team=='blue','blue','red'))) + geom_bar(stat='identity',width=.5,alpha=.5) + scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + xlab('') + ylab('Barons') + theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') + xlim(0,resD$gamelength[1])

k5<-ggplot(data=resObjects,aes(x=Heralds,y=ifelse(Team=='blue',1,-1),fill=ifelse(Team=='blue','blue','red'))) + geom_bar(stat='identity',width=.5,alpha=.5) + scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + xlab('minutes') + ylab('Heralds') + theme(axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') + xlim(0,resD$gamelength[1])

grid.arrange(k1,k2,k3,k4,k5,ncol=1)


##Putting all together
g1<-res %>% na.omit() %>% 
  ggplot(aes(x=time,y=gold_diff,shape=blueTeam,fill=ifelse(gold_diff>0,"blue","red"))) + 
  geom_histogram(stat='identity',size=1) + 
  scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + 
  scale_shape_manual(name='Result',values = c(19)) + ggtitle(res$blueTeam) + 
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') +  ylab('Gold diff.') + xlim(0,res$gamelength[1])

g2<-ggplot(data=resD,aes(x=Time,y=ifelse(TeamColor=='Blue',1,-1),fill=ifelse(TeamColor=='Blue','blue','red'))) + 
  geom_histogram(stat='identity',width=.25,alpha=.5) +
  scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) +
  xlab('minutes') + ylab('Kills') +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y = element_text(size=8),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position='none') + xlim(0,resD$gamelength[1])

grid.arrange(g1,g2,k1,k2,k3,k4,k5,ncol=1)

#Gold Values vs. Time played

# disentangle winner/loser and get its `global` data
# merge the result with the `goldValues` dataset  by `MatchHistory` and select the proper feature 
# calculate the `mean`, `max` and `min` over time
# plot with errors bars
# breakdown all by `Year`

getGold<-function(x){
  #select blue as winners
  bW<-merge( 
    (global %>% filter(bResult==1 & Year==x) %>% select(MatchHistory,gamelength,Season,Year,blueTeamTag,redTeamTag)),
    (gold %>% filter(NameType=='goldblue')) , 
    by='MatchHistory')
  
  #select red as winners
  rW<-merge( 
    (global %>% filter(rResult==1 & Year==x) %>% select(MatchHistory,gamelength,Season,Year,blueTeamTag,redTeamTag)),
    (gold %>% filter(NameType=='goldred')) , 
    by='MatchHistory')
  
  #merge datasets 
  allWins<-data.frame(rbind(rW,bW))
  #str(allWins)
  #head(allWins)
  games_w<-data.frame(t(allWins[,8:83]))
  
  #calculate mean, max and min over all games
  meanWG <- (games_w %>% select_if(is.numeric) %>% mutate(mean = rowMeans(.,na.rm=T)))$mean
  minWG<-rowMins(as.matrix(games_w),na.rm=T)
  maxWG<-rowMaxs(as.matrix(games_w),na.rm=T)
  
  #select blue as losers
  bL<-merge( 
    (global %>% filter(bResult==0 & Year==x) %>% select(MatchHistory,gamelength,Season,Year,blueTeamTag,redTeamTag)),
    (gold %>% filter(NameType=='goldblue')) , 
    by='MatchHistory')
  rL<-merge( 
    (global %>% filter(rResult==0 & Year==x) %>% select(MatchHistory,gamelength,Season,Year,blueTeamTag,redTeamTag)),
    (gold %>% filter(NameType=='goldred')) , 
    by='MatchHistory')
  
  
  allLoses<-data.frame(rbind(rL,bL))
  games_l<-data.frame(t(allLoses[,8:83]))
  meanLG <- (games_l %>% select_if(is.numeric) %>% mutate(mean = rowMeans(.,na.rm=T)))$mean
  minLG<-rowMins(as.matrix(games_l),na.rm=T)
  maxLG<-rowMaxs(as.matrix(games_l),na.rm=T)
  
  #merge all
  res<-as.data.frame(cbind(meanWG, minWG, maxWG, meanLG, minLG, maxLG))
  
  res$time<-rep(1:nrow(res))
  res$year<-rep(x,nrow(res))
  #head(res)
  return(res)
}

gold_2014<-getGold(2014)
gold_2015<-getGold(2015)
gold_2016<-getGold(2016)
gold_2017<-getGold(2017)
res<-as.data.frame(rbind(gold_2014,gold_2015,gold_2016,gold_2017))

##Mean gold vs. `Year`
#One important thing to notice is that the mean gold value (vs. `Time played`) differs from `Year` (games rules changed), therefore averaging the mean gold over all games is not correct

h1<-ggplot() + geom_point(data=res,aes(x=time,y=meanWG,color=factor(year))) +
  scale_color_manual(values=c("#78B7C5", "#EBCC2A", "#F8AFA8", "#F21A00")) + 
  xlab('minutes') + ylab('mean gold winners') + theme(legend.position='top')

h2<-ggplot() + geom_point(data=res,aes(x=time,y=meanLG,color=factor(year))) +
  scale_color_manual(values=c("#78B7C5", "#EBCC2A", "#F8AFA8", "#F21A00")) + 
  xlab('minutes') + ylab('mean gold losers') + theme(legend.position='top')
grid.arrange(h1,h2,ncol=2)

#We see that the average gold vlaue is similar for `2016` and `2017`, and for `2014` and `2015`.

##Mean gold vs. `Year`, breakdown by win/loss 

ggplot() + geom_point(data=res,aes(x=time,y=meanWG,color="win")) + geom_point(data=res,aes(x=time,y=meanLG,color="loss"))  +
  scale_color_manual(name="",values=c(win="#3B9AB2",loss="#F21A00")) + 
  xlab('minutes') + ylab('Mean gold acquired') + theme(legend.position='top') + facet_grid(~factor(res$year))
  
#Now we see a behavior (common for all years) that is the winning team got more gold (around) mid game than the losing teams. This could be a feature to take into account when predicting if a team will win/lose.

###Mean gold vs. `Year`, breakdown by win/loss, with errors bars 
list_plot<-list()
cnt<-1
for(i in 2014:2017){
  list_plot[[cnt]] <- ggplot() + 
    geom_errorbar(data=filter(res,year==i),aes(x=time,y=meanWG,ymin=minWG, ymax=maxWG),alpha=.5) + 
    geom_line() + geom_point(data=filter(res,year==i),aes(x=time,y=meanWG,color='win')) +
    scale_color_manual(name="",values=c(win="#3B9AB2")) + ggtitle(paste0('wins ; ',i)) +
    theme(legend.position='none') + ylab('')
  
  list_plot[[cnt+4]] <- ggplot() + 
    geom_errorbar(data=filter(res,year==i),aes(x=time,y=meanLG,ymin=minLG, ymax=maxLG),alpha=.5) + 
    geom_line() + geom_point(data=filter(res,year==i),aes(x=time,y=meanLG,color='loss')) +
    scale_color_manual(name="",values=c(loss="#F21A00")) + ggtitle(paste0('losses ;',i)) +
    theme(legend.position='none') + ylab('')
  
  cnt<-cnt + 1
}
```

```{r}
grid.arrange(list_plot[[1]],list_plot[[2]],list_plot[[3]],list_plot[[4]],
             list_plot[[5]],list_plot[[6]],list_plot[[7]],list_plot[[8]],ncol=4)
```

<hr>
  
  #`Bans` analysis
  
  Definition of `banning` in LOL, [from gaming.stackexchange](https://gaming.stackexchange.com/questions/28259/why-are-characters-banned-in-tournament-play-for-dota-likes)
: "The origins of banning heroes comes from a match type called -CM, or Captain's Mode in DoTA. In Captain's mode, teams take turns banning heroes they don't want the other team to use whilst picking their own heroes for the match. One person from each team (the person in the first slot) is the one who makes these decisions, so you usually only play CM with a group of people you're familiar with since it requires a lot of coordination. In professional level play, this becomes extremely important as you want to prevent the other team from picking heroes that they are adept at, or countering the heroes you picked."

There are also plans from [RIOT](http://na.leagueoflegends.com/en/news/esports/esports-editorial/spring-split-10-bans-pro-play) to increase the bans from 5 (actually) to 10.

As the number of `bans` has changed through years, the first thing to look is how it has changed.

```{r}
colfuncBlue <- colorRampPalette(c("black", "#3B9AB2"))
colfuncRed <- colorRampPalette(c("black", "#F21A00"))
colsBlue=colfuncBlue(4)
colsRed=colfuncRed(4)
bans[bans==""] <- NA
#calculate the number of possible bans and group them with global data
bans$numBans<-rowSums(!is.na(bans[-(1:2)]))
redBans<- data.frame(merge(global %>% select(MatchHistory,League,Season,Year,redTeamTag,rResult,gamelength), bans %>% filter(TeamColor=='Red') %>% select(MatchHistory,numBans), by='MatchHistory'))
#define a tag from win/loss
redBans$RES<-ifelse(redBans$rResult==0,'LOSS','WIN')

blueBans<- data.frame(merge(global %>% select(MatchHistory,League,Season,Year,blueTeamTag,bResult,gamelength), bans %>% filter(TeamColor=='Blue') %>% select(MatchHistory,numBans), by='MatchHistory'))
blueBans$RES<-ifelse(blueBans$bResult==0,'LOSS','WIN')

rBans<-ggplot(data=redBans,aes(x=numBans,fill=factor(Year))) + geom_bar(position='dodge',width=.5) + scale_y_log10() + facet_grid(~factor(RES)) + scale_fill_manual(name="",values=colsRed) + xlab('number of bans') + theme(legend.position="right",axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
bBans<-ggplot(data=blueBans,aes(x=numBans,fill=factor(Year))) + geom_bar(position='dodge',width=.5) + scale_y_log10() + facet_grid(~factor(RES)) + scale_fill_manual(name="",values=colsBlue) + xlab('number of bans') + theme(legend.position="right")

grid.arrange(rBans,bBans,ncol=1)
```

As we see, the number of bans has incresed in 2017 from 3 to 5.
