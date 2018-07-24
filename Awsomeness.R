library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

global<-read.csv('D:/Studies/Research/Dataset - Kaggle/bans2.csv',sep=',')
any(is.na(global))
global=data.frame(global)


nrow(global)

# Separating the columns

ban1=unlist(global$ban_1)
ban2=unlist(global$ban_2)
ban3=unlist(global$ban_3)
ban4=unlist(global$ban_4)
ban5=unlist(global$ban_5)

all

allBan=paste(ban1,ban2,ban3,ban4,ban5,collapse =" ")

allBan=data.frame(allBan)

words<-strsplit(allBan," ")
#Calculate word frequencies
wordfreq<-table(unlist(words));
View(wordfreq)

wordfreq=data.frame(wordfreq)
View(wordfreq)
nrow(wordfreq)
wordfreq=wordfreq[2:137,]
View(wordfreq)
res=wordfreq[wordfreq$Freq >1000,]

View(res)

plot(wordfreq$Var1,wordfreq$Freq)
ggplot(wordfreq)
 
barplot(table(unlist(words)))
barplot(res$Freq,names.arg = res$Var1,las=2,ylim=c(0,2500)   )

help(barplot)
global.Corpus=Corpus(VectorSource(allBan))


global.Clean<-tm_map(global.Corpus, PlainTextDocument)
global.Clean<-tm_map(global.Corpus,tolower)
global.Clean<-tm_map(global.Clean,removeNumbers)
global.Clean<-tm_map(global.Clean,removeWords,stopwords("english"))
global.Clean<-tm_map(global.Clean,removePunctuation)
global.Clean<-tm_map(global.Clean,stripWhitespace)
global.Clean<-tm_map(global.Clean,stemDocument)                     
                                
wordcloud(global.Clean,max.words = 300,random.color = TRUE,random.order=FALSE)



#new df with bans and picks

df1<-read.csv('D:/Studies/Research/Dataset - Kaggle/bans2.csv',sep=',')
df2<-read.csv('D:/Studies/Research/Dataset - Kaggle/matchinfo.csv',sep=',')
View(df1)
df1=subset(df1,select=-c(Team,ban_2))
df1
df3<-merge(df1[0:5],df2,by.x="Address",by.y="Address")


uniqueChamps=read.csv('D:/Studies/Research/myMine/Chapions.csv')
uniqueChamps=as.data.frame(uniqueChamps)
View(uniqueChamps)
uniqueChamps=subset(uniqueChamps, select = c(uniqueChamps$data__.__name,uniqueChamps$))

View(df3)
                     