library(twitteR)
superbowl = searchTwitter('commercial', lang='en', n = 10000, since='2015-02-01', until = '2015-02-02', retryOnRateLimit=120)
sb1 = sapply(superbowl, function(x) x$getText())
sb2 = sapply(superbowl, function(x) x$getRetweetCount())
sb3 = sapply(superbowl, function(x) x$getFavorited())
super = cbind(sb1,sb2,sb3)
colnames(super) = c('Text','Retweets','Favorited')
write.csv(super, file='super.csv')
super = read.csv('super.csv', header=T)

## scrape website to get sponsor names
library(rvest)
adsponsors = html('http://adage.com/article/special-report-super-bowl/super-bowl-xlix-ad-chart-buying-big-game-commercials/295841/')
sponsors = adsponsors %>% html_nodes('.sb-header') %>% html_text()
z = strsplit(sponsors, split=' ')
z = lapply(z, function(x) x[1])
z = gsub('[[:punct:]]', '', z)
z[c(1,3,25,41:43)] = c('LikeAGirl','Bud', 'No More','Jurrasic','Carl','Terminator')

#How many tweets about each company's ad
source('clean_text.r')
library(ggplot2)
library(plyr)
library(dplyr)
tweetText = clean_text(super$Text)
a = sort(sapply(z, function(x) sum(grepl(x, tweetText, ignore.case=T))), decreasing=T)
a = a[!a==0]
a = data.frame(AllTweets = a, Brand = names(a))
a$UniqueTweets = sapply(a$Brand, function(x) sum(grepl(x, unique(tweetText), ignore.case=T)))
a$Brand <- factor(a$Brand, levels = a$Brand, ordered = TRUE)
library(reshape2)
a_melt = melt(a, id='Brand')
write.csv(a_melt, 'a_melt.csv')

ggplot(a_melt, aes(x=Brand, y = value, group=variable, fill=variable))+theme_bw()+xlab('')+ylab('Count')+
  geom_bar(stat='identity', position=position_dodge())+labs(title='Number of Tweets mentioning Brand Name')+
  scale_fill_discrete(name = 'Tweets and \nunique tweets')+theme(axis.text.x = element_text(angle=90))

a = a[a$AllTweets>100,]

#Sentiment Analysis
pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')
source('score.sentiment.R')

p = sapply(a$Brand, function(x) tweetText[grep(x, tweetText, ignore.case=T)])
j = lapply(p, function(x) score.sentiment(x, pos.words, neg.words))
a$Sentiment = sapply(1:length(j), function(x) sum(j[[x]]$score/nrow(j[[x]])))


fillcolor <- ifelse(a$Sentiment > 0, 'steelblue', 'red4')
ggplot(a, aes(x=Brand, y=Sentiment)) + geom_bar(stat='identity', fill=fillcolor)+theme_bw()+xlab('')+
  ylab('Mean Ad Sentiment')+labs(title='Twitter Response to Superbowl Ads')+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#reach!
a$Reach = sapply(a$Brand, function(x) sum(unique(super[grepl(x, super$Text, ignore.case=T),'Retweets'])))
a$maxReach = sapply(a$Brand, function(x) max(unique(super[grepl(x, super$Text, ignore.case=T),'Retweets'])))

ggplot(a, aes(x=Brand, y=log(Reach), fill = maxReach/Reach)) + 
  geom_bar(stat='identity', position= position_stack())+theme_bw()+xlab('')+
  ylab('log10(Total Number of Retweets)')+labs(title='Reach of Superbowl Ad Related Tweets')+
  theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_continuous(name='Message \nCoherence', low='yellow', high='red')

write.csv(a, 'a.csv')
#links
links = sapply(a$Brand, function(x) str_extract(super$Text[grep(x, super$Text, ignore.case=T)], "http[[:print:]]{18}"))
library(SocialMediaMineR)
get_url(a$Links)
#starpower
