library(rtweet)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(ggthemes)
library(maps)
library(SentimentAnalysis)
library(textcat)
library(jsonlite)
library(extrafont)

create_token(
  app = "SurveyMethodMichigan",
  consumer_key = "iaevFnr5HtcB3WIp2oVJUDDAN",
  consumer_secret = "ILzWQZ96dItZJTIRWSoLIaLeffU2DVKpUawdZiN2znJw73X8WM",
  access_token = "761393280059793408-gsY8vYhXpBHlGrExZYyon8lFApszM7U",
  access_secret = "EP8nzJ93K8aIAhmAt37sKCb6wjMkdGPkwbEOnt5Jc1wxd"
)

get_token()

Trump <- search_tweets('to:@realDonaldTrump',geocode = lookup_coords("usa"),n=3000)

Trump <- fromJSON(txt = "Trump.json")
Trump$created_at <- as.POSIXct(Trump$created_at)
Trump%>%
  group_by(reply_to_status_id)%>%
  summarise(total=n())%>%
  arrange(desc(total))


Trump%>%
  filter(reply_to_status_id=='1198948075928268800')%>%
  ggplot(aes(x=created_at))+
  geom_histogram(color='#ffc280',fill='#ffe180')
  
first_mostly_reply <- Trump%>%
  filter(reply_to_status_id=='1198948075928268800')

second_mostly_reply <- filter(Trump,reply_to_status_id=='1198981079723589639')


tidy_Trump<-Trump%>%
  unnest_tokens(word,text)

tidy_Trump%>%
  count(word,sort=TRUE)

positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

tidy_Trump %>%
  semi_join(positive) %>%
  count(word, sort = TRUE)

bing <- get_sentiments("bing")

bing_word_counts <- tidy_Trump %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE)


bing_word_counts %>%
  filter(n > 1500&word!='trump'&word!='like') %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(title = 'Common Sentiment words',y = "Contribution to sentiment",y='Word')+
  theme(plot.title = element_text(size=20, 
                                  face="bold", 
                                  family="Cambria",
                                  color="tomato",
                                  hjust=0.5,
                                  lineheight=1.2),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))





#First mostly reply tweet:
#Support for Impeachment is dropping like a rock, down into the 20¡¯s in some Polls. Dems should now get down to work and finally approve USMCA, and much more!
usableText<-gsub("[^\x01-\x7F]", "", first_mostly_reply$text) 

first_mostly_reply %>%
  group_by(created_at)%>%
  arrange(created_at)
sentiment <- analyzeSentiment(usableText)
sentiment <- cbind(sentiment,first_mostly_reply$created_at)
names(sentiment)[15]<-'time'


ggplot(sentiment,aes(x=SentimentGI))+
  geom_histogram(color='darkgrey',fill='#ae6bff')+
  theme_bw()+
  labs(title="Distribution of Retweets Sentiment",
       y="Count", 
       x="Sentiment", 
       caption="Source: Twitter API")+
  theme(plot.title = element_text(size=20, 
                                  family = 'Cambria',
                                  face='bold',
                                  color = 'tomato',
                                  hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))


sentiment%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()+
  labs(title="Sentiment Trend",
       subtitle = 'Political Statement | Overall',
       y="Sentiment", 
       x="Time", 
       caption="Source: Twitter API")+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=7, 
                                   face='bold',
                                   family = 'Cambria',
                                   color = 'tomato',
                                   hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))



sentiment%>%
  filter(SentimentGI>0)%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()+
  labs(title="Sentiment Trend",
       subtitle = 'Political Statement | Positive: SentimentGI > 0',
       y="Sentiment", 
       x="Time", 
       caption="Source: Twitter API")+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=7, 
                                   face='bold',
                                   family = 'Cambria',
                                   color = 'tomato',
                                   hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))


sentiment%>%
  filter(SentimentGI<0)%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()+
  labs(title="Sentiment Trend",
       subtitle = 'Political Statement | Negative: SentimentGI<0',
       y="Sentiment", 
       x="Time", 
       caption="Source: Twitter API")+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
  plot.subtitle=element_text(size=7, 
                             face='bold',
                             family = 'Cambria',
                             color = 'tomato',
                             hjust=0.5),
  plot.caption = element_text(family = 'Cambria',
                              face = 'bold'),
  axis.title.x = element_text(family = 'Cambria',
                              face = 'bold'),
  axis.title.y = element_text(family = 'Cambria',
                              face = 'bold'))  # subtitle

sentiment%>%
  ggplot(aes(x=time))+
  geom_histogram(color='#ffc280',fill='#ffe180')

sentiment%>%
  filter(SentimentGI>0)%>%
  group_by(time)%>%
  summarise(n=n())%>%
  ggplot(aes(x=time))+
  geom_histogram(color='#ffc280',fill='#ffe180')

sentiment%>%
  filter(SentimentGI<0)%>%
  group_by(time)%>%
  summarise(n=n())%>%
  ggplot(aes(x=time))+
  geom_histogram(color='#dc4eff',fill="orange")+
  theme_bw()

#Second mostly reply tweet:
#Another new Stock Market Record. Enjoy!
second_usableText<-gsub("[^\x01-\x7F]", "", second_mostly_reply$text) 

second_mostly_reply %>%
  group_by(created_at)%>%
  arrange(created_at)
sentiment_second <- analyzeSentiment(second_usableText)
sentiment_second <- cbind(sentiment_second,second_mostly_reply$created_at)
names(sentiment_second)[15]<-'time'
sentiment_second$time<-as.POSIXct(sentiment_second$time1)
ggplot(sentiment_second,aes(x=SentimentGI))+
  geom_histogram(color='#ffc280',fill='#ffe180')

sentiment_second%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()


sentiment_second%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()+
  labs(title="Sentiment Trend",
       subtitle = 'Fact Statement | Overall',
       y="Sentiment", 
       x="Time", 
       caption="Source: Twitter API")+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=7, 
                                   face='bold',
                                   family = 'Cambria',
                                   color = 'tomato',
                                   hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))


sentiment_second%>%
  filter(SentimentGI<0)%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()+
  labs(title="Sentiment Trend",
       subtitle = 'Fact Statement | Negative: SentimentGI < 0',
       y="Sentiment", 
       x="Time", 
       caption="Source: Twitter API")+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=7, 
                                   face='bold',
                                   family = 'Cambria',
                                   color = 'tomato',
                                   hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))

sentiment_second%>%
  filter(SentimentGI>0)%>%
  ggplot(aes(x=time,y=SentimentGI))+
  geom_smooth(color='#dc4eff',fill='orange')+   
  theme_bw()+
  labs(title="Sentiment Trend",
       subtitle = 'Fact Statement | Positive: SentimentGI > 0',
       y="Sentiment", 
       x="Time", 
       caption="Source: Twitter API")+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=7, 
                                   face='bold',
                                   family = 'Cambria',
                                   color = 'tomato',
                                   hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))


sentiment_second%>%
  filter(SentimentGI==0)%>%
  group_by(time)%>%
  summarise(n=n())%>%
  ggplot(aes(x=time))+
  geom_histogram(stat = 'count')

sentiment_second%>%
  filter(SentimentGI>0)%>%
  group_by(time)%>%
  summarise(n=n())%>%
  ggplot(aes(x=time))+
  geom_histogram(color='#ffc280',fill='#ffe180')

windows()
sentiment_second%>%
  filter()%>%
  group_by(time)%>%
  summarise(n=n())%>%
  ggplot(aes(x=time))+
  geom_histogram(bins=50,color='darkgrey',fill='#ae6bff')+
  labs(title = "Amount of Retweets over Time",
       x='Time',
       y='Number of retweets',
       caption = 'Source: Twitter API')+
  theme(plot.title=element_text(size=20, 
                                face="bold",
                                family = 'Cambria',
                                color = 'tomato',
                                hjust=0.5,
                                lineheight=1.2),  # title
        plot.subtitle=element_text(size=7, 
                                   face='bold',
                                   family = 'Cambria',
                                   color = 'tomato',
                                   hjust=0.5),
        plot.caption = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.x = element_text(family = 'Cambria',
                                    face = 'bold'),
        axis.title.y = element_text(family = 'Cambria',
                                    face = 'bold'))

fit <- arima(sentiment$SentimentGI)
fit

fit_second <- arima(sentiment_second$SentimentGI)
fit_second

