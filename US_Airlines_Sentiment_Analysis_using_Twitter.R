library(ggplot2) # Data visualization

Tweets<-read_csv('../input/Tweets.csv')
head(Tweets)

#Count of tweets for each sentiment
Sentiment_Table <- table(Tweets$airline_sentiment)
Sentiment_Table
Sentiment_table_prop <- prop.table(table(Tweets$airline_sentiment))
Sentiment_table_prop

#bar graph for counts

barplot(Sentiment_Table,col=c("coral1","coral2","coral3"))
barplot(Sentiment_table_prop,col=c("red","blue","green"))

barplot(table(Tweets$airline))

#Proportion of negative tweets per airline

table1 <- subset(Tweets,airline=="Virgin America")
table2<-table(table1$airline_sentiment)
table2
barplot(prop.table(table2))

# Bar plot based on date

Tweets$tweet_created1 <- format(as.Date(Tweets$tweet_created,format="%Y-%m-%d"), "%m/%d/%Y")


table_date <- table(Tweets$airline_sentiment, Tweets$tweet_created1)
barplot(table_date,col=c("red","yellow","blue"),legend=rownames(table_date))

#Proportion of negative tweets per airline

airline_table <- prop.table(table(Tweets$airline_sentiment,Tweets$airline))
airline_table
barplot(airline_table,col=c("red","yellow","blue"), legend=rownames(airline_table))


#Reasons for negative sentiments

NegTweetReason = as.data.frame(prop.table(table(Tweets$negativereason)))
colnames(NegTweetReason) = c('Reason', 'Frequency')
NegTweetReason = NegTweetReason[-1, ]
NegTweetReason

g = ggplot(NegTweetReason, aes(x = Reason, y = Frequency)) + geom_bar(stat = 'identity', fill = 'pink')
g = g + ggtitle('Reasons for Negative Sentiment')
g = g + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -0.1),
              axis.text.x = element_text(angle = 30, size = 10, vjust = 1))