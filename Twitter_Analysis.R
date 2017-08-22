# https://www.r-bloggers.com/building-wordclouds-in-r/
# https://www.r-bloggers.com/analyzing-the-us-election-using-twitter-and-meta-data-in-r/
# http://tidytextmining.com/sentiment.html
# https://github.com/SMAPPNYU/smappR/#a-installing-r-packages
# https://stackoverflow.com/questions/31266528/twitter-r-package-how-to-get-as-many-tweets-as-possible-per-account-within-api

library(twitteR)
library(ggplot2)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(lubridate)
library(SnowballC)
library(wordcloud)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(streamR)
library(Rfacebook)
library(smappR)

# install.packages("~/Downloads/rmongodb_1.8.0.tar", repos = NULL, type="source")

# doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
# toInstall <- c("ROAuth", "igraph", "ggplot2", "wordcloud", "devtools", "tm",
    # "R2WinBUGS", "rmongodb", "scales")
# if(doInstall){
    # install.packages(toInstall, repos = "http://cran.r-project.org")
    # library(devtools)
    # # R packages to get twitter and Facebook data
    
    # install_github("streamR", "pablobarbera", subdir="streamR")
    # install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
    # # smapp R package
    # install_github("smappR", "SMAPPNYU")
# }

# library(ROAuth)
# requestURL <- "https://api.twitter.com/oauth/request_token"
# accessURL <- "https://api.twitter.com/oauth/access_token"
# authURL <- "https://api.twitter.com/oauth/authorize"
# consumerKey <- "d4Dbde7PItbezEY7e3ClUkoRz"
# consumerSecret <- "Z7vJvgp7LTJsqzR0lom17VkTziCP6tnHrEkYMK2CRa85GjAebw"
# my_oauth <- OAuthFactory$new(consumerKey=consumerKey, consumerSecret=consumerSecret, 
    # requestURL=requestURL, accessURL=accessURL, authURL=authURL)
# my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
# setwd("~/")
# dir.create("my_oauth")
# save(my_oauth, file="my_oauth/my_oauth")

get_twitter_sentiments <- function(account_name){
	
	file.remove("~/twitterdata")
	getTimeline("~/twitterdata",
		id = account_name,
		verbose = T,
		oauth_folder = "~/my_oauth")
	
	input_tweets <- parseTweets("~/twitterdata")
	 
	#Put the tweets downloaded into a data.frame
	#input_tweets <- twListToDF(input_tweets)
	
	# https://stackoverflow.com/questions/15748190/emoticons-in-twitter-sentiment-analysis-in-r
	input_tweets$text <- sapply(input_tweets$text,function(row){
		iconv(row, "latin1", "ASCII", sub="")
	})
	
	input_tweets$text_original <- input_tweets$text
	
	input_tweets$text <- gsub("#","_hashtag_",input_tweets$text)
	input_tweets$text <- gsub("\\\"","",input_tweets$text)
	input_tweets$text <- gsub("\\.|,|;|:|!|\\(|\\)|-","",input_tweets$text)
	input_tweets$text <- gsub("\n"," ",input_tweets$text)
	input_tweets$text <- gsub("/"," ",input_tweets$text)
	input_tweets$text <- tolower(input_tweets$text)
	input_tweets$text <- gsub("https//","",input_tweets$text)
	input_tweets$text <- gsub("https","",input_tweets$text)
	input_tweets$text <- gsub("&amp","",input_tweets$text)
	
	input_tweets$day <- strptime(x = input_tweets$created, format = "%a %b %d %H:%M:%S %z %Y")
	input_tweets$day <- strftime(input_tweets$day, format = "%d:%m:%Y")
	
	input_tweets$hour <- strptime(x = input_tweets$created, format = "%a %b %d %H:%M:%S %z %Y")
	input_tweets$hour <- strftime(input_tweets$hour, format = "%d:%m:%Y-%H")
	
	input_tweets$minute <- strptime(x = input_tweets$created, format = "%a %b %d %H:%M:%S %z %Y")
	input_tweets$minute <- strftime(input_tweets$minute, format = "%d:%m:%Y-%H:%M")
	
	input_tweets$year_month <- strptime(x = input_tweets$created, format = "%a %b %d %H:%M:%S %z %Y")
	input_tweets$year_month <- strftime(input_tweets$year_month, format = "%b %Y")
	
	input_tweets <- input_tweets[,c("text","text_original","created_at","day","hour","minute")]
	
	tidy_tweet <- input_tweets %>%
		group_by(day) %>%
		mutate(linenumber = row_number()) %>%
		ungroup() %>%
		unnest_tokens(word, text)
	
	twittersentiment_day <- tidy_tweet %>%
	  inner_join(get_sentiments("bing")) %>%
	  count(day, sentiment) %>%
	  spread(sentiment, n, fill = 0) %>%
	  mutate(sentiment_day = positive - negative)
	twittersentiment_day_df <- as.data.frame(twittersentiment_day)
	twittersentiment_day_df$day_date <-  as.Date(twittersentiment_day_df$day, format = "%d:%m:%Y")
	twittersentiment_day_df <- twittersentiment_day_df[order(twittersentiment_day_df$day_date),]
	
	twittersentiment_hour <- tidy_tweet %>%
	  inner_join(get_sentiments("bing")) %>%
	  count(hour, sentiment) %>%
	  spread(sentiment, n, fill = 0) %>%
	  mutate(sentiment_hour = positive - negative)
	twittersentiment_hour_df <- as.data.frame(twittersentiment_hour)
	twittersentiment_hour_df$hour_data <-  as.Date(twittersentiment_hour_df$hour, format = "%d:%m:%Y-%H")
	twittersentiment_hour_df <- twittersentiment_hour_df[order(twittersentiment_hour_df$hour_data),]
	
	twittersentiment_minute <- tidy_tweet %>%
	  inner_join(get_sentiments("bing")) %>%
	  count(minute, sentiment) %>%
	  spread(sentiment, n, fill = 0) %>%
	  mutate(sentiment_minute = positive - negative)
	twittersentiment_minute_df <- as.data.frame(twittersentiment_minute)
	twittersentiment_minute_df$minute_data <-  as.Date(twittersentiment_minute_df$minute, format = "%d:%m:%Y-%H")
	twittersentiment_minute_df <- twittersentiment_minute_df[order(twittersentiment_minute_df$minute_data),]
	
	input_tweets_sentiment <- merge(x = input_tweets, y = twittersentiment_day_df,
		by = "day")
	input_tweets_sentiment <- merge(x = input_tweets_sentiment, y = twittersentiment_hour_df,
		by = "hour")
	input_tweets_sentiment <- merge(x = input_tweets_sentiment, y = twittersentiment_minute_df,
		by = "minute")
	input_tweets_sentiment <- as.data.frame(input_tweets_sentiment)
	
	range(input_tweets_sentiment$sentiment_day)
	
	input_tweets_sentiment[input_tweets_sentiment$sentiment_day > 60,"text_original"]
	input_tweets_sentiment[input_tweets_sentiment$sentiment_day < (-5),"text_original"]
	
	input_tweets_sentiment[input_tweets_sentiment$sentiment_hour > 10,"text_original"]
	input_tweets_sentiment[input_tweets_sentiment$sentiment_hour < (-10),"text_original"]
	
	input_tweets_sentiment$created_at_num <- as.numeric(input_tweets_sentiment$day_date)
	range_date <- range(input_tweets_sentiment$created_at_num)
	all_dates <- c(range_date[1]:range_date[2])
	
	as.Date(all_dates, origin = "1970-01-01")
	
	input_tweets_sentiment <- input_tweets_sentiment[order(input_tweets_sentiment$created_at_num),]
	
	quartz(width = 10)
	plot(rep(0,length(all_dates)) ~ all_dates,
		data = input_tweets_sentiment, pch = 20, cex = 0.1,
		las = 1,
		bty = "n",
		xlab = "",
		ylab = "Sentiment score",
		type = "n",
		ylim = range(input_tweets_sentiment[,c("sentiment_day","sentiment_hour","sentiment_minute")]),
		xaxt = "n")
	
	axis(1, at = round(seq(range_date[1], range_date[2], length.out = 10)),
		labels = F)
	text(x = round(seq(range_date[1], range_date[2], length.out = 10)), 
		y = par("usr")[3] * 1.3, 
		labels = strftime(
			as.Date(all_dates[round(seq(1, length(all_dates), length.out = 10))],
				origin = "1970-01-01"),
			format = "%b %Y"),
		srt = 45,
		pos = 1,
		xpd = TRUE)
	
	points(sentiment_minute ~ created_at_num,
		data = input_tweets_sentiment, pch = 20, cex = 0.1,
		col = "blue")
	lines(x = input_tweets_sentiment$created_at_num,
		y = input_tweets_sentiment$sentiment_minute,
		lwd = 0.5,
		col = "lightblue")
	
	points(sentiment_hour ~ created_at_num,
		data = input_tweets_sentiment, pch = 20, cex = 0.1,
		col = "red")
	lines(x = input_tweets_sentiment$created_at_num,
		y = input_tweets_sentiment$sentiment_hour,
		lwd = 0.5,
		col = "lightpink")
	
	points(sentiment_day ~ created_at_num,
		data = input_tweets_sentiment, pch = 20, cex = 0.1,
		las = 1)
	lines(x = input_tweets_sentiment$created_at_num,
		y = input_tweets_sentiment$sentiment_day,
		lwd = 0.5,
		col = "lightgrey")
	
	j <- order(input_tweets_sentiment$created_at_num)
	
	lw3 <- loess(sentiment_minute ~ created_at_num,
		data = input_tweets_sentiment,
		span = 0.1)
	lines(input_tweets_sentiment$created_at_num[j],lw3$fitted[j],
		col="blue",
		lwd=2)
	lw2 <- loess(sentiment_hour ~ created_at_num,
		data = input_tweets_sentiment,
		span = 0.1)
	lines(input_tweets_sentiment$created_at_num[j],lw2$fitted[j],
		col="red",
		lwd=2)
	lw1 <- loess(sentiment_day ~ created_at_num,
		data = input_tweets_sentiment,
		span = 0.1)
	lines(input_tweets_sentiment$created_at_num[j],lw1$fitted[j],
		col="black",
		lwd=2)
	
	abline(h = 0)
	legend("topright",
		legend = c("Averaged per day", "Averaged per hour", "Averaged per minute"),
		fill = c("black","red","blue"),
		#bty = "n",
		box.col = "lightgrey",
		bg = alpha("white",0.5),
		border = NA,
		title = paste0("Tweets ", account_name))
	quartz.save(paste0("~/Desktop/3_Playground/Twitter_sentiment/outputs/", account_name,"_sentiment.png"))
	dev.off()
	
	quartz(height = 3)
	par(mfrow = c(1,3))
	wordFreq <- word.frequencies(input_tweets_sentiment$text, stopwords = c("tco","hashtag"))
	wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
	    random.order=F, colors="black", scale=c(2.5,.5), rot.per=0)
	
	wordFreq <- word.frequencies(input_tweets_sentiment$text[input_tweets_sentiment$sentiment_minute > 1], stopwords = c("tco","hashtag"))
	wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
	    random.order=F, colors="green", scale=c(2.5,.5), rot.per=0)
	
	wordFreq <- word.frequencies(input_tweets_sentiment$text[input_tweets_sentiment$sentiment_minute < (-1)], stopwords = c("tco","hashtag"))
	wordcloud(words=names(wordFreq), freq=wordFreq, max.words=50, 
	    random.order=F, colors="red", scale=c(2.5,.5), rot.per=0)
	quartz.save(paste0("~/Desktop/3_Playground/Twitter_sentiment/outputs/", account_name,"_wordclouds.png"))
	dev.off()
}

#Extract tweets from a single user at a time   
# "@HillaryClinton" 
# "@realDonaldTrump"
# "@POTUS"
# "@BarackObama"

accounts_use <- c("@HillaryClinton",
	"@realDonaldTrump",
	"@POTUS",
	"@BarackObama",
	"@colbertlateshow","@LateNightSeth","@TheDailyShow","@rickygervais")

for(account_tmp in accounts_use){
	get_twitter_sentiments(account_tmp)	
}
