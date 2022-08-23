install.packages("corrplot")
install.packages("corrgram")
install.packages("Hmisc")
install.packages("margins")

library(tm)
library(pdftools)
library(SnowballC)
library(wordcloud)
library(readtext)
library(stringr)
library(dplyr)
library(magicfor)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(tidyverse)
library(xml2)
library(tidytext)
library(cld2)
library(googleLanguageR)
library(stargazer)
library(corrplot)
library(corrgram)
library(Hmisc)
library(margins)
library(ggeffects)


#Reading LISS data and preprocessed headlines
liss_2 <- read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/LISS_df.csv") 
headlines <- read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/headlines_preprocessed.csv") 

#Selecting relevant columns
headlines_2 <-select(headlines,c('date','sentiment.sentiment'))


#Selecting relevant data and creating categorical variable with the different time periods.
liss_2 <- liss_2  %>%
  filter(date >= 2019/01/01)
liss_2 <- liss_2  %>%
  mutate(time_point = case_when(date == '2019-12-01' ~ "Period 1", 
                                date == '2020-01-01' ~ "Period 2",
                                date == '2020-12-01' ~ "Period 3", 
                                date == '2021-01-01' ~ "Period 4"))

headlines_2$time <- case_when((headlines_2$date>='2019-01-01' & headlines_2$date<='2019-12-01') ~ as.Date('2019-12-01'),
                         (headlines_2$date>'2019-12-01' & headlines_2$date<='2020-01-01') ~ as.Date('2020-01-01'),
                         (headlines_2$date>'2020-01-01' & headlines_2$date<='2020-12-01') ~ as.Date('2020-12-01'),
                         (headlines_2$date>'2020-12-01' & headlines_2$date<='2021-01-01') ~ as.Date('2021-01-01'),
                         headlines_2$date>'2021-01-01'~ as.Date('2021-12-01'))  

headlines_2 <- headlines_2  %>%
  mutate(time_point = case_when(time == '2019-12-01' ~ "Period 1", 
                                time == '2020-01-01' ~ "Period 2",
                                time == '2020-12-01' ~ "Period 3", 
                                time == '2021-01-01' ~ "Period 4"))

#Drop NAs
headlines_2 <- headlines_2 %>%
  drop_na(time_point) 

#Calculating average sentiment per time point
headlines_2 <- headlines_2 %>%
  group_by(time_point) %>%
  mutate(sentiment_mean = mean(sentiment.sentiment))

headlines_final <-select(headlines_2,c('time_point','sentiment_mean'))

headlines_final <- headlines_final  %>%
  group_by(time_point) %>%
  summarise(mean(sentiment_mean))

#Merging sentiment data with LISS data
df_final <- merge(headlines_final, liss_2, by="time_point")

------
#Count volume of eu news over time
headlines_ <-select(headlines,c('headlines','date','sentiment.sentiment'))


#Detecting pre and post COVID-19 tokens
headlines_ <- headlines_  %>%
  mutate(is_covid = case_when(date <= '2020-03-01' ~ "Pre-COVID-19", 
                              date > '2020-03-01' ~ "Post-COVID-19"))


headlines_$date<-as.Date(headlines_$date)

#Groping by time period (pre or post COVID) and counting headlines
headlines_count <- headlines_ %>% 
  group_by(month = lubridate::floor_date(date, "month")) %>%
  tally() %>%
  mutate(is_covid = case_when(month <= '2020-03-01' ~ "Pre-COVID-19", 
                              month > '2020-03-01' ~ "Post-COVID-19"))


#Mean of headlines by time period (pre or post COVID)
headlines_count.mean <- headlines_count %>%
  group_by(is_covid) %>%
  mutate(ymean = mean(n))


#Plot headlines count pre and post COVID
ggplot(headlines_count, aes(month, n, fill=is_covid)) +
  geom_col() + 
  scale_fill_manual(values = c('#56B1F7','#142B42')) +
  geom_errorbar(data=headlines_count.mean, aes(month, ymax = ymean, ymin = ymean),
                size=1, linetype = "longdash", inherit.aes = F, width = 10) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), linetype="dashed", 
             color = "#5E5E5E", size=0.7)

#Renaming column mean(sentiment_mean) as sentiment 
df_final <-rename(df_final,"sentiment" = "mean(sentiment_mean)")

---------
#Models 
lm_1 <- lm(confidence_eu_parliament ~ sentiment + news_consumption + sentiment*news_consumption, data = df_final)
stargazer(lm_1, type = "html", out = "lm_1.html", style = "aer", covariate.labels = c("Sentiment","News consumption", "Sentiment * News consumption"))

lm_2 <- lm(confidence_eu_parliament ~ sentiment + news_consumption + sentiment*news_consumption + edu + cohort + urb, data = df_final)
stargazer(lm_2, type = "html", out = "lm_2.html", style = "aer")

#Models pre and post covid 
lm_pre_covid <- df_final %>%
  filter(date <= '2020-01-01')

lm_3 <- lm(confidence_eu_parliament ~ sentiment + news_consumption + sentiment*news_consumption, data = df_final)
stargazer(lm_3, type = "html", out = "lm_3.html", style = "aer", covariate.labels = c("Sentiment","News consumption", "Sentiment * News consumption"))

lm_4 <- lm(confidence_eu_parliament ~ sentiment + news_consumption + sentiment*news_consumption + edu + cohort + urb, data = df_final)
stargazer(lm_4, type = "html", out = "lm_4.html", style = "aer")


##loess default estimation
ggplot(mean_trust_EP, aes(x=date, y= mean_trust)) + 
  geom_point() +
  geom_smooth(method="loess") +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), linetype="dashed", 
             color = "#5E5E5E", size=0.7) 

#Detecting positive and negative headlines
df_final_2 <- df_final %>%
  mutate(
    sentiment_binary = case_when(
      sentiment >= 0 ~ "Positive",
      sentiment < 0 ~ "Negative"))

df_final_positive <- df_final_2 %>%
  filter(sentiment_binary == "Positive")

df_final_negative <- df_final_2 %>%
  filter(sentiment_binary == "Negative")

df_final_negative <- drop_na(df_final_negative) 

#Models 
lm_5 <- lm(confidence_eu_parliament ~ news_consumption + sentiment_binary + news_consumption*sentiment_binary + cohort + edu, data = df_final_2)
lm_6 <- lm(confidence_eu_parliament ~ news_consumption, data = df_final_2)
stargazer(lm_5, type = "html", out = "ols.html", style = "aer", covariate.labels = c("News consumption", "News environment POSITIVE","Age 30-55", "Age 56+", "Low education", "Medium education", "Media consumption x News environment"))
stargazer(lm_6, type = "html", out = "ols_2.html", style = "aer", covariate.labels = c("News consumption"))

write.csv(df_final_2,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/DF_FINAL_2.csv")

#Marginal effects trust

df_final_marginal_efects<-df_final_2
df_final_marginal_efects$news_consumption

#Transforming logical to factor
df_final_marginal_efects$news_consumption<-factor(df_final_2$news_consumption)
df_final_marginal_efects$sentiment_binary<-factor(df_final_2$sentiment_binary)
lm_5_marginal <- lm(confidence_eu_parliament ~ sentiment_binary + news_consumption + news_consumption*sentiment_binary, data = df_final_marginal_efects)
lm_6_marginal <- lm(confidence_eu_parliament ~ news_consumption, data = df_final_marginal_efects)

#Ploting marginal effects 
plot_model(lm_6_marginal, type = "pred")
plot_model(lm_5_marginal, type = "int", colors =c('#142B42','#56B1F7'))

