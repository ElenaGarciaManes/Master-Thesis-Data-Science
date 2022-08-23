
####### LISS Politics and Values | Longitudinal Panel Data

#Packages
install.packages("haven")
install.packages("dplyr")
install.packages("labelled")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("scales")
install.packages("sjPlot")
install.packages("flextable")
library(haven)
library(dplyr)
library(labelled)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales) # to access breaks/formatting functions
library(sjPlot)
library(stargazer)
library(flextable)

#Load LISS data
LISS_W2_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_2_cv09b_2.1p_EN.dta')
LISS_W3_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_3_cv10c_EN_1.0p.dta')
LISS_W4_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_4_cv11d_EN_1.0p.dta')
LISS_W5_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_5_cv12e_EN_1.0p.dta')
LISS_W6_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_6_cv13f_EN_1.0p.dta')
LISS_W7_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_7_cv14g_EN_1.0p.dta')
LISS_W8_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_8_cv16h_EN_1.0p.dta')
LISS_W9_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_9_cv17i_EN_1.0p.dta')
LISS_W10_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_10_cv18j_EN_1.0p.dta')
LISS_W11_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_11_cv19k_EN_1.0p.dta')
LISS_W12_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_12_cv20l_EN_1.0p.dta')        
LISS_W13_ <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/WV_13_cv21m_EN_1.0p.dta') 

#Load Background data
LISS_W2_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_200812_EN_2.0p.dta')
LISS_W3_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_200912_EN_2.0p.dta')
LISS_W4_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201012_EN_2.0p.dta')
LISS_W5_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201112_EN_2.0p.dta')
LISS_W6_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201212_EN_1.0p.dta')
LISS_W7_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201412_EN_1.0p.dta')
LISS_W8_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201512_EN_1.0p.dta')
LISS_W9_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201612_EN_1.0p.dta')
LISS_W10_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201712_EN_1.0p.dta')
LISS_W11_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201812_EN_1.0p.dta')
LISS_W12_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_201912_EN_1.0p.dta')
LISS_W13_B <- read_dta('/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Background/avars_202012_EN_1.0p.dta')


#Merge main + background
LISS_W2<-merge(LISS_W2_, LISS_W2_B,by= "nomem_encr")
LISS_W3<-merge(LISS_W3_, LISS_W3_B,by= "nomem_encr")
LISS_W4<-merge(LISS_W4_, LISS_W4_B,by= "nomem_encr")
LISS_W5<-merge(LISS_W5_, LISS_W5_B,by= "nomem_encr")
LISS_W6<-merge(LISS_W6_, LISS_W6_B,by= "nomem_encr")
LISS_W7<-merge(LISS_W7_, LISS_W7_B,by= "nomem_encr")
LISS_W8<-merge(LISS_W8_, LISS_W8_B,by= "nomem_encr")
LISS_W9<-merge(LISS_W9_, LISS_W9_B,by= "nomem_encr")
LISS_W10<-merge(LISS_W10_, LISS_W10_B,by= "nomem_encr")
LISS_W11<-merge(LISS_W11_, LISS_W11_B,by= "nomem_encr")
LISS_W12<-merge(LISS_W12_, LISS_W12_B,by= "nomem_encr")
LISS_W13<-merge(LISS_W13_, LISS_W13_B,by= "nomem_encr")

                     
#Variable of interest in each dataset - Media consumptions habits + Confidence: European Parliament
LISS_W2_s <-select(LISS_W2,c("nomem_encr","cv09b_m","cv09b002","cv09b003","cv09b004","cv09b005","cv09b006","cv09b007","cv09b008","cv09b014","cv09b019","sted", "oplcat", "leeftijd"))
LISS_W3_s <-select(LISS_W3,c("nomem_encr","cv10c_m","cv10c002","cv10c003","cv10c004","cv10c005","cv10c006","cv10c007","cv10c008","cv10c014","cv10c019","sted", "oplcat", "leeftijd"))
LISS_W4_s <-select(LISS_W4,c("nomem_encr","cv11d_m","cv11d002","cv11d003","cv11d004","cv11d166","cv11d006","cv11d007","cv11d008","cv11d014","cv11d019","sted", "oplcat", "leeftijd"))
LISS_W5_s <-select(LISS_W5,c("nomem_encr","cv12e_m","cv12e002","cv12e003","cv12e004","cv12e166","cv12e006","cv12e007","cv12e008","cv12e014","cv12e019","sted", "oplcat", "leeftijd"))
LISS_W6_s <-select(LISS_W6,c("nomem_encr","cv13f_m","cv13f002","cv13f003","cv13f004","cv13f166","cv13f006","cv13f007","cv13f008","cv13f014","cv13f019","sted", "oplcat", "leeftijd"))
LISS_W7_s <-select(LISS_W7,c("nomem_encr","cv14g_m","cv14g002","cv14g003","cv14g004","cv14g166","cv14g006","cv14g007","cv14g008","cv14g014","cv14g019","sted", "oplcat", "leeftijd"))

 #one of the datasets changes the way dates were stored and we use 13 columns to build a single column ("date") containing all the information regarding time
LISS_W8<-LISS_W8 %>% 
  mutate( date = coalesce(maandnr,maandnr_lang,maandnr_deel1,maandnr_deel2,maandnr_deel3,maandnr_a,maandnr_b,maandnr_c,maandnr_d,maandnr_e,maandnr_f,maandnr_i,maandnr_j))

LISS_W8_s <-select(LISS_W8,c("nomem_encr","date","cv16h002","cv16h003","cv16h004","cv16h166","cv16h006","cv16h007","cv16h008","cv16h014","cv16h019","sted", "oplcat", "leeftijd"))
LISS_W9_s <-select(LISS_W9,c("nomem_encr","cv17i_m","cv17i002","cv17i003","cv17i004","cv17i166","cv17i006","cv17i007","cv17i008","cv17i014", "cv17i019","sted", "oplcat", "leeftijd"))
LISS_W10_s <-select(LISS_W10,c("nomem_encr","cv18j_m1","cv18j002","cv18j003","cv18j004","cv18j166","cv18j006","cv18j007","cv18j008","cv18j014","cv18j019","sted", "oplcat", "leeftijd"))
LISS_W11_s <-select(LISS_W11,c("nomem_encr","cv19k_m1","cv19k002","cv19k003","cv19k004","cv19k166","cv19k006","cv19k007","cv19k008","cv19k014","cv19k019","sted", "oplcat", "leeftijd"))
LISS_W12_s <-select(LISS_W12,c("nomem_encr","cv20l_m1","cv20l002","cv20l003","cv20l004","cv20l166","cv20l006","cv20l167","cv20l008","cv20l014","cv20l019","sted", "oplcat", "leeftijd"))
LISS_W13_s <-select(LISS_W13,c("nomem_encr","cv21m_m1","cv21m002","cv21m003","cv21m004","cv21m166","cv21m006","cv21m167","cv21m008","cv21m014","cv21m019","sted", "oplcat", "leeftijd"))

#2 datasets' labelling differs from the rest
LISS_W2_s <- remove_labels(LISS_W2_s)
LISS_W3_s <- remove_labels(LISS_W3_s)
LISS_W4_s <- remove_labels(LISS_W4_s)
LISS_W5_s <- remove_labels(LISS_W5_s)
LISS_W6_s <- remove_labels(LISS_W6_s)
LISS_W7_s <- remove_labels(LISS_W7_s)
LISS_W8_s <- remove_labels(LISS_W8_s)
LISS_W9_s <- remove_labels(LISS_W9_s)
LISS_W10_s <- remove_labels(LISS_W10_s)
LISS_W11_s <- remove_labels(LISS_W11_s)
LISS_W12_s <- remove_labels(LISS_W12_s)
LISS_W13_s <- remove_labels(LISS_W13_s)

#Renaming of variables of interest

LISS_W2_s <-rename(LISS_W2_s,c("date"= "cv09b_m",
                              "follow_the_news_tv-radio" = "cv09b002",
                              "follow_the_news_internet"= "cv09b003",
                              "follow_the_news_free-newspaper" = "cv09b004",
                              "follow_the_news_paid-newspaper" = "cv09b005",
                              "follow_the_news_not-hardly" = "cv09b006",
                              "follow_the_news_dont_know" = "cv09b007",
                              "interested_in_the_news" = "cv09b008",
                              "confidence_dutch_parliament" = "cv09b014",
                              "confidence_eu_parliament" = "cv09b019",
                              "rural_urban" = "sted",
                              "education" = "oplcat", 
                              "age" = "leeftijd"))

LISS_W3_s <-rename(LISS_W3_s,c("date"= "cv10c_m",
                               "follow_the_news_tv-radio" = "cv10c002",
                               "follow_the_news_internet"= "cv10c003",
                               "follow_the_news_free-newspaper" = "cv10c004",
                               "follow_the_news_paid-newspaper" = "cv10c005",
                               "follow_the_news_not-hardly" = "cv10c006",
                               "follow_the_news_dont_know" = "cv10c007",
                               "interested_in_the_news" = "cv10c008",
                               "confidence_dutch_parliament" = "cv10c014",
                               "confidence_eu_parliament" = "cv10c019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W4_s <-rename(LISS_W4_s,c("date"= "cv11d_m",
                               "follow_the_news_tv-radio" = "cv11d002",
                               "follow_the_news_internet"= "cv11d003",
                               "follow_the_news_free-newspaper" = "cv11d004",
                               "follow_the_news_paid-newspaper" = "cv11d166",
                               "follow_the_news_not-hardly" = "cv11d006",
                               "follow_the_news_dont_know" = "cv11d007",
                               "interested_in_the_news" = "cv11d008",
                               "confidence_dutch_parliament" = "cv11d014",
                               "confidence_eu_parliament" = "cv11d019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W5_s <-rename(LISS_W5_s,c("date"= "cv12e_m",
                               "follow_the_news_tv-radio" = "cv12e002",
                               "follow_the_news_internet"= "cv12e003",
                               "follow_the_news_free-newspaper" = "cv12e004",
                               "follow_the_news_paid-newspaper" = "cv12e166",
                               "follow_the_news_not-hardly" = "cv12e006",
                               "follow_the_news_dont_know" = "cv12e007",
                               "interested_in_the_news" = "cv12e008",
                               "confidence_dutch_parliament" = "cv12e014",
                               "confidence_eu_parliament" = "cv12e019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W6_s <-rename(LISS_W6_s,c("date"= "cv13f_m",
                               "follow_the_news_tv-radio" = "cv13f002",
                               "follow_the_news_internet"= "cv13f003",
                               "follow_the_news_free-newspaper" = "cv13f004",
                               "follow_the_news_paid-newspaper" = "cv13f166",
                               "follow_the_news_not-hardly" = "cv13f006",
                               "follow_the_news_dont_know" = "cv13f007",
                               "interested_in_the_news" = "cv13f008",
                               "confidence_dutch_parliament" = "cv13f014",
                               "confidence_eu_parliament" = "cv13f019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W7_s <-rename(LISS_W7_s,c("date"= "cv14g_m",
                               "follow_the_news_tv-radio" = "cv14g002",
                               "follow_the_news_internet"= "cv14g003",
                               "follow_the_news_free-newspaper" = "cv14g004",
                               "follow_the_news_paid-newspaper" = "cv14g166",
                               "follow_the_news_not-hardly" = "cv14g006",
                               "follow_the_news_dont_know" = "cv14g007",
                               "interested_in_the_news" = "cv14g008",
                               "confidence_dutch_parliament" = "cv14g014",
                               "confidence_eu_parliament" = "cv14g019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W8_s <-rename(LISS_W8_s,c("date"= "date",
                               "follow_the_news_tv-radio" = "cv16h002",
                               "follow_the_news_internet"= "cv16h003",
                               "follow_the_news_free-newspaper" = "cv16h004",
                               "follow_the_news_paid-newspaper" = "cv16h166",
                               "follow_the_news_not-hardly" = "cv16h006",
                               "follow_the_news_dont_know" = "cv16h007",
                               "interested_in_the_news" = "cv16h008",
                               "confidence_dutch_parliament" = "cv16h014",
                               "confidence_eu_parliament" = "cv16h019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W9_s <-rename(LISS_W9_s,c("date"= "cv17i_m",
                               "follow_the_news_tv-radio" = "cv17i002",
                               "follow_the_news_internet"= "cv17i003",
                               "follow_the_news_free-newspaper" = "cv17i004",
                               "follow_the_news_paid-newspaper" = "cv17i166",
                               "follow_the_news_not-hardly" = "cv17i006",
                               "follow_the_news_dont_know" = "cv17i007",
                               "interested_in_the_news" = "cv17i008",
                               "confidence_dutch_parliament" = "cv17i014",
                               "confidence_eu_parliament" = "cv17i019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W10_s <-rename(LISS_W10_s,c("date"= "cv18j_m1",
                               "follow_the_news_tv-radio" = "cv18j002",
                               "follow_the_news_internet"= "cv18j003",
                               "follow_the_news_free-newspaper" = "cv18j004",
                               "follow_the_news_paid-newspaper" = "cv18j166",
                               "follow_the_news_not-hardly" = "cv18j006",
                               "follow_the_news_dont_know" = "cv18j007",
                               "interested_in_the_news" = "cv18j008",
                               "confidence_dutch_parliament" = "cv18j014",
                               "confidence_eu_parliament" = "cv18j019",
                               "rural_urban" = "sted",
                               "education" = "oplcat", 
                               "age" = "leeftijd"))

LISS_W11_s <-rename(LISS_W11_s,c("date"= "cv19k_m1",
                                 "follow_the_news_tv-radio" = "cv19k002",
                                 "follow_the_news_internet"= "cv19k003",
                                 "follow_the_news_free-newspaper" = "cv19k004",
                                 "follow_the_news_paid-newspaper" = "cv19k166",
                                 "follow_the_news_not-hardly" = "cv19k006",
                                 "follow_the_news_dont_know" = "cv19k007",
                                 "interested_in_the_news" = "cv19k008",
                                 "confidence_dutch_parliament" = "cv19k014",
                                 "confidence_eu_parliament" = "cv19k019",
                                 "rural_urban" = "sted",
                                 "education" = "oplcat", 
                                 "age" = "leeftijd"))

LISS_W12_s <-rename(LISS_W12_s,c("date"= "cv20l_m1",
                                 "follow_the_news_tv-radio" = "cv20l002",
                                 "follow_the_news_internet"= "cv20l003",
                                 "follow_the_news_free-newspaper" = "cv20l004",
                                 "follow_the_news_paid-newspaper" = "cv20l166",
                                 "follow_the_news_not-hardly" = "cv20l006",
                                 "follow_the_news_dont_know" = "cv20l167",
                                 "interested_in_the_news" = "cv20l008",
                                 "confidence_dutch_parliament" = "cv20l014",
                                 "confidence_eu_parliament" = "cv20l019",
                                 "rural_urban" = "sted",
                                 "education" = "oplcat", 
                                 "age" = "leeftijd"))

LISS_W13_s <-rename(LISS_W13_s,c("date"= "cv21m_m1",
                                 "follow_the_news_tv-radio" = "cv21m002",
                                 "follow_the_news_internet"= "cv21m003",
                                 "follow_the_news_free-newspaper" = "cv21m004",
                                 "follow_the_news_paid-newspaper" = "cv21m166",
                                 "follow_the_news_not-hardly" = "cv21m006",
                                 "follow_the_news_dont_know" = "cv21m167",
                                 "interested_in_the_news" = "cv21m008",
                                 "confidence_dutch_parliament" = "cv21m014",
                                 "confidence_eu_parliament" = "cv21m019",
                                 "rural_urban" = "sted",
                                 "education" = "oplcat", 
                                 "age" = "leeftijd"))

#Merging the 13 datasets together
LISS_merged <- rbind(LISS_W2_s, LISS_W3_s, LISS_W4_s, LISS_W5_s, LISS_W6_s, LISS_W7_s, LISS_W8_s, LISS_W9_s, LISS_W10_s, LISS_W11_s, LISS_W12_s, LISS_W13_s) 

#Cleaning variables on Confidence in the EU Parliament and Dutch Parliament
LISS_merged <- LISS_merged %>%
  drop_na(confidence_eu_parliament) %>%
  subset(confidence_eu_parliament != -9) %>%
  subset(confidence_eu_parliament != 999)

LISS_merged <- LISS_merged %>%
  drop_na(confidence_dutch_parliament) %>%
  subset(confidence_dutch_parliament != -9) %>%
  subset(confidence_dutch_parliament != 999)

#Managing dates
LISS_merged$date <-as.Date(paste(as.character.Date(LISS_merged$date),"01",sep=""), "%Y%m%d") #transforming number into a date
unique(LISS_merged$date)

#Change of column type to integer on Confidence in the EU Parliament and Dutch Parliament variables
LISS_merged$confidence_eu_parliament <- as.integer(LISS_merged$confidence_eu_parliament)
LISS_merged$confidence_dutch_parliament <- as.integer(LISS_merged$confidence_dutch_parliament)
  
#Mutate: news consumption habits
LISS_merged <- LISS_merged %>%
  mutate(news_consumption = (`follow_the_news_free-newspaper` == 1 |`follow_the_news_paid-newspaper` ==1))

#Mean Trust EP & Dutch Parliament 
mean_trust_EP <- LISS_merged %>%
  group_by(date) %>%
  summarize(mean_trust = mean(confidence_eu_parliament))

mean_trust_DP <- LISS_merged %>%
  group_by(date) %>%
  summarize(mean_trust2 = mean(confidence_dutch_parliament))  

ggplot(mean_trust_EP) + 
  geom_line(aes(x = date, y = mean_trust))

ggplot(mean_trust_DP) + 
  geom_line(aes(x = date, y = mean_trust2))

#People who consume news MEAN CONFIDENCE
consumption_news <- LISS_merged %>%
  filter(news_consumption == T) %>%
  group_by(date) %>%
  summarize(mean_trust = mean(confidence_eu_parliament))

#Plot consumption news over time
ggplot(consumption_news, aes(x = date, y = mean_trust)) + 
  geom_line(colour = '#142B42', size = 1) +
  geom_point(colour = '#142B42', size = 1.5) +
  geom_vline(xintercept = as.POSIXct(as.Date("2020-03-01")), 
             color = "red", 
             lwd = 2) +
  scale_x_date(breaks = as.Date(c("2007-12-01" ,"2008-03-01", "2008-12-01", "2009-01-01" ,
                                  "2009-12-01", "2010-01-01", "2010-02-01", "2011-01-01",
                                  "2010-12-01","2011-12-01", "2012-01-01", "2012-12-01",
                                  "2013-01-01", "2013-12-01","2014-01-01","2015-12-01",
                                  "2016-01-01", "2016-12-01" ,"2017-01-01", "2017-12-01", 
                                  "2018-01-01", "2018-12-01", "2019-01-01" ,"2019-12-01", 
                                  "2020-01-01", "2020-12-01", "2021-01-01")), date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), linetype="dashed", 
             color = "#5E5E5E", size=0.7) 

#Create categorical variables for cohort, edu, urb and trust_eu¨_levels and drop NAs

LISS_merged <- LISS_merged %>%
  mutate(cohort = case_when(age>=16 & age<=29 ~ 'Age 16-29',
                            age>=30 & age<=55 ~ 'Age 30-55',
                            age>=56 ~'Age 56+'),
         edu = case_when(education>=1 & education<=2 ~ '1.Low',
                         education>=3 & education<=5 ~ '2.Medium',
                         education==6 ~ '3.High'),
         urb = case_when(rural_urban>=1 & rural_urban<=2 ~ 'Urban',
                         rural_urban>=3 & rural_urban<=5 ~ 'Rural'),
         trust_eu_levels = case_when(confidence_eu_parliament>=0 & confidence_eu_parliament<=3 ~ '1.Low',
                                     confidence_eu_parliament>=4 & confidence_eu_parliament<=6 ~ '2.Medium',
                                     confidence_eu_parliament>=7 ~ '3.High'))

LISS_merged <- LISS_merged %>%
  drop_na(cohort,edu,urb) 

# Summary table
table1::label(LISS_merged$cohort) <- "Age cohort"
table1::label(LISS_merged$edu) <- "Educational attainment" 
table1::label(LISS_merged$urb) <- "Rural vs Urban" 
sum_trust <- table1::table1(~cohort+edu+urb | trust_eu_levels , data = LISS_merged)
sum_trust

n_distinct(LISS_merged$nomem_encr)


write.csv(LISS_merged,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/LISS_trust/LISS_merged.csv")

#Ploting trust by news consumption over time
mean_trust_EP_news <- LISS_merged %>%
group_by(date, news_consumption) %>%
summarize(mean_trust = mean(confidence_eu_parliament))


ggplot(mean_trust_EP_news, aes(x = date, y = mean_trust, color= news_consumption)) + 
  geom_line(size = 1) +
  scale_color_manual(values=c('#142B42','#56B1F7'))+
  geom_point(size = 2) +
  scale_x_date(breaks = as.Date(c("2007-12-01" ,"2008-03-01", "2008-12-01", "2009-01-01" ,
                                  "2009-12-01", "2010-01-01", "2010-02-01", "2011-01-01",
                                  "2010-12-01","2011-12-01", "2012-01-01", "2012-12-01",
                                  "2013-01-01", "2013-12-01","2014-01-01","2015-12-01",
                                  "2016-01-01", "2016-12-01" ,"2017-01-01", "2017-12-01", 
                                  "2018-01-01", "2018-12-01", "2019-01-01" ,"2019-12-01", 
                                  "2020-01-01", "2020-12-01", "2021-01-01")), date_minor_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), linetype="dashed", 
             color = "#5E5E5E", size=0.7) 

LISS_merged
