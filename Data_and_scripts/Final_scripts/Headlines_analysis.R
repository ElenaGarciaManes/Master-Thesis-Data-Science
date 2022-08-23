install.packages('tm')
install.packages('pdftools')
install.packages('SnowballC')
install.packages('wordcloud')
install.packages('analyzeSentiment')
install.packages('readtext')
install.packages('stringr')
install.packages('dplyr')
install.packages('magicfor')
install.packages('syuzhet')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('scales')
install.packages('reshape2')
install.packages('tidyverse')
install.packages('xml2')
install.packages('tidytext')
install.packages('textcat')
install.packages('googleLanguageR')
install.packages('sentimentr')


library(textcat)
library(tm)
library(pdftools)
library(SnowballC)
library(wordcloud)
library(analyzeSentiment)
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
library(sentimentr)

#Loading individuals pdfs
pdf_1 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-1.pdf'
pdf_2 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-2.pdf'
pdf_3 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-3.pdf'
pdf_4 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-4.pdf'
pdf_5 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-5.pdf'
pdf_6 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-6.pdf'
pdf_7 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-7.pdf'
pdf_8 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-8.pdf'
pdf_9 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-9.pdf'
pdf_10 = '/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Factiva_headlines/Factiva-10.pdf'

#Reading the PDFs
data_1=pdftools::pdf_text(pdf_1, opw = "", upw = "")
data_2=pdftools::pdf_text(pdf_2, opw = "", upw = "")
data_3=pdftools::pdf_text(pdf_3, opw = "", upw = "")
data_4=pdftools::pdf_text(pdf_4, opw = "", upw = "")
data_5=pdftools::pdf_text(pdf_5, opw = "", upw = "")
data_6=pdftools::pdf_text(pdf_6, opw = "", upw = "")
data_7=pdftools::pdf_text(pdf_7, opw = "", upw = "")
data_8=pdftools::pdf_text(pdf_8, opw = "", upw = "")
data_9=pdftools::pdf_text(pdf_9, opw = "", upw = "")
data_10=pdftools::pdf_text(pdf_10, opw = "", upw = "")

#Pre-processing the data

#PDF 1

#Extracting headlines and dates
data_1=str_split(data_1, "\n\n")
data_1=unlist(data_1,recursive=FALSE)
headlines_1=str_extract_all(data_1, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_1=unlist(str_extract_all(headlines_1, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_1=data.frame(dates_1)
headline_clean_1=str_replace(unlist(headlines_1),'([^\\n]*$)','')
headline_df_1=data.frame(headline_clean_1)

headline_df_1<-headline_df_1 %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Removing headlines with no dates and transforming into a dataframe
date_df_1=date_df_1[-53,]
date_df_1=data.frame(date_df_1)

#Combine headlines and dates
df_concat_1 = cbind(headline_df_1, date_df_1)

#PDF 2
#Extracting headlines and dates
data_2=str_split(data_2, "\n\n")
data_2=unlist(data_2,recursive=FALSE)
headlines_2=str_extract_all(data_2, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_2=unlist(str_extract_all(headlines_2, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_2=data.frame(dates_2)
headline_clean_2=str_replace(unlist(headlines_2),'([^\\n]*$)','')
headline_df_2=data.frame(headline_clean_2)

headline_df_2<-headline_df_2 %>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Combine headlines and dates
df_concat_2 = cbind(headline_df_2, date_df_2)

#PDF 3
#Extracting headlines and dates
data_3=str_split(data_3, "\n\n")
data_3=unlist(data_3,recursive=FALSE)
headlines_3=str_extract_all(data_3, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_3=unlist(str_extract_all(headlines_3, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_3=data.frame(dates_3)
headline_clean_3=str_replace(unlist(headlines_3),'([^\\n]*$)','')
headline_df_3=data.frame(headline_clean_3)

headline_df_3<-headline_df_3%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Removing headlines with no dates and transforming into a dataframe
date_df_3=date_df_3[-63,]
date_df_3=data.frame(date_df_3)

#Combine headlines and dates
df_concat_3 = cbind(headline_df_3, date_df_3)

#PDF 4
#Extracting headlines and dates
data_4=str_split(data_4, "\n\n")
data_4=unlist(data_4,recursive=FALSE)
headlines_4=str_extract_all(data_4, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_4=unlist(str_extract_all(headlines_4, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_4=data.frame(dates_4)
headline_clean_4=str_replace(unlist(headlines_4),'([^\\n]*$)','')
headline_df_4=data.frame(headline_clean_4)

headline_df_4<-headline_df_4%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Removing headlines with no dates and transforming into a dataframe
date_df_4=date_df_4[-c(52,62),]
date_df_4=data.frame(date_df_4)

#Combine headlines and dates
df_concat_4 = cbind(headline_df_4, date_df_4)

#PDF 5
#Extracting headlines and dates
data_5=str_split(data_5, "\n\n")
data_5=unlist(data_5,recursive=FALSE)
headlines_5=str_extract_all(data_5, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_5=unlist(str_extract_all(headlines_5, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_5=data.frame(dates_5)
headline_clean_5=str_replace(unlist(headlines_5),'([^\\n]*$)','')
headline_df_5=data.frame(headline_clean_5)

headline_df_5<-headline_df_5%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Combine headlines and dates
df_concat_5 = cbind(headline_df_5, date_df_5)

#PDF 6
#Extracting headlines and dates
data_6=str_split(data_6, "\n\n")
data_6=unlist(data_6,recursive=FALSE)
headlines_6=str_extract_all(data_6, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_6=unlist(str_extract_all(headlines_6, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_6=data.frame(dates_6)
headline_clean_6=str_replace(unlist(headlines_6),'([^\\n]*$)','')
headline_df_6=data.frame(headline_clean_6)

headline_df_6<-headline_df_6%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Removing headlines with no dates and transforming into a dataframe
date_df_6=date_df_6[-c(21,64),]
date_df_6=data.frame(date_df_6)

#Combine headlines and dates
df_concat_6 = cbind(headline_df_6, date_df_6)

#PDF 7
#Extracting headlines and dates
data_7=str_split(data_7, "\n\n")
data_7=unlist(data_7,recursive=FALSE)
headlines_7=str_extract_all(data_7, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_7=unlist(str_extract_all(headlines_7, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_7=data.frame(dates_7)
headline_clean_7=str_replace(unlist(headlines_7),'([^\\n]*$)','')
headline_df_7=data.frame(headline_clean_7)

headline_df_7<-headline_df_7%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Combine headlines and dates
df_concat_7 = cbind(headline_df_7, date_df_7)

#PDF 8
#Extracting headlines and dates
data_8=str_split(data_8, "\n\n")
data_8=unlist(data_8,recursive=FALSE)
headlines_8=str_extract_all(data_8, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_8=unlist(str_extract_all(headlines_8, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_8=data.frame(dates_8)
headline_clean_8=str_replace(unlist(headlines_8),'([^\\n]*$)','')
headline_df_8=data.frame(headline_clean_8)

headline_df_8<-headline_df_8%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Combine headlines and dates
df_concat_8 = cbind(headline_df_8, date_df_8)

#PDF 9
#Extracting headlines and dates
data_9=str_split(data_9, "\n\n")
data_9=unlist(data_9,recursive=FALSE)
headlines_9=str_extract_all(data_9, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_9=unlist(str_extract_all(headlines_9, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_9=data.frame(dates_9)
headline_clean_9=str_replace(unlist(headlines_9),'([^\\n]*$)','')
headline_df_9=data.frame(headline_clean_9)

headline_df_9<-headline_df_9%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Removing headlines with no dates and transforming into a dataframe
date_df_9=date_df_9[-73,]
date_df_9=data.frame(date_df_9)

#Combine headlines and dates
df_concat_9 = cbind(headline_df_9, date_df_9)

#PDF 10
#Extracting headlines and dates
data_10=str_split(data_10, "\n\n")
data_10=unlist(data_10,recursive=FALSE)
headlines_10=str_extract_all(data_10, '(?<=)(?s)(.*?)(?= \\d* palabras)')
dates_10=unlist(str_extract_all(headlines_10, '\\d{1,2} de [a-zA-Z]* de \\d{4}'))
date_df_10=data.frame(dates_10)
headline_clean_10=str_replace(unlist(headlines_10),'([^\\n]*$)','')
headline_df_10=data.frame(headline_clean_10)

headline_df_10<-headline_df_10%>%
  # recode empty strings "" by NAs
  na_if("") %>%
  # remove NAs
  na.omit

#Removing headlines with no dates and transforming into a dataframe
date_df_10=date_df_10[-c(21,95),]
date_df_10=data.frame(date_df_10)

#Combine headlines and dates
df_concat_10 = cbind(headline_df_10, date_df_10)

#Standarise dates
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,' de ','-')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'enero','01')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'febrero','02')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'marzo','03')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'abril','04')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'mayo','05')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'junio','06')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'julio','07')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'agosto','08')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'septiembre','09')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'octubre','10')
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'noviembre','11') 
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'diciembre','12') 
df_concat_1$date_df_1<-str_replace_all(df_concat_1$date_df_1,'/','-')
df_concat_1$date_df_1<-as.Date(df_concat_1$date_df_1, format = '%d-%m-%Y')

df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,' de ','-')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'enero','01')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'febrero','02')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'marzo','03')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'abril','04')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'mayo','05')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'junio','07')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'julio','07')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'agosto','08')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'septiembre','09')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'octubre','10')
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'noviembre','11') 
df_concat_2$dates_2<-str_replace_all(df_concat_2$dates_2,'diciembre','12') 
df_concat_2$dates_2<-as.Date(df_concat_2$dates_2, format = '%d-%m-%Y')

df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,' de ','-')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'enero','01')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'febrero','02')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'marzo','03')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'abril','04')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'mayo','05')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'junio','06')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'julio','07')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'agosto','08')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'septiembre','09')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'octubre','10')
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'noviembre','11') 
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'diciembre','12') 
df_concat_3$date_df_3<-str_replace_all(df_concat_3$date_df_3,'/','-')
df_concat_3$date_df_3<-as.Date(df_concat_3$date_df_3, format = '%d-%m-%Y')

df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,' de ','-')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'enero','01')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'febrero','02')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'marzo','03')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'abril','04')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'mayo','05')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'junio','06')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'julio','07')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'agosto','08')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'septiembre','09')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'octubre','10')
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'noviembre','11') 
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'diciembre','12') 
df_concat_4$date_df_4<-str_replace_all(df_concat_4$date_df_4,'/','-')
df_concat_4$date_df_4<-as.Date(df_concat_4$date_df_4, format = '%d-%m-%Y')

df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,' de ','-')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'enero','01')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'febrero','02')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'marzo','03')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'abril','04')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'mayo','05')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'junio','06')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'julio','07')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'agosto','08')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'septiembre','09')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'octubre','10')
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'noviembre','11') 
df_concat_5$dates_5<-str_replace_all(df_concat_5$dates_5,'diciembre','12') 
df_concat_5$dates_5<-as.Date(df_concat_5$dates_5, format = '%d-%m-%Y')

df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,' de ','-')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'enero','01')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'febrero','02')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'marzo','03')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'abril','04')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'mayo','05')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'junio','06')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'julio','07')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'agosto','08')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'septiembre','09')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'octubre','10')
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'noviembre','11') 
df_concat_6$date_df_6<-str_replace_all(df_concat_6$date_df_6,'diciembre','12') 
df_concat_6$date_df_6<-as.Date(df_concat_6$date_df_6, format = '%d-%m-%Y')

df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,' de ','-')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'enero','01')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'febrero','02')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'marzo','03')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'abril','04')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'mayo','05')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'junio','06')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'julio','07')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'agosto','08')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'septiembre','09')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'octubre','10')
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'noviembre','11') 
df_concat_7$dates_7<-str_replace_all(df_concat_7$dates_7,'diciembre','12') 
df_concat_7$dates_7<-as.Date(df_concat_7$dates_7, format = '%d-%m-%Y')

df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,' de ','-')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'enero','01')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'febrero','02')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'marzo','03')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'abril','04')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'mayo','05')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'junio','06')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'julio','07')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'agosto','08')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'septiembre','09')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'octubre','10')
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'noviembre','11') 
df_concat_8$dates_8<-str_replace_all(df_concat_8$dates_8,'diciembre','12') 
df_concat_8$dates_8<-as.Date(df_concat_8$dates_8, format = '%d-%m-%Y')

df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,' de ','-')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'enero','01')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'febrero','02')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'marzo','03')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'abril','04')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'mayo','05')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'junio','06')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'julio','07')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'agosto','08')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'septiembre','09')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'octubre','10')
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'noviembre','11') 
df_concat_9$date_df_9<-str_replace_all(df_concat_9$date_df_9,'diciembre','12') 
df_concat_9$date_df_9<-as.Date(df_concat_9$date_df_9, format = '%d-%m-%Y')

df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,' de ','-')
df_concat_10$date_df_10<-str_replace_all(x,'enero','01')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'febrero','02')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'marzo','03')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'abril','04')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'mayo','05')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'junio','06')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'julio','07')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'agosto','08')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'septiembre','09')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'octubre','10')
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'noviembre','11') 
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'diciembre','12') 
df_concat_10$date_df_10<-str_replace_all(df_concat_10$date_df_10,'/','-')
df_concat_10$date_df_10<-as.Date(df_concat_10$date_df_10, format = '%d-%m-%Y')


-------------------
##Titles translation
  
  
##Section where we translate the headlines from Dutch into English with Google Translate API from Google Cloud Platform (GCP)
##GCP is a paid service, therefore this section was disable to not incur in cost. We've translated all the headlines and we've stored all the headlines with the respective translations in a csv. 
  
##Credentials
#gl_auth("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Credentials/arched-inkwell-346020-5e488f8af973.json")


##Removing whitespaces from the left and right sides of strings
#df_concat_1$headline_sq<-str_squish(df_concat_1$headline_clean_1)
#headlines_sq_1<-df_concat_1$headline_sq

##Translating headlines
#tr_1 <- 
#  gl_translate(
#    headlines_sq_1,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_1$title_en <- tr_1$translatedText 

--
  
##Removing whitespaces from the left and right sides of strings
#df_concat_2$headline_sq<-str_squish(df_concat_2$headline_clean_2)
#headlines_sq_2<-df_concat_2$headline_sq

##Translating headlines
#tr_2 <- 
#  gl_translate(
#    headlines_sq_2,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_2$title_en <- tr_2$translatedText 

--
  
##Removing whitespaces from the left and right sides of strings
#df_concat_3$headline_sq<-str_squish(df_concat_3$headline_clean_3)
#headlines_sq_3<-df_concat_3$headline_sq

##Translating headlines
#tr_3 <- 
#  gl_translate(
#    headlines_sq_3,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_3$title_en <- tr_3$translatedText 

--
  
##Removing whitespaces from the left and right sides of strings
#df_concat_4$headline_sq<-str_squish(df_concat_4$headline_clean_4)
#headlines_sq_4<-df_concat_4$headline_sq

##Translating headlines
#tr_4 <- 
#  gl_translate(
#    headlines_sq_4,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_4$title_en <- tr_4$translatedText 

--
##Removing whitespaces from the left and right sides of strings
  
#df_concat_5$headline_sq<-str_squish(df_concat_5$headline_clean_5)
#headlines_sq_5<-df_concat_5$headline_sq

##Translating headlines
#tr_5 <- 
#  gl_translate(
#    headlines_sq_5,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_5$title_en <- tr_5$translatedText 

--
  
##Removing whitespaces from the left and right sides of strings  
#df_concat_6$headline_sq<-str_squish(df_concat_6$headline_clean_6)
#headlines_sq_6<-df_concat_6$headline_sq

##Translating headlines
#tr_6 <- 
#  gl_translate(
#    headlines_sq_6,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing translated headlines in dataframe as title_en
#df_concat_6$title_en <- tr_6$translatedText 

--
##Removing whitespaces from the left and right sides of strings
#df_concat_7$headline_sq<-str_squish(df_concat_7$headline_clean_7)
#headlines_sq_7<-df_concat_7$headline_sq

##Translating headlines
#tr_7 <- 
#  gl_translate(
#    headlines_sq_7,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing translated headlines in dataframe as title_en
#df_concat_7$title_en <- tr_7$translatedText 

--
##Removing whitespaces from the left and right sides of strings
#df_concat_8$headline_sq<-str_squish(df_concat_8$headline_clean_8)
#headlines_sq_8<-df_concat_8$headline_sq

##Translating headlines
#tr_8 <- 
#  gl_translate(
#    headlines_sq_8,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_8$title_en <- tr_8$translatedText 

--
  
##Removing whitespaces from the left and right sides of strings
#df_concat_9$headline_sq<-str_squish(df_concat_9$headline_clean_9)
#headlines_sq_9<-df_concat_9$headline_sq

##Translating headlines
#tr_9 <- 
#  gl_translate(
#    headlines_sq_9,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_9$title_en <- tr_9$translatedText 

--
##Removing whitespaces from the left and right sides of strings
#df_concat_10$headline_sq<-str_squish(df_concat_10$headline_clean_10)
#headlines_sq_10<-df_concat_10$headline_sq

##Translating headlines
#tr_10 <- 
#  gl_translate(
#    headlines_sq_10,
#    target = "en",
#    format = c("text"),
#    source = "")

##Storing traslated headlines in dataframe as title_en
#df_concat_10$title_en <- tr_10$translatedText 


##Saving translations in .csv

#write.csv(df_concat_1,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_1_translated.csv")
#write.csv(df_concat_2,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_2_translated.csv")
#write.csv(df_concat_3,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_3_translated.csv")
#write.csv(df_concat_4,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_4_translated.csv")
#write.csv(df_concat_5,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_5_translated.csv")
#write.csv(df_concat_6,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_6_translated.csv")
#write.csv(df_concat_7,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_7_translated.csv")
#write.csv(df_concat_8,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_8_translated.csv")
#write.csv(df_concat_9,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_9_translated.csv")
#write.csv(df_concat_10,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_10_translated.csv")


#Reading cvs.

df_1<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_1_translated.csv")
df_2<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_2_translated.csv")
df_3<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_3_translated.csv")
df_4<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_4_translated.csv")
df_5<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_5_translated.csv")
df_6<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_6_translated.csv")
df_7<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_7_translated.csv")
df_8<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_8_translated.csv")
df_9<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_9_translated.csv")
df_10<-read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/Factiva_10_translated.csv")


#Standarising column names and combining headlines dataframes
df_1<-rename(df_1, c(headlines=headline_clean_1,date=date_df_1 ))
df_2<-rename(df_2, c(headlines=headline_clean_2,date=dates_2 ))
df_3<-rename(df_3, c(headlines=headline_clean_3,date=date_df_3 ))
df_4<-rename(df_4, c(headlines=headline_clean_4,date=date_df_4 ))
df_5<-rename(df_5, c(headlines=headline_clean_5,date=dates_5 ))
df_6<-rename(df_6, c(headlines=headline_clean_6,date=date_df_6 ))
df_7<-rename(df_7, c(headlines=headline_clean_7,date=dates_7 ))
df_8<-rename(df_8, c(headlines=headline_clean_8,date=dates_8 ))
df_9<-rename(df_9, c(headlines=headline_clean_9,date=date_df_9 ))
df_10<-rename(df_10, c(headlines=headline_clean_10,date=date_df_10 ))
df<-rbind(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10)
df=df[-c(198,199,200,201,202,203,204),]

-------------

#Pre-processing headlines
corpus<-Corpus(VectorSource(df$title_en))
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Creating list with customized stopwords and removing stop words from headlines
stop<-stopwords('en')
stop<-data.frame(stop)

#Removing negations from stopwords list
stop<-stop[-c(81,82,83,84,85,86,87,88,89,90,91,92,93),]
stop<-data.frame(stop)
stop<-stop[-c(81,82,83,84,85,152,153,154),]
corpus <- tm_map(corpus, removeWords, stop)
corpus <- tm_map(corpus, stemDocument) 

#Saving preprocessed headlines in dataframe as title_p
df$title_p<-unlist(as.list(corpus))
df$title_p<-trimws(df$title_p) 

#DTM <- DocumentTermMatrix(corpus)
#inspect(DTM)
#tdm <- TermDocumentMatrix(corpus)
#tdm <- as.matrix(tdm)

#w <- rowSums(tdm)
#w <- subset(w, w>=4)
#barplot(w,
#        las = 2,
#        col = rainbow(50))


#w <- sort(rowSums(tdm), decreasing = TRUE)
#set.seed(222)
#wordcloud(words = names(w),
#          freq = w,
#          max.words = 150,
#          random.order = F,
#          min.freq = 5,
#          colors = brewer.pal(8, 'Dark2'),
#          scale = c(2, 0.3),
#          rot.per = 0.7)

#Saving pre-processed headlines
write.csv(df,"/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/headlines_preprocessed.csv")

#Reading pre-processed headlines
df <- read.csv("/Users/elena/Documents/Hertie_MPP/2nd_Year/THESIS/DATA/Translations/headlines_preprocessed.csv") 

----------
#Sentiment analysis with SentimentR
df$sentiment<-sentimentr::sentiment(df$title_p)
df$date<-as.Date(df$date) #Converts column date between character representations and objects of class "Date"

#Creating column grouping the dates based on the different LISS waves.
df$date_LISS<- case_when((df$date>='2019-01-01' & df$date<='2019-12-01') ~ as.Date('2019-12-01'),
                         (df$date>'2019-12-01' & df$date<='2020-01-01') ~ as.Date('2020-01-01'),
                         (df$date>'2020-01-01' & df$date<='2020-12-01') ~ as.Date('2020-12-01'),
                         (df$date>'2020-12-01' & df$date<='2021-01-01') ~ as.Date('2021-01-01'),
                         df$date>'2021-01-01'~ as.Date('2021-12-01'))  

#Group sentiment by month and get mean
df <-df %>% 
  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(sentiment=mean(sentiment$sentiment))


#Creating binomial column to detect pre (0) and post (1) COVID data.

df$diff = ifelse(df$month < "2020-03-01",0,1)

#Mean by group
df.mean <- df %>%
  group_by(diff) %>%
  mutate(ymean = mean(sentiment))


#Plot mean by month and group differences
ggplot(df, aes(month, sentiment, fill=diff)) +
  geom_col() +
  geom_errorbar(data=df.mean, aes(month, ymax = ymean, ymin = ymean),
                size=1, linetype = "longdash", inherit.aes = F, width = 10) +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), linetype="dashed", 
             color = "#5E5E5E", size=0.7)


-------------
#Most frequent words

#Tokenizing the headlines and selecting the top 10
news_tokens <- df %>% unnest_tokens(word, title_p)
head(news_tokens, 10)

#Counting words and selecting the top 10
news_tokens_count <- news_tokens %>% count(word, sort = TRUE) %>% mutate(proportion = n / sum(n))
head(news_tokens_count, 10)

#Selecting words that appears more than 30 times
news_token_over30-news_tokens_count %>% filter(n > 30) %>% mutate(word = reorder(word, n))

#Plotting most appeared words
news_token_over30 %>%
  ggplot(aes(word, proportion*1000, fill=ceiling(proportion*1000))) +
  geom_col() + xlab(NULL) + coord_flip() + theme(legend.position = "none")
