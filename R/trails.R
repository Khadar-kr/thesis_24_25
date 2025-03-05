library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(survminer)
library(survival)

data <- read_csv("Acedemic/Masters/KULEUVEN/Thesis/Margaux/data_with_all_columns.csv")

as.Date(data$ipodate)
as.Date(data$ENDDAT)
as.Date(data$ipodate)
as.Date(data$ENDDAT)


data <- data[-which(data$ipodate == ""), ]

data$SurvTime <- interval(data$ipodate,data$ENDDAT) %/% years(1)
#A few have negative SurvTime? Drop them
data <- subset(data, data$SurvTime>=0)

data$ind<-ifelse(data$ENDDAT=="2023-12-29",FALSE,TRUE)
data.colnames()
colnames(data)
data$cusipshort <- substr(data$cusip,1,8)
#histograms
ggplot(distinct(data,cusipshort,.keep_all = TRUE), aes(x=as.Date(ENDDAT))) + geom_histogram()
ggplot(distinct(data,cusipshort,.keep_all = TRUE), aes(x=SurvTime)) + geom_histogram()


#KM's

#Simple
result.km.1<- survfit(Surv(time=data$SurvTime, event=data$ind)~1, conf.type="log-log", type="kaplan-meier")

summary(result.km.1)
ggsurvplot(result.km.1, data=data_c, conf.int=TRUE)

#Finance vs Industry KM
result.km.2<- survfit(Surv(time=data_c$SurvTime, event=data_c$ind)~data_c$indfmt, conf.type="log-log", type="kaplan-meier")

summary(result.km.2)
ggsurvplot(result.km.2, data=data_c, conf.int=TRUE)

#HEXCD KM
result.km.3<- survfit(Surv(time=data_c$SurvTime, event=data_c$ind)~data_c$HEXCD, conf.type="log-log", type="kaplan-meier")

summary(result.km.3)
ggsurvplot(result.km.3, data=data_c, conf.int=TRUE)

table(data_c$HEXCD)