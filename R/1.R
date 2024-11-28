library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(survminer)
library(survival)


#read and merge data
  step1 <- read.csv("step-1.csv", header=TRUE, sep=",")
  step2 <- read.csv("Step-2.csv", header=TRUE, sep=",")
  
  
  #remove ones with missing ipodate
  step1 <- step1[-which(step1$ipodate == ""), ]
  
  #leave out last digit cusip
  step1$cusipshort <- substr(step1$cusip,1,8)
  
  #merge by shortcusip
  data_a <- merge(step1,step2,by.x="cusipshort",by.y="CUSIP", all.x=TRUE)

  data_b <- merge(step1,step2,by.x="cusipshort",by.y="CUSIP")

  
#determine survivaltime, start, stop and event
  
  as.Date(data_a$ipodate)
  as.Date(data_a$ENDDAT)
  as.Date(data_b$ipodate)
  as.Date(data_b$ENDDAT)
  
  data_b$SurvTime <- interval(data_b$ipodate,data_b$ENDDAT) %/% years(1)
  
  #A few have negative SurvTime? Drop them
  data_b <- subset(data_b, data_b$SurvTime>=0)
  
  data_b$ind<-ifelse(data_b$ENDDAT=="2023-12-29",FALSE,TRUE)
  
  data_c <- distinct(data_b,cusipshort,.keep_all = TRUE)
  
  data_d <- data_b
  data_d$start <- year(make_date(data_d$fyear, 1, 1))-year(data_d$ipo)
  data_d$end <- data_d$start + 1
  data_d$ind<-ifelse(data_d$ENDDAT=="2023-12-29",FALSE,
                     ifelse(year(data_d$ENDDAT)==data_d$fyear, TRUE, FALSE))
  
  #A few have negative start? Drop them
  data_d <- subset(data_d, data_d$start>=0)

#histograms
ggplot(distinct(data_b,cusipshort,.keep_all = TRUE), aes(x=as.Date(ENDDAT))) + geom_histogram()
ggplot(distinct(data_b,cusipshort,.keep_all = TRUE), aes(x=SurvTime)) + geom_histogram()


#KM's

  #Simple
  result.km.1<- survfit(Surv(time=data_c$SurvTime, event=data_c$ind)~1, conf.type="log-log", type="kaplan-meier")
  
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

#CPH 
  
  #covariates suggested by Wei
  result.cox.1 <- coxph(Surv(data_d$start, data_d$end, data_d$ind)
                      ~  + data_d$wcap + data_d$re + data_d$ebit + data_d$lt + data_d$at,
                      id=data_d$cusip)
  summary(result.cox.1)


  #Significant covariates from model 1
  result.cox.2 <- coxph(Surv(data_d$start, data_d$end, data_d$ind)
                        ~data_d$wcap + data_d$re + data_d$ebit + data_d$lt + data_d$at,
                        id=data_d$cusip)

  summary(result.cox.2)

