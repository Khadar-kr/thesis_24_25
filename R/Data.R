library(dplyr)

step1 <- read.csv("step-1.csv", header=TRUE, sep=",")
step2 <- read.csv("Step-2.csv", header=TRUE, sep=",")



step1$cusipshort <- substr(step1$cusip,1,8)

data_a <- merge(step1,step2,by.x="cusipshort",by.y="CUSIP", all.x=TRUE)
data_b <- merge(step1,step2,by.x="cusipshort",by.y="CUSIP")


sum(is.na(data$ENDDAT))

n_distinct(data$cusipshort)

which(step1$gvkey == 179637, arr.ind=TRUE)

step1[447849,]
