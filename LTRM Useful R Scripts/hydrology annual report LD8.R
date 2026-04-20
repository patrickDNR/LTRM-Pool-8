## R code for hydrology section of FS2 annual report: Pool 8 1959 to 2024
#written by A.Carhart on 02-29-24

# discharge data downloaded from https://www.mvp-wc.usace.army.mil/data/LockDam_08.Data.shtml
# I typically save a separate .csv that has column headings of date and discharge and remove the other heading info - not sure how to do that in R. Also convert date/time to just date

#1. load required packages----
library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyverse)


#3. download discharge data from USACE website----
data_url <- "https://www.mvp-wc.usace.army.mil/data/datadownload/LockDam_08_Flow-Out_~15Minutes_best.csv"

download.file(data_url, "LockDam_08_Flow-Out_~15Minutes_best.csv")

discharge_data <- read.csv("LockDam_08_Flow-Out_~15Minutes_best.csv")
#discharge units = CFS

#remove the first 5 rows, rename columns, and split date and time
discharge_data <- discharge_data %>%
  filter(!row_number() %in% c(1:5)) %>%
  dplyr::rename('Date' = colnames(discharge_data)[1]) %>%
  dplyr::rename('discharge' = colnames(discharge_data)[2]) %>%
  mutate(Date = as.Date(format(mdy_hm(Date), '%Y-%m-%d')))


#view data file, table headings, data structure, summary, convert year and month to factor
View(discharge_data)
names(discharge_data)
str(discharge_data)
summary(discharge_data)


#add columns for year, month, day
discharge_data <- discharge_data %>% 
  mutate(year = format(ymd(Date), '%Y'))%>%
  mutate(month = format(ymd(Date), '%m'))%>%
  mutate(day = format(ymd(Date), '%d'))

discharge_data$year=as.integer(discharge_data$year)
discharge_data$month=as.integer(discharge_data$month)
discharge_data$day=as.integer(discharge_data$day)

#depending on when you download data, you may need to remove the first few months of the current year
#subset 1959-2024
discharge_data= discharge_data[which(discharge_data$year<"2025"),]

View(discharge_data)
summary(discharge_data) #check to make sure year. month, etc looks correct

#4. Table 1.1 ----
### Table 1.1  Spring flood pulse statistics by year during the LTRM period of record (1993-2024) for discharge 
#at Lock and Dam 8 of the Upper Mississippi River. Duration represents the number of days each spring when 
#discharge was above the 75th percentile from the long-term record (1959-2024). Timing represents the month 
#when the preponderance of the ten highest discharge days were observed each spring. Magnitude represents the 
#maximum discharge observed each spring.

#subset spring discharge (march-may)
discharge_spring<-discharge_data[which(discharge_data$month=="3"|discharge_data$month=="4"| discharge_data$month=="5" ),]
summary(discharge_spring)

#calculate duration - this is the total number of days above the 75% percentile in spring (p75 of spring daily discharge values)
p75_spring59_24=quantile(discharge_spring$discharge, probs = c(.75))
p75_spring59_24
#p75=80000 for 1959-2024

#subset spring 2024 - update to current year
discharge_spring2024<-discharge_spring[which(discharge_spring$year=="2024"),]
summary(discharge_spring2024)

#calculating duration- will need to update year, sum all duration values 
library(EflowStats)
#install.packages("remotes")
#remotes::install_github("USGS-R/EflowStats")

find_eventDuration(
  discharge_spring2024$discharge,
  p75_spring59_24,
  aggType = "none",
  type = "high",
  pref = "mean",
  trim = FALSE
)
#duration=2 events, 18 days total in 2024


#Rank discharge by year (Spring data only), print top 10 discharge values for current year
#record the timing (month) that had the majority of the 10 highest discharge days
timing=discharge_spring2024 %>% arrange(year, discharge) %>%
  group_by(year) %>% 
  mutate(rank = rank(-discharge,ties.method = "first"))%>% filter(rank <= 10)

View(timing)
#timing=May in 2024

#calculate magnitude for all years (maximum discharge cfs) *note we don't have units in current table 1
#subset years during ltrm POR 1993 to present- spring data
LTRM_POR_spring=discharge_spring[discharge_spring$year >= "1993", ]

magnitude_all=aggregate(LTRM_POR_spring$discharge~LTRM_POR_spring$year,FUN=max)
magnitude_all

#rename columns
names(magnitude_all)[names(magnitude_all) == "LTRM_POR_spring$year"] <- "Year"
names(magnitude_all)[names(magnitude_all) == "LTRM_POR_spring$discharge"] <- "Maximum discharge"
View(magnitude_all)
#magnitude=85700 in 2024

#convert to dataframe, save as csv
data.frame(magnitude_all)

write.csv(magnitude_all, "magnitude_93-24.csv", row.names=FALSE)

#will need to copy 'duration' and 'timing' from previous report table (1993-2023) and add in current year

#5. Figure 1.1----

#Figure 1.1 (Top panel- (A) Daily discharge at Lock and Dam 8 on the Upper Mississippi River for 2024 is represented by 
#the solid line. Mean daily discharge by day of the year for 1959-2023 is represented by the dotted line. (B) Mean discharge 
#by year is represented by the black dots. The solid line represents mean historic discharge 
#for 1959-2024. The dashed lines represent the 10th and 90th percentiles for 1959-2024 discharge. (C)
#Mean growing season discharge (May-Sept.) by year is represented by the black dots. The solid line represents 
#mean historic growing season discharge for 1959-2024. The dashed lines represent the 10th and 90th percentiles for 
#1959-2024 growing seasons.

#convert date from character to date class
discharge_data$Date=as.Date(discharge_data$Date,"%m/%d/%Y")

#convert date to julian day using yday in lubridate package
discharge_data$Julian=yday(discharge_data$Date)

#subset 2024 daily discharge
discharge2024<-discharge_data[which(discharge_data$year=="2024"),]
summary(discharge2024)
str(discharge2024)

Daily_discharge_plot=ggplot(data=discharge2024,aes(x=as.Date(Julian,origin=as.Date("2024-01-01")),y=discharge))+geom_line()+labs(x="", y="Daily Discharge (ft3/sec)")+theme_classic()+theme(panel.border = element_rect(color="black", fill=NA, linewidth=1))+scale_x_date(date_labels = "%b")       
Daily_discharge_plot

#daily discharge (1959-2023)
#remove 2024 data
discharge1959to2023=discharge_data[discharge_data$year <= "2023", ]
summary(discharge1959to2023)
View(discharge1959to2023)


##calculate mean and percentiles of daily discharge 1959-2023
daily.sum59_23 <- discharge1959to2023 %>% 
  group_by(Julian) %>%  # group by  Julian day
  dplyr::summarize(mean_Q59_23=mean(discharge, na.rm=TRUE), 
                                    `0th Percentile` = quantile(discharge, probs = 0, na.rm = TRUE),
                                    `25th Percentile` = quantile(discharge, probs = 0.25, na.rm = TRUE),
                                    `50th Percentile` = quantile(discharge, probs = 0.5, na.rm = TRUE),
                                    `75th Percentile` = quantile(discharge, probs = 0.75, na.rm = TRUE),
                                    `100th Percentile` = quantile(discharge, probs = 1, na.rm = TRUE)
                                  
  )

str(daily.sum59_23) #structure of output
head(daily.sum59_23) #first 5 rows of data

#merge by julian day to simplfy plotting
daily_Q_summary=merge(x=discharge2024,y=daily.sum59_23,by="Julian")
head(daily_Q_summary)
str(daily_Q_summary)
View(daily_Q_summary)

#plot mean daily discharge 1959-2023 and 2024 daily discharge FIG 1.1a
fig1.1a=ggplot(data=daily_Q_summary)+
  geom_line(aes(x=Date,y=discharge,color="2024 Daily Discharge"))+
  geom_line(aes(x=Date,y=mean_Q59_23,color="Mean Daily Discharge 1959-2023",linetype="dashed"),linetype="dashed",linewidth=1)+
  labs(x="", y=bquote("Daily Discharge  ft"^3/sec),title="A")+
  theme_classic()+theme(panel.border = element_rect(color="black", fill=NA, linewidth=1),legend.position="bottom",legend.key.size=unit(3,"lines"),plot.title.position="plot",plot.title = element_text(hjust=1,vjust=1),legend.margin = margin(0,0,0,0),legend.box.margin=margin(-21,-21,-21,-21))+
  scale_x_date(date_labels = "%b",date_breaks="2 months")+scale_color_manual(name="",breaks=c('2024 Daily Discharge','Mean Daily Discharge 1959-2023'),values=c('2024 Daily Discharge'='black','Mean Daily Discharge 1959-2023'='black'))+
  guides(color=guide_legend(override.aes=list(linetype=c("solid","dashed"))))
fig1.1a

# Alternative- plotting percentile ranges fig 1,1 A
fig1.1a=ggplot(data=daily_Q_summary)+
  geom_ribbon(aes(x = Date, ymin= `0th Percentile`, ymax=`25th Percentile`), fill = "#85D54AFF", color="#2BB07FFF",alpha=0.5) +
  geom_ribbon(aes(x = Date, ymin = `25th Percentile`, ymax=`75th Percentile`), fill = "#25858EFF", color="#2D708EFF",alpha=0.5) +
  geom_ribbon(aes(x = Date, ymin = `75th Percentile`, ymax=`100th Percentile`), color="#440154FF",fill = "#440154FF",alpha=0.5) +
  geom_line(aes(x=Date,y=discharge,color="2024 Daily Discharge"), size=1)+
  geom_line(aes(x = Date, y = `50th Percentile`,color="Mean Daily Discharge 1959-2023"), size = 1, linetype="dashed") +
  labs(x="", y=bquote("Daily Discharge  ft"^3/sec),title="A")+
  theme_classic()+theme(panel.border = element_rect(color="black", fill=NA, linewidth=1),legend.position="bottom",legend.key.size=unit(3,"lines"),plot.title.position="plot",plot.title = element_text(hjust=1,vjust=1),legend.margin = margin(0,0,0,0),legend.box.margin=margin(-21,-21,-21,-21),axis.text = element_text(size=10,color="black"))+
  scale_x_date(date_labels = "%b",date_breaks="2 months")+scale_color_manual(name="",breaks=c('2024 Daily Discharge','Mean Daily Discharge 1959-2023'),values=c('2024 Daily Discharge'='black','Mean Daily Discharge 1959-2023'='black'))+
  scale_y_continuous(labels=comma, breaks=c(0,50000,100000,150000,200000,250000))
fig1.1a

###middle panel fig 1.1b
#calculate mean historic discharge from 1959-2024 (constant value)
mean_historic=mean(discharge_data$discharge)
mean_historic
#mean=39596.12

#mean annual Q 1959-2024
mean_annual_59_24=aggregate(discharge_data$discharge~discharge_data$year,FUN=mean)
mean_annual_59_24
summary(mean_annual_59_24)

names(mean_annual_59_24)[names(mean_annual_59_24) == "discharge_data$year"] <- "Year"
names(mean_annual_59_24)[names(mean_annual_59_24) == "discharge_data$discharge"] <- "mean_discharge"

# 10th/90th of annual means from 1959-2024
percent_10_59_24=quantile(mean_annual_59_24$mean_discharge, probs = .10)
percent_10_59_24
#10%=22838.72

percent_90_59_24=quantile(mean_annual_59_24$mean_discharge, probs = .90)
percent_90_59_24
#90%=54065.62


#plot middle panel Fig 1.1b
fig1.1b=ggplot()+geom_point(data=mean_annual_59_24,aes(x=Year, y=mean_discharge,color="Mean Discharge"))+
  geom_hline(aes(yintercept = mean_historic,color="Mean Historic Discharge"),linewidth=1)+
  geom_hline(aes(yintercept = percent_10_59_24,color="10th and 90th Percentiles"),linetype="dashed")+
  geom_hline(aes(yintercept=percent_90_59_24,color="10th and 90th Percentiles"),linetype="dashed")+
  labs(x="", y=bquote("Mean Discharge  ft"^3/sec),title="B")+theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA, linewidth=1),axis.text.x = element_text(angle = 45,vjust=1,hjust=1),axis.text = element_text(size=10,color="black"),legend.title = element_blank(),legend.position = "bottom",legend.key.size=unit(3,"lines"),plot.title.position="plot",plot.title = element_text(hjust=1,vjust=1),legend.margin = margin(0,0,0,0),legend.box.margin=margin(-21,-21,-21,-21))+
  scale_x_continuous(limits=c(1993,2024),breaks=seq(1993,2024,by=2))+
  scale_y_continuous(limits=c(0,90000),labels=comma,breaks=c(0,20000,40000,60000,80000))+
  scale_color_manual(name="",breaks=c('Mean Discharge','Mean Historic Discharge','10th and 90th Percentiles'),values=c('Mean Discharge'='black','Mean Historic Discharge'='black','10th and 90th Percentiles'='black'))+
  guides(color=guide_legend(override.aes=list(linetype=c("blank","solid","dashed"),shape=c(16,32,32))))
fig1.1b


###bottom panel fig 1.1c
LTRM_POR_all=discharge_data[discharge_data$year >= "1993", ]

#subset growing season may-sept
discharge_GS<-discharge_data[which(discharge_data$month=="5"|discharge_data$month=="6"| discharge_data$month=="7" | discharge_data$month=="8" | discharge_data$month=="9" ),]
summary(discharge_GS)

#calculate mean historic discharge from 1959-2024 during the growing season (constant value, updated annually)
mean_historic_GS=mean(discharge_GS$discharge)
mean_historic_GS
#mean=44735.94

#subset growing season from LTRM POR 
discharge_GS_LTRM_POR<-LTRM_POR_all[which(LTRM_POR_all$month=="5"|LTRM_POR_all$month=="6"| LTRM_POR_all$month=="7" | LTRM_POR_all$month=="8" | LTRM_POR_all$month=="9" ),]
summary(discharge_GS_LTRM_POR)


#mean annual Q 1959-2024 during GS
mean_annual_59_24_GS=aggregate(discharge_GS$discharge~discharge_GS$year,FUN=mean)
mean_annual_59_24_GS
summary(mean_annual_59_24_GS)

#rename columns
names(mean_annual_59_24_GS)[names(mean_annual_59_24_GS) == "discharge_GS$year"] <- "Year"
names(mean_annual_59_24_GS)[names(mean_annual_59_24_GS) == "discharge_GS$discharge"] <- "mean_discharge"


#calculate the 10th and 90th percentiles of growing season means 1959-2024
percent_10_59_24_GS=quantile(mean_annual_59_24_GS$mean_discharge, probs = .10)
percent_10_59_24_GS
#10%=24437.81

percent_90_59_24_GS=quantile(mean_annual_59_24_GS$mean_discharge, probs = .90)
percent_90_59_24_GS
#90%=66794.12

#plot bottom panel fig 1.1c
fig1.1c=ggplot()+geom_point(data=mean_annual_59_24_GS,aes(x=Year, y=mean_discharge,color="Mean Growing Season Discharge"))+
  geom_hline(aes(yintercept = mean_historic_GS,color="Mean Historic Growing Season Discharge"),linewidth=1)+
  geom_hline(aes(yintercept = percent_10_59_24_GS,color="10th and 90th Percentiles"),linetype="dashed")+
  geom_hline(aes(yintercept=percent_90_59_24_GS,color="10th and 90th Percentiles"),linetype="dashed")+
  labs(x="", y=bquote("Mean Discharge  ft"^3/sec),title="C")+theme_classic()+
  theme(panel.border = element_rect(color="black", fill=NA, linewidth=1),axis.text.x = element_text(angle = 45,vjust=1,hjust=1),axis.text = element_text(size=10,color="black"),legend.title = element_blank(),legend.position = "bottom",legend.key.size=unit(3,"lines"),plot.title.position="plot",plot.title = element_text(hjust=1,vjust=1),legend.margin = margin(0,0,0,0),legend.box.margin=margin(-21,-21,-21,-21))+
  scale_x_continuous(limits=c(1993,2024),breaks=seq(1993,2024,by=2))+
  scale_y_continuous(limits=c(0,100000),labels=comma,breaks=c(0,20000,40000,60000,80000,100000))+scale_color_manual(name="",breaks=c('Mean Growing Season Discharge','Mean Historic Growing Season Discharge','10th and 90th Percentiles'),values=c('Mean Growing Season Discharge'='black','Mean Historic Growing Season Discharge'='black','10th and 90th Percentiles'='black'))+
  guides(color=guide_legend(override.aes=list(linetype=c("blank","solid","dashed"),shape=c(16,32,32))))
fig1.1c



#combine panels a-c

library(ggpubr)
figure1.1 <- ggarrange(fig1.1a,fig1.1b,fig1.1c,ncol=1,nrow=3)
figure1.1

ggsave("Figure1.1v2.tif")

#Rmarkdown file----
library(rmarkdown)
render("hydrology annual report 1959-2024 LD8.R", "pdf_document", clean=TRUE)
