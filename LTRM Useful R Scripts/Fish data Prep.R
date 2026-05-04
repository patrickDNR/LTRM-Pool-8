#Use LTRM fish data to summarize fyke and e-fishing data -- this will 
#pull sampling data to get CPUE, species richness, and diversity indices

#load required packages
library(lubridate)
library(tidyverse)

#load data from gitHub - updated data file 30 Apr 2026 - should be complete through 2025
data_url <- "https://raw.githubusercontent.com/patrickDNR/LTRM-Pool-8/refs/heads/main/LTRM%20data/ltrm_fish_data.csv"
download.file(data_url, 'ltrm_fish_data.csv')

#load data into data frame
fish <- read.csv('ltrm_fish_data.csv')

#########Diversity indices per sample and by different gear types #########
#Starting with large fyke nets - filter data using gear type "F"
#Also want to specifiy "summary" code - 5 as normally completed sample, 
# 7 includes psuedo shoreline
# 8 is minor gear damage but not critical and didn't effect sample
fyke <- fish %>%
  filter(gear == 'F') %>%
  filter(summary == 5 | summary == 8 | summary == 7)

#make a vector of unique barcodes (i.e. sample)
fyke.codes <- unique(fyke$barcode)

#get some summary stats per code 
#loops through the unique samples to pull quantity data (i.e. number of fish per species per sample)
fyke.stats <- c()
for(i in 1:length(fyke.codes)){
  
  test <- fyke %>%
    dplyr::filter(barcode == fyke.codes[i])

  test.species <- tapply(test$length,test$fishcode, quantile, na.rm = T)
  #make a data frame of info

  fish.names <- names(test.species)
  quant.data <- as.data.frame(bind_rows(test.species))

  #how many fish per group in that sample?
  nums <- tapply(test$length, test$fishcode, length)

  quant.data <- cbind(fish.names, quant.data, nums, code = test$barcode[1], 
                    sdate = test$sdate[1], stime = test$stime[1], fdate = test$fdate[1],
                    ftime = test$ftime[1],
                    gear = test$gear[1], zone15e = test$zone15e[1], 
                    zone15n = test$zone15n[1], temp = test$temp[1], 
                    depth = test$depth[1], do = test$do[1], 
                    vegd = test$vegd[1], summary = test$summary[1])
  fyke.stats <- rbind(fyke.stats, quant.data)
}

#Now for each of these, get summary data on diversity - species richness
#and Shannon index.
codes <- unique(fyke.stats$code)

#Now use previously created data frame to calculate species richness (i.e. number of species present)
# and shannon diversity index per sample
fyke.sum <- c()
for(i in 1:length(codes)){
  codei <- fyke.stats %>%
    filter(code == codes[i])
  
  sp.rich <- nrow(codei)
  sp.prop <- codei$nums/sum(codei$nums, na.rm = T)
  
  shannon = -sum(sp.prop * log(sp.prop))
  
  x <- cbind(codei[1,8:ncol(codei)], richness = sp.rich, shannon = shannon)
  fyke.sum <- rbind(fyke.sum, x)
}

#Add "effort" for both - calculate the number of days the net was set
fyke.stats$sDateTime <- mdy_hm(paste(fyke.stats$sdate, fyke.stats$stime, sep = ' '))
fyke.stats$fDateTime <- mdy_hm(paste(fyke.stats$fdate, fyke.stats$ftime, sep = ' '))

fyke.stats$effort.days <- difftime(fyke.stats$fDateTime, fyke.stats$sDateTime, 
                                  units = 'days')

#Calculate Catch per unit effort (CPUE) as number caught/effort
fyke.stats$CPUE_num.day <- fyke.stats$nums/as.numeric(fyke.stats$effort.days)

#Now do the same procedure for mini-fykes (code "M") - also summary code 5, 7, 8
mini <- fish %>%
  filter(gear == 'M') %>%
  filter(summary == 5 | summary == 8 | summary == 7)

mini.codes <- unique(mini$barcode)

#get some summary stats per code
mini.stats <- c()
for(i in 1:length(mini.codes)){
  
  test <- mini %>%
    dplyr::filter(barcode == mini.codes[i])
  
  test.species <- tapply(test$length,test$fishcode, quantile, na.rm = T)
  #make a data frame of info
  
  fish.names <- names(test.species)
  quant.data <- as.data.frame(bind_rows(test.species))
  
  #how many fish per group in that sample?
  nums <- tapply(test$length, test$fishcode, length)
  
  quant.data <- cbind(fish.names, quant.data, nums, code = test$barcode[1], 
                      sdate = test$sdate[1], stime = test$stime[1], fdate = test$fdate[1],
                      ftime = test$ftime[1],
                      gear = test$gear[1], zone15e = test$zone15e[1], 
                      zone15n = test$zone15n[1], temp = test$temp[1], 
                      depth = test$depth[1], do = test$do[1], 
                      vegd = test$vegd[1])
  mini.stats <- rbind(mini.stats, quant.data)
}

#Now for each of these, get summary data on diversity - species richness
#and Shannon index

codes <- unique(mini.stats$code)

#make a data frame for these 
mini.sum <- c()
for(i in 1:length(codes)){
  codei <- mini.stats %>%
    filter(code == codes[i])
  
  sp.rich <- nrow(codei)
  sp.prop <- codei$nums/sum(codei$nums, na.rm = T)
  
  shannon = -sum(sp.prop * log(sp.prop))
  
  x <- cbind(codei[1,8:ncol(codei)], richness = sp.rich, shannon = shannon)
  mini.sum <- rbind(mini.sum, x)
}
#Add "effort" for both
mini.stats$fDateTime <- mdy_hm(paste(mini.stats$fdate, mini.stats$ftime, sep = ' '))
mini.stats$sDateTime <- mdy_hm(paste(mini.stats$sdate, mini.stats$stime, sep = ' '))

mini.stats$effort.days <- difftime(mini.stats$fDateTime, mini.stats$sDateTime, 
                                   units = 'days')

#Calculate CPUE
mini.stats$CPUE_num.day <- mini.stats$nums/as.numeric(mini.stats$effort.days)

#add year to data
mini.stats$year <- format(mdy(mini.stats$sdate), '%Y')
fyke.stats$year <- format(mdy(fyke.stats$sdate), '%Y')

mini.sum$year <- format(mdy(mini.sum$sdate), '%Y')
fyke.sum$year <- format(mdy(fyke.sum$sdate), '%Y')

#Need to do the same thing for daytime electrofishing - gear code "D"
buzz <- fish %>%
  filter(gear == 'D') %>%
  filter(summary == 5 | summary == 8 | summary == 7)

buzz.codes <- unique(buzz$barcode)

#get some summary stats per code
buzz.stats <- c()
for(i in 1:length(buzz.codes)){
  
  test <- buzz %>%
    dplyr::filter(barcode == buzz.codes[i])
  
  test.species <- tapply(test$length,test$fishcode, quantile, na.rm = T)
  #make a data frame of info
  
  fish.names <- names(test.species)
  quant.data <- as.data.frame(bind_rows(test.species))
  
  #how many fish per group in that sample?
  nums <- tapply(test$length, test$fishcode, length)
  
  quant.data <- cbind(fish.names, quant.data, nums, code = test$barcode[1], 
                      sdate = test$sdate[1], stime = test$stime[1], fdate = test$fdate[1],
                      ftime = test$ftime[1],
                      gear = test$gear[1], zone15e = test$zone15e[1], 
                      zone15n = test$zone15n[1],effmin = test$effmin[1], temp = test$temp[1], 
                      depth = test$depth[1], do = test$do[1], 
                      vegd = test$vegd[1])
  buzz.stats <- rbind(buzz.stats, quant.data)
}

#Now for each of these, get summary data on diversity - species richness
#and Shannon index
#Do this with a for loop and whatnot...
codes <- unique(buzz.stats$code)

#make a data frame for these 
buzz.sum <- c()
for(i in 1:length(codes)){
  codei <- buzz.stats %>%
    filter(code == codes[i])
  
  sp.rich <- nrow(codei)
  sp.prop <- codei$nums/sum(codei$nums, na.rm = T)
  
  shannon = -sum(sp.prop * log(sp.prop))
  
  x <- cbind(codei[1,8:ncol(codei)], richness = sp.rich, shannon = shannon)
  buzz.sum <- rbind(buzz.sum, x)
}

#Calculate CPUE
buzz.stats$CPUE_num.min <- buzz.stats$nums/as.numeric(buzz.stats$effmin)

#add year to data
buzz.stats$year <- format(mdy(buzz.stats$sdate), '%Y')
buzz.sum$year <- format(mdy(buzz.sum$sdate),'%Y')


#Final data sets...
#fyke net abundance and CPUE
head(fyke.stats)
