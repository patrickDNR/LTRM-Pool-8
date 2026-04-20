#Use LTRM fish data to summarize fyke and e-fishing data -- this will 
#pull sampling data to get CPUE, species richness, and diversity indices

library(lubridate)
library(tidyverse)

#load data
fish <- read.csv('Data/ltrm_fish_data.csv')

#Test out coming up with diversity per sample...
#get some stats per gear type...
fyke <- fish %>%
  filter(gear == 'F') %>%
  filter(summary == 5 | summary == 8 | summary == 7)

fyke.codes <- unique(fyke$barcode)

#get some summary stats per code
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
#and Shannon index
#Do this with a for loop and whatnot...
codes <- unique(fyke.stats$code)

#make a data frame for these 
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
#Add "effort" for both
fyke.stats$sDateTime <- mdy_hm(paste(fyke.stats$sdate, fyke.stats$stime, sep = ' '))
fyke.stats$fDateTime <- mdy_hm(paste(fyke.stats$fdate, fyke.stats$ftime, sep = ' '))

fyke.stats$effort.days <- difftime(fyke.stats$fDateTime, fyke.stats$sDateTime, 
                                  units = 'days')

#Calculate CPUE
fyke.stats$CPUE_num.day <- fyke.stats$nums/as.numeric(fyke.stats$effort.days)

#Now do the ame thing for mini-fykes
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
#Do this with a for loop and whatnot...
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

#Need to do the same thing for daytime electrofishing
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

#load conversion table
conv <- read.csv('Data/fish abbrev convert.csv')

#combine with stat tables
fyke.stats.trim <- left_join(fyke.stats, conv, by = c('fish.names' = 'Abbr')) %>%
  select(Fishname, sdate, gear, zone15e, zone15n, temp, depth, do, vegd, CPUE_num.day) %>%
  rename('CPUE' = 'CPUE_num.day') %>%
  mutate(lab = 'CPUE (num/day)')
mini.stats.trim <- left_join(mini.stats, conv, by = c('fish.names' = 'Abbr'))%>%
  select(Fishname, sdate, gear, zone15e, zone15n, temp, depth, do, vegd, CPUE_num.day) %>%
  rename('CPUE' = 'CPUE_num.day') %>%
  mutate(lab = 'CPUE (num/day)')
buzz.stats.trim <- left_join(buzz.stats, conv, by = c('fish.names' = 'Abbr'))%>%
  select(Fishname, sdate, gear, zone15e, zone15n, temp, depth, do, vegd, CPUE_num.min) %>%
  rename('CPUE' = 'CPUE_num.min') %>%
  mutate(lab = 'CPUE (num/min)')

#combine all stats together
stats.all <- rbind(fyke.stats.trim, mini.stats.trim, buzz.stats.trim)

#save these files somewhere
write.csv(stats.all, 'Data/CPUE_all.csv')
write.csv(fyke.stats, 'Data/Fyke_length_nums.csv')
write.csv(fyke.sum, 'data/fyke_summary.csv')
write.csv(mini.stats, 'data/miniFyke_length_nums.csv')
write.csv(mini.sum, 'data/miniFyke_summary.csv')
write.csv(buzz.stats, 'data/shocking_length_nums.csv')
write.csv(buzz.sum, 'data/shocking_summary.csv')
