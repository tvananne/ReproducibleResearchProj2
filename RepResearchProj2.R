
#Reproducible Research - Project 2

#my file location / wd
    setwd("C:/Users/tjvan/Documents/Coursera/ReproducibleResearch/Project2/project2git")
    
    require("dplyr")
    require("ggplot2")
    require("reshape")
    require("lubridate")
    require("stringdist")
    library(dplyr)
    library(ggplot2)
    library(reshape)
    library(lubridate)
    library(stringdist)
  
rawdata <- read.csv("../data/repdata-data-StormData.csv.bz2", stringsAsFactors=FALSE)
    
#Easy way to reset environment without having to re-read the data (it takes a long time)
    rm(list=setdiff(ls(), c("rawdata", "rawdata.flt")))

    #Make some real date fields, then take a sample of data
    rawdata$DATE <- parse_date_time(rawdata$BGN_DATE, orders = "mdy hms") 
    rawdata$YEAR <- year(rawdata$DATE)
    
    #clean EVTYPE
    clean_EVTYPEs <- read.csv("clean_EVTYPEs.csv", stringsAsFactors = FALSE)
    clean_EVTYPEs <- toupper(clean_EVTYPEs$Clean_EVTYPEs)
    dirty_EVTYPEs <- rawdata$EVTYPE
    EVTYPE_Indeces <- amatch(dirty_EVTYPEs, clean_EVTYPEs, maxDist=23)
    clean_Events <- clean_EVTYPEs[EVTYPE_Indeces]
    rawdata$Clean_Event <- clean_Events
    rawdata_clean_nas <- filter(rawdata, is.na(Clean_Event))
    testrawdata <- as.data.frame(rawdata$EVTYPE); testrawdata$clean <- rawdata$Clean_Event
    

#Question 1: Across the United States, which types of events (as indicated in 
#the EVTYPE variable) are most harmful with respect to population health?
    
    rawdata$EVTYPE <- as.factor(rawdata$EVTYPE)
    
    #group by event type and year
    rawdata.evt.grp <- group_by(rawdata, Clean_Event, YEAR)  #this can be used for both Q1 and Q2
    rawdata.evt.summ <- summarize(rawdata.evt.grp, 'sum_fatalities'=sum(FATALITIES, na.rm = TRUE), 
                                   'sum_injuries'=sum(INJURIES, na.rm = TRUE))
    
    rawdata.evt.summ
    class(rawdata.evt.summ)
    #need to convert that to a normal df now so we can regroup and average across years
    rawdata.evt.summ
    rawdata.evt.summ <- as.data.frame(rawdata.evt.summ)
    #now group ONLY by event type
    rawdata.evt.summ.grp <- group_by(rawdata.evt.summ, Clean_Event)
    
    #this will collapse the sums for each year into averages across all years
    rawdata.avg <- summarize(rawdata.evt.summ.grp, 'avg_fatalities'=mean(sum_fatalities), 
                             'avg_injuries'=mean(sum_injuries))
    
    rawdata.avg #this is much more accurate than a sum of all EVTYPE
    
    
    #highest fatalities
    rawdata.avg.flt <- filter(rawdata.avg, avg_fatalities > 10)
    topFatalities <- arrange(rawdata.avg.flt, desc(avg_fatalities))
    topFatalities
    
    top5FatalEvt <- topFatalities[1:5, ]
    colnames(top5FatalEvt) <- c("Event_Type", "Avg_Fatalities_per_Year", "Avg_Injuries_per_Year")
    top5FatalEvt
    
    ggplot(top5FatalEvt, aes(x = reorder(Event_Type, -Avg_Fatalities_per_Year), y = Avg_Fatalities_per_Year)) + 
        geom_bar(stat = "identity") +
        xlab("Event Type") +
        ylab("Average Fatalities per Year") +
        ggtitle("Event Types with Most Fatalities") +
        geom_text(aes(label = Avg_Fatalities_per_Year), vjust = -0.5)
    
    
    #highest injuries
    rawdata.avg.flt2 <- filter(rawdata.avg, avg_injuries > 10)
    topInjuries <- arrange(rawdata.avg.flt2, desc(avg_injuries))
    topInjuries
    top5InjuryEvt <- topInjuries[1:5, ]
    top5InjuryEvt
    colnames(top5InjuryEvt) <- c("Event_Type", "Avg_Fatalities_per_Year", "Avg_Injuries_per_Year")
    top5InjuryEvt
    
    ggplot(top5InjuryEvt, aes(x = reorder(Event_Type, -Avg_Injuries_per_Year), y = Avg_Injuries_per_Year)) + 
        geom_bar(stat = "identity") +
        xlab("Event Type") +
        ylab("Average Injuries per Year") +
        ggtitle("Event Types with Most Injuries") +
        geom_text(aes(label = Avg_Injuries_per_Year), vjust = -0.5)
    
    
#Question 2 
    rm(list=setdiff(ls(), c("rawdata", "rawdata.flt")))
    
    #We only care about these fields really
    rawdata$PROPDMGEXP <- toupper(rawdata$PROPDMGEXP) 
    rawdata.flt <- filter(rawdata, PROPDMGEXP == "K" | PROPDMGEXP == "B" | PROPDMGEXP == "M")
    
    
    rawdata.flt$PropertyDamage <- 0
    #for (i in 1:nrow(rawdata.flt)) {
     #   print(i)
      #  if (rawdata.flt$PROPDMGEXP[i] == "K") {
       #     rawdata.flt$PropertyDamage[i] <- rawdata.flt$PROPDMG[i] * 1000
        #} else if (rawdata.flt$PROPDMGEXP[i] == "M") {
         #   rawdata.flt$PropertyDamage[i] <- rawdata.flt$PROPDMG[i] * 1000000
        #} else if (rawdata.flt$PROPDMGEXP[i] == "B") {
        #    rawdata.flt$PropertyDamage[i] <- rawdata.flt$PROPDMG[i] * 1000000000
        #} 
    #}
    

    rawdata.flt.grpby <- group_by(rawdata.flt, Clean_Event, YEAR)
    rawdata.flt.summ <- summarize(rawdata.flt.grpby, 'sum_propertydmg'=sum(PropertyDamage))
    
    
    rawdata.flt.summ <- as.data.frame(rawdata.flt.summ)
    rawdata.flt.grpby2 <- group_by(rawdata.flt.summ, Clean_Event)
    rawdata.flt.summ2 <- summarize(rawdata.flt.grpby2, 'avg_propertydmg'=mean(sum_propertydmg))
    rawdata.flt.summ2.arrg <- arrange(rawdata.flt.summ2, desc(avg_propertydmg))
    
    top5PropertyDmg <- rawdata.flt.summ2.arrg[1:5, ]
    top5PropertyDmg <- as.data.frame(top5PropertyDmg)
    
    for (i in 1:nrow(top5PropertyDmg)) {
        top5PropertyDmg$Dollar[i] <- paste("$",format(top5PropertyDmg[i,2], big.mark=","),sep="")  
    }
    
    
    
    
    top5PropertyDmg
    
    ggplot(top5PropertyDmg, aes(x = reorder(Clean_Event, -avg_propertydmg), y = avg_propertydmg)) + 
        geom_bar(stat = "identity") +
        xlab("Event Type") +
        ylab("Average Property Damage per Year") +
        ggtitle("Event Types with the Highest Average Annual Property Damage") +
        geom_text(aes(label = Dollar), vjust = -0.5)
    
    
        
    
    
    
    
    
    