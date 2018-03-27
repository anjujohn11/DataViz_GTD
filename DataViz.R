#------------------------------------------------------------------------------------------------------------------#
# Name        :Anju John
# Student#    :R00052504
# Class       :Msc Data Science & Analytics-Data Visualization
# Description :Global Terrorism Data set is used here to evaluvate various visualization techniques.
#------------------------------------------------------------------------------------------------------------------#
library(tidyverse) 
library(ggplot2)      # Data visualization
library(readr)        # Read_csv function 
library(dplyr)
library(gridExtra) 
library(sp)           # spatial data classes and functions
library(maptools)
library(RColorBrewer)
library(maps)         # powerful and flexible mapping package
library(mapdata)
library(ggmap) 
library(treemap)
library(leaflet)
library(purrr)
#------------------------------------------------------------------------------------------------------------------#
#Load database into R environment
#------------------------------------------------------------------------------------------------------------------#
gtd = read.csv(file="globalterrorismdb_0617dist.csv", header=TRUE, sep=",")  
#------------------------------------------------------------------------------------------------------------------#
# Data Cleaning and Preparation
#------------------------------------------------------------------------------------------------------------------#
# Create a dataframe with same structure for the world, for reference purpose
gtd <- gtd %>%
  dplyr::select(eventid, iyear, imonth, iday, extended, summary, doubtterr, multiple, related, country_txt, region_txt, provstate, latitude, longitude, attacktype1_txt, attacktype2_txt, attacktype3_txt, success, suicide, weaptype1_txt, weaptype2_txt, weaptype3_txt, weaptype4_txt, target1, targtype1_txt, natlty1_txt, target2, targtype2_txt, natlty2_txt, target3, targtype3_txt,natlty3_txt, gname, gname2, gname3, guncertain1, guncertain2, guncertain3, nperps, nperpcap, claimed, compclaim, motive, nkill, nkillter, nwound, nwoundte, property, propextent, ishostkid, nhostkid, INT_LOG, INT_IDEO, INT_MISC, INT_ANY) %>%
  filter(doubtterr == 0)
# Change variables with strings back to character vector
gtd$summary <- as.character(gtd$summary)
gtd$target1 <- as.character(gtd$target1)
gtd$target2 <- as.character(gtd$target2)
gtd$target3 <- as.character(gtd$target3)
# Trim factor variables to only have included levels
#gtd <- gtd %>% dmap_if(is.factor, fct_drop)
# Recode -9 and -99 as NA in the dataframe
gtd[gtd == -9 | gtd == -99] <- NA
# Recode factors "." and "Unknown" into NA in the factors
for (i in 1:ncol(gtd)){
  if (is.factor(gtd[,i])){
    levels(gtd[,i]) <- sub("^.$", NA, levels(gtd[,i]))
    levels(gtd[,i]) <- sub("Unknown", NA, levels(gtd[,i]))
  }
}

# Replace unknown days (0 according to the code book) with 1
gtd$iday <- as.integer(gsub(0, 1, gtd$iday))
# Create a new variable "idate"
gtd$idate <- as.Date(paste0(gtd$iyear,
                            stringr::str_pad(as.character(gtd$imonth), width = 2, side = "left", pad = "0"),
                            stringr::str_pad(as.character(gtd$iday), width = 2, side = "left", pad = "0")), "%Y%m%d")
# There are 23 incidents where the date object fails to show
# a closer look finds that these are incidents where iday = 31
# when the month actually won't have 31 days.
# We will treat these idays as 30, i.e. end of the month
gtd$iday[is.na(gtd$idate)] <- 30
gtd$idate <- as.Date(paste0(gtd$iyear,
                            stringr::str_pad(as.character(gtd$imonth), width = 2, side = "left", pad = "0"),
                            stringr::str_pad(as.character(gtd$iday), width = 2, side = "left", pad = "0")), "%Y%m%d")


# subset gtd data to attacks in Turkey  and Europe
gtd.turkey <- gtd%>%   filter(country_txt == "Turkey")
gtd.Europe <- gtd%>%   filter(region_txt %in% c("Western Europe","Eastern Europe"))
glimpse(gtd.turkey) # Summary of turkey.gtd structure 
#------------------------------------------------------------------------------------------------------------------#
# Area Plot
#------------------------------------------------------------------------------------------------------------------#

#Terrorist Attacks, 1970-2016 (Global vs Turkey)
plot1<-ggplot(data=gtd) +   
  geom_area(mapping=aes(x=iyear), stat="count", fill= ('#e67e22')) +   
  ggtitle("Yearly Terrorist Attacks, 1970-2016", subtitle = "Source: Global Terrorism Database") 
plot2<-ggplot(data=gtd.turkey) +   
  geom_area(mapping=aes(x=iyear), stat="count", fill= ('#e67e22')) +   
  ggtitle("Yearly Terrorist Attacks, 1970-2016", subtitle = "Turkey") 
plot3<-ggplot(data=gtd.Europe) +   
  geom_area(mapping=aes(x=iyear), stat="count", fill= ('#e67e22')) +   
  ggtitle("Yearly Terrorist Attacks, 1970-2016", subtitle = "Europe") 

windows(6,6)
grid.arrange(plot1,plot2,plot3,ncol=2)
#------------------------------------------------------------------------------------------------------------------#
#Histogram
#------------------------------------------------------------------------------------------------------------------#

#Histogram of Attacks by Year 
plot4 = ggplot(gtd.turkey, aes(gtd.turkey$iyear)) +   
        geom_histogram(fill= ('#e67e22'), color='black', binwidth=1) +   
        scale_x_continuous(limit=c(1970, 2017), breaks=seq(1970, 2017, by = 5)) +   
        labs(x= 'year', y= 'number of events') +   
        ggtitle('Histogram of Attacks by Year')
plot4
#------------------------------------------------------------------------------------------------------------------#

#Histogram of Attacks by Month # 
#Data Preparation 
gtd.turkey.month<-gtd.turkey%>%   group_by(imonth)%>%   summarise(numberOfEvents=n()) 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 0] <- 0 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 1] <- "January" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 2] <- "February" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 3] <- "March" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 4] <- "April" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 5] <- "May" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 6] <- "June" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 7] <- "July" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 8] <- "August" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 9] <- "September" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 10] <- "October" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 11] <- "November" 
gtd.turkey.month$imonth[gtd.turkey.month$imonth == 12] <- "December" 

gtd.turkey.month <- gtd.turkey.month[order(gtd.turkey.month$numberOfEvents), ] 
gtd.turkey.month$imonth <- factor(gtd.turkey.month$imonth, levels = gtd.turkey.month$imonth) 

plot5 <- ggplot(gtd.turkey.month, aes(x=imonth, y=numberOfEvents)) +    
         geom_point(size=5,color='#e67e22') +    
         geom_segment(aes(x=imonth,xend=imonth,y=0,yend=numberOfEvents)) +    
         labs(x= 'month', y= 'number of events') +   
         labs(title="Frequency of Attacks by Month",         
         subtitle="Source: gtd.turkey") +    
         theme(axis.text.x = element_text(angle=45, vjust=0.6, size=10)) 
plot5
windows(10,10)
grid.arrange(plot4,plot5,ncol=2)
#------------------------------------------------------------------------------------------------------------------#

# #Frequency of Attacks by City 
plot6 <- gtd.turkey %>%     
  group_by(provstate)%>%count() %>%arrange(desc(n))%>% 
ggplot(aes(x=provstate,y=n))+
        geom_bar(stat = "identity",aes(fill=n>250)) +
        theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=0.5)) +
        labs(x= 'provstate', y= 'number of events') +
        ggtitle('Frequency of Attacks by Provstate')+
        scale_fill_manual(values = c('#e67e22', '#2980b9'),
        guide=FALSE)
#plot6
#------------------------------------------------------------------------------------------------------------------#

plot7 <- gtd.turkey %>%     
  group_by(attacktype1_txt)%>%count() %>% arrange(desc(n)) %>%       
ggplot(aes(x=reorder(attacktype1_txt,n),y=n)) +       
  geom_bar(stat = "identity",fill= ('#e67e22'), color='black') +       
  theme(axis.text.x = element_text(size=8,vjust=1, hjust=1)) +       
  labs(x= 'attacktype', y= 'number of events') +       
  ggtitle('Frequency of Attacks by Attacktype') +       
  coord_flip() 
#plot7
#------------------------------------------------------------------------------------------------------------------#

plot8<- gtd.turkey %>%     
  group_by(targtype1_txt)%>%count() %>% arrange(desc(n)) %>%       
ggplot(aes(x=reorder(targtype1_txt,n),y=n)) +       
  geom_bar(stat = "identity",fill= ('#e67e22'), color='black') +       
  theme(axis.text.x = element_text(size=8,vjust=1, hjust=1)) +       
  labs(x= 'targettype', y= 'number of events') +       
  ggtitle('Frequency of Attacks by Targettype') +       
  coord_flip() 
#plot8

plot9<- gtd.turkey %>%     
  group_by(weaptype1_txt)%>%count() %>%arrange(desc(n)) %>%     
  ggplot(aes(x=reorder(weaptype1_txt,n),y=n)) +       
  geom_bar(stat = "identity",fill= ('#e67e22'), color='black') +       
  theme(axis.text.x = element_text(size=8,vjust=1, hjust=1)) +       
  labs(x= 'weaptype', y= 'number of events') +       
  ggtitle('Frequency of Attacks by Weapon type') +       
  coord_flip() 
#plot9
#------------------------------------------------------------------------------------------------------------------#

plot10 <-gtd.turkey %>%     
  group_by(gname)%>%count() %>% # count the number of times a gname appear     
  arrange(desc(n)) %>% # subset by rows based on condition     
  head(n=10) %>%       
  ggplot(aes(x=reorder(gname,n),y=n))+       
  geom_bar(stat = "identity",fill= ('#e67e22'), color='black') +       
  theme(axis.text.x = element_text(angle=90,size=8,vjust=1, hjust=1)) +       
  labs(x= 'group name', y= 'number of events') +       
  ggtitle('Frequency of Attacks by Group Name') +       
  coord_flip()
#plot10
#------------------------------------------------------------------------------------------------------------------#

gtd.turkey.year= group_by(gtd.turkey, iyear) %>%   
  summarise(numberOfEvents = length(eventid), 
            numberOfCasualties = sum(nkill, na.rm = TRUE)) 
plot11<- ggplot(gtd.turkey.year, aes(x = iyear)) +   
  geom_line(aes(y = numberOfEvents), size = 2,colour = '#e67e22') +   
  geom_line(aes(y = numberOfCasualties), size = 2, ,colour = ('#2980b9'), alpha=0.5) +   
  scale_x_continuous(breaks=seq(1970,2017,1)) +   
  annotate("text", x = c(2010,2010), y = c(1200,1100),label = c("Total Casualities", "Total Attacks"), colour = c(('#e67e22'), ('#2980b9')), size = 4)  + 
  ggtitle("Attacks / Casualities by Years") +   
  theme(axis.text.x = element_text(angle=90, size=8)) +   
  labs(x = "year", y = "attacks / casualities") 
#plot11
#------------------------------------------------------------------------------------------------------------------#

gtd.turkey %>%   mutate(total = 1) %>% # total attacks that year (creates a new variable)   
  count(iyear, wt=nkill) %>% # failed attempt   
  cbind("nkill") ->killed
colnames(killed)[3] <- "rate" 
  gtd.turkey %>%   count(iyear, wt = nwound) %>%cbind("nwound")-> wounded
  colnames(wounded)[3] <- "rate" 
plot12<-rbind(killed,wounded) %>%   ggplot(aes(iyear,n)) +     
  geom_line(aes(group=rate, colour=rate),size = 2)+   
  ggtitle("Rate of Killed/Wounded by Years") +   
  labs(x = "Years", y = "rate of killed/wounded") 
#plot12

windows(25,25)
grid.arrange(plot6,plot7,plot8,plot9,plot10,ncol=2)

#------------------------------------------------------------------------------------------------------------------#
#Maps
#------------------------------------------------------------------------------------------------------------------#

lon_lat <- data.frame(lon=gtd.turkey$longitude,lat=gtd.turkey$latitude)
plot19<-map("world2Hires", "Turkey")
points(gtd.turkey$longitude, gtd.turkey$latitude, col="red", pch=18)
title("Terrorism in Turkey")
#------------------------------------------------------------------------------------------------------------------#
geocode("Turkey")
geocode("USA")
geocode("Europe")

basemap <- get_googlemap(center = c(lon = 35.24332, lat = 38.96375),
                         zoom = 4,
                         size = c(640, 420),
                         maptype = 'roadmap',
                         color = 'bw') %>% ggmap()
basemap + ggtitle("Turkey Base Map") + theme(plot.title = element_text(size=22))
basemap1 <- get_googlemap(center = c(lon = 15.25512, lat = 54.52596),
                         zoom = 4,
                         size = c(640, 420),
                         maptype = 'roadmap',
                         color = 'bw') %>% ggmap()
basemap1 + ggtitle("Europe Base Map") + theme(plot.title = element_text(size=22))

basemap + geom_point(aes(x = longitude, y = latitude),
                            data = gtd.turkey,
                            alpha = .5,
                            size = 2.5) +
  ggtitle("Attack in Turkey") +
  scale_colour_manual(values = brewer.pal(3, "Accent")) +
  theme(legend.position = c(0.78, 0.93),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.background = element_rect(fill = 'white'),
        plot.title = element_text(size=22))


#------------------------------------------------------------------------------------------------------------------#
#Treemap- number of death in an year.
#------------------------------------------------------------------------------------------------------------------#

df= gtd.Europe
df %>% filter(nkill > 0) -> dfk
plot14<- treemap(dfk, 
                 index=c("iyear"), 
                 vSize = "nkill",  
                 palette = "Reds",  
                 title="Killings in Global Terrorism", 
                 fontsize.title = 14 
)
plot14
#------------------------------------------------------------------------------------------------------------------#
#Overview of number of people killed in Europe - Leaflet
#------------------------------------------------------------------------------------------------------------------#

plot15<-leaflet(data = gtd) %>%
  addTiles() %>%
  addMarkers(lat=gtd$latitude, lng=gtd$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", gtd$iday,"/",gtd$imonth,"/", gtd$iyear,
                          "<br><br><strong>Place: </strong>", gtd$city,"-",gtd$country_txt,
                          "<br><strong>Killed: </strong>", gtd$nkill,
                          "<br><strong>Wounded: </strong>", gtd$nwound))
plot15
#------------------------------------------------------------------------------------------------------------------#
#Types of Line plot
#------------------------------------------------------------------------------------------------------------------#

dfk %>% group_by(iyear) %>% summarise(nkill = sum(nkill)) %>% ungroup() -> dfy
plot16<- ggplot(data=dfy, aes(x = iyear, y = nkill)) +       
  geom_line() + geom_point() + theme_bw()
#plot16
#------------------------------------------------------------------------------------------------------------------#
#nkill by region
dfk %>% group_by(iyear,region_txt) %>% summarise(nkills = sum(nkill)) %>% ungroup() -> dfyr
colnames(dfyr)<-c("Year","Region","Killed")
ggplot(data = dfyr, aes(x = Year, y = Killed, colour = Region)) +       
  geom_line() + geom_point() + theme_bw()

#Type of Attack
df %>% group_by(iyear,attacktype1_txt) %>% summarise(n = length(iyear)) %>% ungroup() -> dfya
colnames(dfya)<-c("Year","Type of attack","Number of events")
plot17<-ggplot(data = dfya, aes(x = Year, y = `Number of events`, colour = `Type of attack`)) + 
  geom_line() + geom_point() + theme_bw()
#plot17

windows(10,10)
grid.arrange(plot11,plot12,plot16,plot17,ncol=2)

#------------------------------------------------------------------------------------------------------------------#
#Overview of number of people killed in UK & Ireland - Leaflet
#------------------------------------------------------------------------------------------------------------------#

df1=gtd.Europe%>%   filter(country_txt %in% c("United Kingdom","Ireland"))
plot18<-leaflet(data = df1) %>%
  addTiles() %>%
  addMarkers(lat=df1$latitude, lng=df1$longitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", df1$day,"/",df1$month,"/", df1$year,
                          "<br><br><strong>Place: </strong>", df1$city,"-",df1$country_txt,
                          "<br><strong>Killed: </strong>", df1$nkill,
                          "<br><strong>Wounded: </strong>", df1$nwound  ))

plot18
#------------------------------------------------------------------------------------------------------------------#


