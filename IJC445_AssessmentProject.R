# Installing packages that are necessary for the file:

install.packages("rgl")
install.packages(("patchwork"))
install.packages("reshape2")

install.packages("scales")
library(tidyverse)
library(patchwork)
library(MASS)
library(rgl)
library(ggradar)
library(scales) 

# The following step were already performed in Excel:
#1. Remove the lyrics column to lighten the dataset
#2. Create pivot table to find which artists have the most unique songs on the billboard
#3. Consider the 100 top artists with the most songs on the billboard
#4. Brute input the gender of the 100 artist from a quick web search.
#5. Extract the Song id for the songs of these 100 artists from the url column.
#6. Use the song id column to bring in the technical features of the songs from the Acoustic Features sheet.

#Reading Data already reorganized in Excel:
df1<-read.csv("billboard_data.csv")

# Visualising the dataframe:
View(df1)
nrow(df1)
ncol(df1)

# Creating Plot 1: 
# How many artists in the top 100 artist list are women?

df2<- subset(df1, select= c(band_singer, Gender)) %>% # Choosing just the columns required for the plot.
  subset(., Gender!="#N/A") # Filtering the data set to remove the singers not on top 100.
  


df2<- unique(df2)# removing repetitive song entries over the years:   
  
View(df2)
#Visual for Plot 1:
Plot1<- ggplot(df2, aes(factor(1), fill=Gender)) + geom_bar() +
  coord_polar(theta="y") +
  scale_fill_manual(values = c("Solo Male" = "#3498DB", "Solo Female" = "#E74C3C", "Band" = "#F39C12")) +
  labs(x=NULL, y=NULL, fill="Gender of Artists",
       title="Distribution of Artists by Gender on the billboard") +
  nfp_theme 
Plot1

# Creating Plot 2: 
# Which of the female singers have the highest number of songs on the Billboard:

df3<-df1 %>% distinct(song, .keep_all = TRUE) %>% # Picking distinct songs from the whole list.
  subset(., Gender!="#N/A") %>%# removing singers not on the top 100 artists.
  filter(Gender=="Solo Female") %>% # Filtering for the female artists.
  count(band_singer, sort="True") # Counting the songs for each Female singer and sorting the results.
view(df3)

#Visual for Plot 2:
Plot2<-ggplot(df3, aes(x = reorder(band_singer, -n), y = n)) +
  geom_bar(stat="identity",fill = "#3498DB", alpha = 0.7, color = "#2C3E50", width=0.7) +
  nfp_theme +
  labs(x = "Band/Singer", y = "Count", title = "Song Distribution by Top Artists")+
  theme(  panel.grid.major = element_line(color = "#ECF0F1", size = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
  
Plot2
# Creating Plot 3:
# How many songs did the top 3 female singers have on the billboard each year?:


df4<-df1 %>% 
  subset(., band_singer %in% c("Rihanna","Taylor Swift","Beyoncé")) %>% # filtering the songs based on the top 3 singers from the plot 2.
  count(year,band_singer) # Counting the songs for each singer for each year.
  
View(df4)  

#Visual for Plot 3:

Plot3<-ggplot(df4, aes(x = year, y = n, colour = band_singer)) +
  geom_line(size = .5) +
  geom_point(size = 2) +
  nfp_theme +
  scale_x_continuous(breaks = seq(2005, 2023, by = 1)) +
  scale_colour_manual(
    name   = "Band Singer",
    values = c("Rihanna" = "#F39C12", "Taylor Swift" = "#E74C3C", "Beyoncé" = "#3498DB")
  ) +
  labs( title="Singer Performance Over The Years",   x = "year",    y = "Count of song"  )+
  theme(  panel.grid.major = element_line(color = "#ECF0F1", size = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
Plot3
# Creating Plot 4:  
# How on average do the top 3 singers songs look from a technical aspect?:

df5<-df1 %>%
  subset(., band_singer %in% c("Rihanna","Taylor Swift","Beyoncé")) %>% # filtering the songs based on the top 3 singers from the plot 2.
  subset(., select= c(band_singer,danceability,	energy,	key,	loudness,	mode,	speechiness,	acousticness,	instrumentalness,	liveness,	valence,	tempo)) %>% #Extracting the Acoustical feature for the songs.
  mutate(across(
    where(is.numeric),
    ~ (.- min(., na.rm = TRUE)) /
      (max(., na.rm = TRUE) - min(., na.rm = TRUE))
  )) %>% # Scaling the acoustic feature between 0 and 1 for better visualization.
  group_by(band_singer) %>% # Grouping the songs by the singer for summerisation.
  summarise(avg_danceability=mean(danceability),
            avg_energy=mean(energy),
            avg_key=mean(key),	
            avg_loudness=mean(loudness),	
            avg_mode=mean(mode),	
            avg_speechiness=mean(speechiness),	
            avg_acousticness=mean(acousticness),	
            avg_instrumentalness=mean(instrumentalness),	
            avg_liveness=mean(liveness),	
            avg_valence=mean(valence),	
            avg_tempo=mean(tempo)) # aggregating acoustical features across songs for the 3 singers.
  
View(df5)

Plot4 <- ggradar(  df5, group.line.width = 1 ,group.point.size = 2, 
  group.colours = c("Rihanna" = "#F39C12", "Taylor Swift" = "#E74C3C", "Beyoncé" = "#3498DB")  ) +
  nfp_theme +
  labs(title = "Acoustic Features: Top 3 Artists") 
Plot4

#Setting up the Theme for all the plots for uniformity:
nfp_theme <- theme_minimal(base_size = 12) +
  theme(
    # Text styling
    plot.title = element_text(size = 16, face = "bold", color = "#2C3E50", hjust = 0.5, margin = margin(b = 12)),
    plot.subtitle = element_text(size = 14, color = "#34495E", hjust = 0.5, margin = margin(b = 8)),
    plot.caption = element_text(size = 11, color = "#7F8C8D", hjust = 1, margin = margin(t = 8)),
    
    # Axis styling
    axis.title = element_text(size = 12, color = "#2C3E50"),
    axis.text = element_text(size = 11, color = "#34495E"),
    
    # Legend styling
    legend.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
    legend.title.position = "top",
    legend.text = element_text(size = 11, color = "#34495E"),
    legend.position = "bottom",
    
    # Panel and background
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
  )
Plot1
Plot2
Plot3
Plot4
