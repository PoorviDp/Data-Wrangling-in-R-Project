install.packages("plyr")
install.packages("tidyverse")
install.packages("formattable")
install.packages("splitstackshape")
install.packages("jsonlite")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("ggthemes")
install.packages("tm")
install.packages("RSentiment")
install.packages("lubridate")

#install.packages("plotly")


library(plyr)
library(tidyverse)
library(formattable)
library(splitstackshape)
library(jsonlite)
library(wordcloud)
library(RColorBrewer)
library(ggthemes)
library(tm)
library(RSentiment)
library(lubridate)
library(RColorBrewer)
#library(plotly)

## data import
html_url <- "https://raw.githubusercontent.com/PoorviDp/Data-Wrangling-in-R-Project/master/tmdb_5000_movies.csv"
movie <- read_csv(html_url,col_names = TRUE,na = "NA")
credits <- read_csv("C:/Users/poorv/Documents/Data Wrangling in R/Final Project/DataWranglingProject/tmdb_5000_credits.csv",col_names = TRUE,na = "NA")

## information about the tables : movie, credits
colnames(movie)
dim(movie)
colnames(credits)
dim(credits)


## data cleaning
## removing spurious characters
movie$title <- (sapply(movie$title,gsub,pattern = "\\Â",replacement = ""))

## deleting duplicate rows
movie <- movie[!duplicated(movie$title), ]
dim(movie)

sum(complete.cases(movie))
sum(complete.cases(movie))

## currency transformation for countries which have currency other than dollars

#movie <- transform(movie, budget = ifelse(production_countries == "South Korea", budget/1173.49, budget))
# movie <- transform(movie, budget = ifelse(production_countries == "Japan", budget/115.33, budget))
# movie <- transform(movie, budget = ifelse(production_countries == "Turkey", budget/3.49, budget))
# movie <- transform(movie, budget = ifelse(production_countries == "Hungary", budget/298.17, budget))
# movie <- transform(movie, budget = ifelse(production_countries == "Thailand", budget/35.67, budget))

# movie <- transform(movie, gross = ifelse(production_countries == "South Korea", gross/1173.49, gross))
# movie <- transform(movie, gross = ifelse(production_countries == "Japan", gross/115.33, gross))
# movie <- transform(movie, gross = ifelse(production_countries == "Turkey", gross/3.49, gross))
# movie <- transform(movie, gross = ifelse(production_countries == "Hungary", gross/298.17, gross))
# movie <- transform(movie, gross = ifelse(production_countries == "Thailand", gross/35.67, gross))


## transformation of "keywords" column into tibble
keywords <- movie %>%    
  filter(nchar(keywords) > 2) %>%                 # fiter out blank keywords field
  mutate(                                         # create a new field 
    js = lapply(keywords, fromJSON)               # containing a LIST of keyword and value pairs
  ) %>%                                           # called id and name
  unnest(js) %>%                                  # turn each keyword/value pairs in the LIST into a row
  select(id, title, keywords = name)

## Combining the keywords of a movie in a single column
keywords <- aggregate(keywords ~.,data = keywords, paste, collapse = ",")

#Combining the genres of a movie in a single column
genres <- movie %>% filter(nchar(genres) > 2) %>%                   
  mutate( js = lapply(genres, fromJSON)) %>%                                           
  unnest(js) %>%                                  
  select(id, title, genres = name) 

genres <- aggregate(genres ~.,data = genres, paste, collapse = ",")

# Combining production_companies
production_companies <- movie %>% filter(nchar(production_companies) > 2) %>%                   
  mutate( js = lapply(production_companies, fromJSON)) %>%                                           
  unnest(js) %>%                                  
  select(id, title, production_companies = name) 

production_companies <- aggregate(production_companies ~.,data = production_companies, paste, collapse = ",")

# Combining production countries

production_countries <- movie %>%    
  filter(nchar(production_countries) > 2) %>%     
  mutate(                                         
    js = lapply(production_countries, fromJSON)   
  ) %>%                                          
  unnest(js) %>%                                  
  select(id, title, production_countries = name)

countries <- movie %>%    
  filter(nchar(production_countries) > 2) %>%     
  mutate(                                         
    js = lapply(production_countries, fromJSON)   
  ) %>%                                          
  unnest(js) %>%                                  
  select(id, title, production_countries = name)

production_countries <- aggregate(production_countries ~.,data = production_countries, paste, collapse = ",")

# combining spoken languages
spoken_languages <- movie %>%    
  filter(nchar(spoken_languages) > 2) %>%        
  mutate(                                         
    js = lapply(spoken_languages, fromJSON)      
  ) %>%                                          
  unnest(js) %>%                                 
  select(id, title, spoken_languages = iso_639_1) 

spoken_languages <- aggregate(spoken_languages ~.,data = spoken_languages, paste, collapse = ",")



#################### COMBINING THESE COLUMNS INTO THE MAIN MOVIE DATASET ###########################

# Dropped existing unformatted columns in the main dataset, creating a new dataset "movies"
movies <- subset(movie, select = -c(genres, keywords, production_companies, production_countries, spoken_languages))
glimpse(movies)

movies <- movies %>%
  full_join(keywords, by = c("id", "title")) %>%
  full_join(genres, by = c("id", "title")) %>%
  full_join(production_companies, by = c("id", "title")) %>%
  full_join(production_countries, by = c("id", "title")) %>%
  full_join(spoken_languages, by = c("id", "title"))


glimpse(movies)
###########################################################################################################
######################################   PLOTS   ##########################################################
###########################################################################################################


# 1 Top 20 movies with highest average_vote with color acc to vote count

movies %>% select(title,vote_average,vote_count, budget) %>% 
  filter(vote_count > 500 ) %>% arrange(desc(vote_average)) %>% head(20) %>%
  ggplot(aes(x = title,y = vote_average,fill = budget )) + geom_bar(stat = "identity") + coord_flip(ylim = c(7, 9)) +
  scale_fill_continuous()

# 2 Top 20 movies by popularity, color on vote count
# movies %>% select(title,vote_count, popularity) %>% 
#   filter(vote_count > 500 ) %>% 
# ggplot(movies, aes(x = title, y = popularity, fill = vote_count)) +
#   geom_point() + coord_flip() 

  movies %>% select(title,vote_average,vote_count, popularity) %>% 
    filter(vote_count > 300 ) %>%  head(30) %>%
    ggplot(aes(x = title,y = popularity, fill = vote_count)) + geom_bar(stat = "identity") + coord_flip() +
    scale_fill_continuous()

# 3
## frequency of genres 
genre1 <- Corpus(VectorSource(genres$genres))
genre_dtm <- DocumentTermMatrix(genre1)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing = TRUE) 
genre_wf <- data.frame(word = names(genre_freq), freq = genre_freq)

ggplot(genre_wf, aes(x = reorder(word,-freq), y = freq)) +  
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Movie Genre frequency graph") + 
  xlab("Genre") + 
  ylab("Frequency")

# wordcloud of genres
set.seed(10)
pallete <- brewer.pal(8,"Accent")
wordcloud(genre_wf$word,genre_wf$freq,random.order = FALSE,
          rot.per = .15, colors = pallete , scale = c(4,.9),
          title = "WordCloud: Movie Genres")

# adding new column for year of release

year <- year(movies$release_date)
movies$year <- year

# 4 heat map of number of movies by year and countries
country_movie_summary <- inner_join(movies,countries) %>% select(year,production_countries) %>%
             group_by(year,production_countries) %>% mutate(movie_count = n())

country_movie_summary %>% ggplot(aes(year, production_countries)) + 
  geom_tile(aes(fill = movie_count),colour = "yellow") +
  scale_fill_gradient(low = "orange",high = "red") +
  xlab("Year of movie release") + 
  ylab("Country") + 
  ggtitle("Heat Map: Country vs Movie Release Year") + 
  guides(fill = FALSE)

# 5 top 10 most expensive movies
movies %>% select(original_title,budget) %>% drop_na(original_title) %>% arrange(desc(budget)) %>% 
  head(10) %>%  ggplot(aes(reorder(original_title,budget),budget,fill = original_title)) + 
  geom_bar(stat = "identity") + scale_fill_brewer(palette = "Spectral" )  + 
  theme(axis.text.x = element_text(angle = 90),plot.title = element_text(color = "Black",face = "italic"),
        legend.position = "none") + scale_y_continuous(labels = scales::comma) +
  labs(x = "",y = "Total Budget in $",title = "Top 10 most expensive movies")


# 6 movies released per month
movie$Year = as.factor(format(movie$release_date,"%Y"))
movie$Date = as.factor(format(movie$release_date,"%d"))
movie$month = month.abb[(as.factor(format(movie$release_date,"%m")))]
movie %>% group_by(month) %>% drop_na(month) %>% 
  summarise(count = n()) %>% arrange(desc(month)) %>% 
  ggplot(aes(reorder(month,count),count,fill = month)) + 
  geom_bar(stat = "identity") + theme(plot.title = element_text(size = 14,face = "italic",colour = "red"),
                                  axis.text.x = element_text(angle = 90),legend.position = "none") + 
  labs(x = "",y = "Total number of movies released",
       title = "Number of Movies Releases per month") + 
  coord_flip() + geom_label(aes(label = count))

# boxplot
movie %>% drop_na(month) %>% ggplot(aes(month,vote_average,fill = month)) + 
  geom_boxplot(outlier.colour = "red",na.rm = TRUE) + 
  theme(plot.title = element_text(size = 14,face = "italic",colour = "black"),
        axis.text.x = element_text(angle = 90),legend.position = "none") +
  labs(x = "", y = "Average Vote",title = "Boxplot of Average votes received by month") + 
  coord_flip()

# 7 movie revenue variation by country
# str(movie)
# movies %>%
#   filter()
ggplot(subset(movies, production_countries %in% country_movie_summary$production_countries),
       aes(x=production_countries,y=revenue/1000000))+
geom_boxplot()+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ylab("Average Movie Revenue (Million $)")+
  xlab("")+
  ggtitle("Movie Revenue variation by Country")+
  ylim(0,100)


# 8 
ggplot(movies,aes(vote_average))+
  geom_histogram(bins=80)+
  geom_vline(xintercept = mean(movie$vote_average,na.rm = TRUE),colour="steel blue")+
  ylab("Count of Movies")+
  xlab("IMDB Score")+
  ggtitle("Histogram: average vote rating")



######################################################################################################
all_crew <- credits %>%      # start with the raw tibble 
  filter(nchar(crew) > 2) %>%        # filter out movies with empty crew  
  mutate(                          #       
    js  =  lapply(crew, fromJSON)  # turn the JSON into a list
  )  %>%                           #
  unnest(js) 

all_cast <- credits %>%      # start with the raw tibble 
  filter(nchar(cast) > 2) %>%        # filter out movies with empty crew  
  mutate(                          #       
    js  =  lapply(cast, fromJSON)  # turn the JSON into a list
  )  %>%                           #
  unnest(js) 
glimpse(all_cast)

glimpse(all_cast)
cast <- subset(all_cast, select = -c(movie_id, title, cast, crew))
crew <- subset(all_cast, select = -c(movie_id, title, cast, crew))
head(crew, n=10)
glimpse(movies)

library(DT)
datatable(head(movies,50))
datatable(head(cast, 20))
datatable(head(cast, 20))
