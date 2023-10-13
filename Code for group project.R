library(rvest)
library(tidyverse)
library(magrittr) # better handling of pipes
library(purrr) # to work with lists and map functions
library(glue) # to paste strings
library(stringr) # to hand strings
library(remotes)
library(polite) # polite is the "polite" version of rvest
library(xml2) # makes it easier to work with HTML and XML from R
#Get them in the system!


# Set the URL of the webpage to scrape
url <- "https://en.wikipedia.org/wiki/List_of_National_Basketball_Association_career_scoring_leaders"
# Read the HTML content of the webpage
nba <- read_html(url)

# Check the data type of 'page'
nba %>% typeof()
nba %>% glimpse()
nba

nba %>% html_structure()

nba %>%
  html_nodes(xpath = '//*[@class="vcard"]/span[@class="fn"]/a') # write .title, with the dot, 
#because we want all results with that tag. Try removing it and see what happens.re

# Extract movie names from the webpage
player_name <- nba%>%
  html_nodes(xpath = '//*[@class="vcard"]/span[@class="fn"]/a') %>% # Select the HTML elements with class ".article_movie_title a"
  html_text()  # Extract the text content
player_name

SLS_df <- tibble(Association = "NBA",
                 Names = player_name) # build the Title variable using the code we used above
SLS_df

# Get the attributes of the selected HTML elements
# This will include the 'href' attribute which contains the URLs
nba %>%
  html_nodes(xpath = '//*[@class="vcard"]/span[@class="fn"]/a') %>%
  # Get the attributes of the selected HTML elements
  # This will include the 'href' attribute which contains the URLs  
  html_attrs() %>%
  glimpse()

# Extract all anchor elements with class "article_movie_title"
nba %>%
  html_nodes(xpath = '//*[@class="vcard"]/span[@class="fn"]/a') %>%
  html_attrs() %>%
  map_chr("href") %>%
  paste0("https://en.wikipedia.org/", .)

total_points <- nba %>%
  html_nodes("td:nth-child(5)") %>%
  html_text() %>%
  str_replace_all("\n", "") 
top_50_points <- head(total_points, 50)
top_50_points

# Update the 'SLS_df' dataframe by adding two new columns: 'Link' and 'Probabilities'
SLS_df %<>% 
  # Extract the 'href' attribute from the anchor elements with class '.article_movie_title'
  mutate(Link = nba %>% html_nodes(xpath = '//*[@class="vcard"]/span[@class="fn"]/a') %>% html_attr("href") ,Points = top_50_points)

SLS_df

for (i in 1:50)
{
  full_link <- paste0("https://en.wikipedia.org/", SLS_df[i,3])
  SLS_df[i,3] <- full_link
}

# Create a variable 'url_link' using the first value from the 'Link' column of 'SLS_df'
url_link<- glue(SLS_df$Link[1])
url_link

# Fetch the release date of the movie from the webpage corresponding to 'url_link'
position <- url_link %>%
  read_html() %>%
  html_node(".infobox-data.role") %>%
  html_text()
position

team <- url_link %>%
  read_html() %>%
  html_node("th.infobox-header") %>%
  html_text()
team

stat_table <- url_link %>%
  read_html() %>%
  html_element("table.wikitable") %>%
  html_table()

stat_table

ppg <- stat_table[nrow(stat_table) - 1, ncol(stat_table)]
ppg
gp<- stat_table[nrow(stat_table) - 1,3]
gp
mp <- stat_table[nrow(stat_table) - 1,5]
mp

head(stat_table)

view(stat_table)


# Create empty vectors
ppg_vector <- c()
gp_vector <- c()
mp_vector <- c()

for(i in 1:50)
{
  url_link <- SLS_df[[i,3]]
  stat_tables <- url_link %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table()
  
  ppg <- NA
  gp <- NA
  mp <- NA
  
  for(j in 1:length(stat_tables)) {
    stat_table <- stat_tables[[j]]
    
    career_row_num <- 0
    
    # Check if the table contains the required columns and header
    if("PPG" %in% colnames(stat_table) && "GP" %in% colnames(stat_table)) {
      
      for (j in 1:nrow(stat_table)) {
        if (grepl("areer", stat_table[j,1]))
        {
          career_row_num <- j
        }
      }
      
      if(!is.null(row_number) && !career_row_num == 0) {
        PPG_Num <- grep("PPG", colnames(stat_table))[1]
        GP_Num <- grep("GP", colnames(stat_table))[1]
        MP_Num <- grep("MPG", colnames(stat_table))[1]
        if (is.na(MP_Num))
        {
          MP_Num <- grep("MIN", colnames(stat_table))[1]
        }
        
        ppg <- stat_table[career_row_num,PPG_Num]
        gp<- stat_table[career_row_num,GP_Num]
        mp <- stat_table[career_row_num,MP_Num]
        
        break
      }
      
    }
  }
  
  # Append the values to the vectors
  ppg_vector <- c(ppg_vector, ppg)
  gp_vector <- c(gp_vector, gp)
  mp_vector <- c(mp_vector, mp)
}

# Add the vectors as new columns to the dataframe
SLS_df$PPG <- ppg_vector
SLS_df$GP <- gp_vector
SLS_df$MP <- mp_vector

view(SLS_df)

# Remove commas and extra symbols from df
for (i in 1:nrow(SLS_df))
{
  SLS_df$Points[i] <- gsub(pattern = "‡", "" ,SLS_df[[i,4]])
  SLS_df$Points[i] <- gsub(pattern = ",", "" ,SLS_df[[i,4]])
  
  SLS_df$PPG[i] <- gsub(pattern = "‡", "" ,SLS_df[[i,5]])
  SLS_df$PPG[i] <- gsub(pattern = ",", "" ,SLS_df[[i,5]])
  
  SLS_df$GP[i] <- gsub(pattern = "‡", "" ,SLS_df[[i,6]])
  SLS_df$GP[i] <- gsub(pattern = ",", "" ,SLS_df[[i,6]])
  
  SLS_df$MP[i] <- gsub(pattern = "‡", "" ,SLS_df[[i,7]])
  SLS_df$MP[i] <- gsub(pattern = ",", "" ,SLS_df[[i,7]])
}

# Now we can mutate to get rows with numbers into numeric class
SLS_df <- SLS_df %>%
  mutate(Points = as.numeric(Points),
         PPG = as.numeric(PPG),
         GP = as.numeric(GP),
         MP = as.numeric(MP))

# Now we can use our data for plotting

plot(SLS_df$MP, SLS_df$PPG)
abline(lm(PPG~MP, data = SLS_df), col = "red")

#Plot

# Relationship between MP and PPG
# The more one got to play, the more points he scored? Direct proportion?
ggplot(data = SLS_df,
       aes(PPG, MP, colour = Names)) + 
  geom_point(aes(size = Points))

# Relationship between Points and MP
# MP in (36,38) are the most efficient?
ggplot(data = SLS_df, 
       aes(x = MP, y = Points)) +
  geom_line(size = 0.5) +
  labs(title = "Points vs MP", x = "MP", y = "Points")
#This graph is normal distributed, therefore it doesn't seem that if a player play more minutes and they will scoremopre points. A player miht be more efficient if they can get some rest(approximately 10 minutes through out the whole game)

# Relationship between GP and PPG
# The more games played, the less points per game? Less efficiency? Inverse ratio?
ggplot(data = SLS_df,
       aes(x = GP, y = PPG)) +
  geom_point() +
  labs(title = "Points Per Game vs Games Played", x = "Games Played", y = "Points Per Game")
#There isn't a lot of relationship between GP and PPG. However, we can see that top 50 scores tends to play at least 800 games or more.

# Relationship between MP and PPG
# The more time in a game, the more points? 
ggplot(data = SLS_df, 
       aes(x = MP, y = PPG)) +
  geom_point() +
  labs(title = "Minutes Per Game vs. Points Per Game", x = "Minutes Per Game", y = "Points Per Game")

ggplot(data = SLS_df,
       aes(x = GP, y = Points)) +
  geom_point() +
  labs(title = "Points Per Game vs Points Scored", x = "Games Played", y = "Points")
# For this graph we can see that top scores tends have played around 1300 to 1600 games, and there is one player played over 1600 games but the points he scored was low.