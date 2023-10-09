install.packages("rvest")
install.packages("tidyverse")
install.packages("remotes")
install.packages("polite")

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
    if("PPG" %in% colnames(stat_table) && "GP" %in% colnames(stat_table) && "MPG" %in% colnames(stat_table)) {
      
      for (j in 1:nrow(stat_table)) {
        if (grepl("Career", stat_table[j,1]))
        {
          career_row_num <- j
        }
      }
      
      if(!is.null(row_number) && !career_row_num == 0) {
        PPG_Num <- grep("PPG", colnames(stat_table))[1]
        GP_Num <- grep("GP", colnames(stat_table))[1]
        MP_Num <- grep("MPG", colnames(stat_table))[1]
        
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







