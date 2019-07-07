# Purpose: Scraping user-provided data for popular Instagram users  
# Description: Scraping Instagram for follower counts based on user profiles who might have mention of their instagram handles
# Author: Tony

####################################### 
############ DOCUMENTATION ############ 
####################################### 

#--Function Flowchart------
# Goal is to minimise code usage and improve readability
# Replacing for loops with apply() functions when possible

# Main Steps:
# 1) Get query from db
# 2) Filter our instagram handles
# 3) Process and put into dataframe
# 4) Check if web links are valid
# 5) Run scraper through valid links
# 6) Retrieve data and sort by most followers

# Main Functions:
# 1) handle_extracter(): 
# Takes in a dataframe containing an "about_me" string column,
# returns a new dataframe with additional instagram handle column. (total 4 columns)
#
# 2) handle_processing():
# Takes in dataframe from (1), removes extra characters (like "@"), duplicates 
# and attaches instagram weblink to create a valid url in a new column. (total 5 columns)
#
# 3) scraper():
# Takes in a web link, returns 3 numerics (Number of followers, following, posts)
# from 1 instagram user. Returns FALSE if the web link is not valid. Within 
# mining_input() function.
#
# 4) mining_input():
# Takes the dataframe from (2), uses the scaper() function within and adds 3 columns
# to the database (followers, following, posts). Returns this database sorted by 
# most followers. (total 8 columns)
#
# 5) convert():
# Takes in a string, converts fields ending with 'k' and 'm' to numerics.
# For example from 1.2m to 1200, 000 and 2.3k to 2300. Within scraper() function.
#

rm(list=ls())

library(rvest) 
library(dplyr)
library(stringr) #for the str_extract_all function
library(tidyr) #for the unnesting function
library(tictoc) #timer to log code runtime

conn <- XXXXXXXXXX::pgsql_connect("XXXXXXXXXXXXXXXXXXXXXXXXXXXX")

####################################### 
############   FUNCTIONS   ############ 
####################################### 

####################################### 
#handling_logic(): 
#######################################
# To make sure that both XXXXXXXXXXXXXXXXXXXXXXXXXXXX are present, before running anything else.

handling_logic = function(){
  
  person_table_exists = dbGetQuery(conn,"XXXXXXXXXXXXXXXXXXXXXXXXXXXX'")
  influencer_table_exists = dbGetQuery(conn,"XXXXXXXXXXXXXXXXXXXXXXXXXXXX'")
  
  if (length(person_table_exists) != 0  && length(influencer_table_exists) != 0){
    return (TRUE)
  }else {
    return (FALSE)
  }
}

####################################### 
#handle_extracter(): 
####################################### 
# Takes in 4 parameters, a vector of about_me string, a vector of corresponding person_id, and 2 vectors of 
# regex expressions, one for pattern matching and second one for extracting the desired pattern.
# Columns that do not trigger the regex search query are removed.
# Returns a data frame with 3 columns.

# Using df instead of 

handle_extracter = function(df, regex_code, extract_code){
  
  data = data.frame(matrix(ncol = 3, nrow = 0))
  #need to set column names for proper binding later
  colnames(data) = c("XXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  
  for (i in 1:length(regex_code)){
    table = df[which(grepl(regex_code[i], df$about_me)),] # This step automatically does a subset already
    table$filter = str_extract_all(table$about_me, extract_code[i])
    data = rbind(data, table)
  }
  return(data)
}

####################################### 
#handle_processing():
####################################### 
# Using entire dataframe as parameter instead of individual vectors as there will be unnesting and 
# removal of duplicate handles, which will affect the person_id and about_me row indexes. 

handle_processing = function(df){
  #Unnest rows with more than 1 extracted text 
  #Remove handles with '@' in front 
  #Remove handles ending with a period
  #Remove duplicates
  #Remove empty handles
  #Add Instrgram Link
  #to unnest instagram link lists to characters for Postresql insertion later
  data = df %>%
    unnest(filter) %>% 
    mutate(filter = sub('@', '', filter)) %>%
    mutate(filter = gsub("\\.(?=\\.*$)", " ", filter, perl=TRUE)) %>%
    distinct(person_id, about_me, filter, .keep_all= TRUE) %>%
    filter(filter != "") %>%
    mutate(link = lapply(filter, function(x){paste('https://www.instagram.com/', x, sep = '')})) %>%
    unnest(link)
  
  return (data)
}

####################################### 
# mining_input(). 
####################################### 

mining_input = function(df){
  
  for (i in 1:nrow(df)){
    
    #remove this after testing
    print(i)
    
    insta_data = scraper(df$link[[i]])
    
    if (class(insta_data) == "numeric"){
      df$followers[i] = insta_data[1]
      df$following[i] = insta_data[2]
      df$posts[i] = insta_data[3]
    }else{
      df$followers[i] = df$following[i] = df$posts[i] = NA
    }
  }
  
  # Removes the NA rows - rows that did not have legitimate Instagram profile page (false positives)
  # Sometimes it is helpful to error check the about_me column as disrepancies here are due to poor user 
  # input (no spaces, using of @ for non-instagram purposes)
  df = df[which(as.logical(df$followers) == TRUE),]
  
  # Reorders the dataframe in terms of follower count
  df = df[order(-df$followers),]

  #Reseting indexes
  rownames(df) <- NULL
  
  return (df)
}

####################################### 
# scraping().
####################################### 

scraper = function(string){
  tryCatch({
    
    pg = read_html(string)
    all_meta_attrs = pg %>% html_nodes("meta") %>% 
      lapply(html_attrs) %>% lapply(names) %>% 
      unlist() %>% unique()
    
    dat = all_meta_attrs %>% lapply(function(x) {pg %>% html_nodes("meta") %>% html_attr(x)}) %>%
      data.frame()
    
    colnames(dat) = all_meta_attrs
    data = dat$content[which(grepl('[0-9]+\\s[FP]o[a-z]+', dat$content))]
    
    #removing commas ',' in the number for easier regex
    data = gsub(',', '', data)
    data = str_extract_all(data, '[0-9.km]+') #instagram labels their numbers with "k" for thousand and "m" for million.
    
    #extracting numerics only
    #Followers:
    followers = convert(data[[1]][1])
    #Following:
    following = convert(data[[1]][2])
    #Posts:
    posts = convert(data[[1]][3])
    
    result = c('Followers' = followers, 'Following' = following, 'Posts' = posts)
    
    return(result)
  },
  error=function(e){
    return (FALSE)}
  )
}

####################################### 
# convert()
####################################### 

convert = function(string){
  
  #Setting decimal places so we will not get 2e+06 notation
  options(scipen = 999)
  
  if (grepl('[m]', string)){
    num = as.double(str_extract_all(string, '[0-9.]+'))
    return (num * 1000000)
    
  }else if (grepl('[k]', string)){
    num2 = as.numeric(str_extract_all(string, '[0-9.]+'))
    return (num2 * 1000)
    
  }else{
    return (as.numeric(string))
  }
}

####################################### 
# Regex Codes for extracting data from about_me column
####################################### 

regex_codes = c('instagram:\\s[A-z0-9._]{0,30}', 'insta:\\s[A-z0-9._]{0,30}', 
                'insta:[A-z0-9._]{0,30}', 'instagram:[A-z0-9._]{0,30}', 'ig:\\s[A-z0-9._]{0,30}',
                'ig:[A-z0-9._]{0,30}', 'ig:\\s@[A-z0-9._]{0,30}', 'instagram:\\s@[A-z0-9._]{0,30}')

# Removed '@[A-z0-9._]{0,30}' and '@\\s[A-z0-9._]{0,30} from regex_codes

extract_codes = c('(?<=instagram: )[A-z0-9._]{0,30}', '(?<=insta: )[A-z0-9._]{0,30}'
                  , '(?<=insta:)[A-z0-9._]{0,30}', '(?<=instagram:)[A-z0-9._]{0,30}', 
                  '(?<=ig:\\s)[A-z0-9._]{0,30}', '(?<=ig:)[A-z0-9._]{0,30}',  '(?<=ig:\\s@)[A-z0-9._]{0,30}',
                  '(?<=instagram:\\s@)[A-z0-9._]{0,30}')

####################################### 
############      MAIN     ############ 
#######################################  

handler <- tryCatch({
  tic()
  
  if (handling_logic()){ #to test if required tables are present in postresql
  
# First SELECT query pulls new person rows created to scan for new about_me sections.
# Second SELECT query pulls users from instagram_influencers table to scan for changed about_me sections.
# Third SELECT query pulls users who initally had a null about_me during a previous run of this code, but later filled it up.
  
  refreshed_list <- dbGetQuery(conn,"XXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  
  data = handle_extracter(refreshed_list, regex_codes, extract_codes)
  data = handle_processing(data)
  final = mining_input(data)
  
  # Creating temporary table to prevent duplicate data in XXXXXXXXXXXXXXXXXXXXXXXXXXXX in the following query.
  # This table will be overwritten everytime the script is run.
  # Not everything in this table would be inserted in the final table, only those are are distinct.

  dbWriteTable(conn, c("adhoc", "temp_table_instagram_influencers"), value = final, 
               overwrite = TRUE, row.names = FALSE)
  
  # Insertion of only unique instagram handle (filter) names by joining with the existing instagram_influencers
  # table. This means that the same user can have more than 1 row if during a change in about_me, there was 
  # a different username picked up by the regex expressions.

  dbSendQuery(conn, "XXXXXXXXXXXXXXXXXXXXXXXXXXXX")
  
  }
  
  #return code run time
  toc <- as.numeric(strsplit(capture.output(toc()), ' ')[[1]][1])
  c(1, toc)
  
},  error=function(cond) {
  message("Error writing data to Postgres")
  message(cond)
  toc <- as.numeric(strsplit(capture.output(toc()), ' ')[[1]][1])
  return(c(0,toc))
})

