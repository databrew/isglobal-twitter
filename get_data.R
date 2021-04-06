library(dplyr)
library(gsheet)
library(rtweet)

# Define url
url_projectes <- 'https://docs.google.com/spreadsheets/d/1yESiqn5R3xFTDckJZVDYuganzgJKDF6Nw2Qk-_Ab4GI/edit?ts=6040d0ec#gid=0'
url_personal <- 'https://docs.google.com/spreadsheets/d/1yESiqn5R3xFTDckJZVDYuganzgJKDF6Nw2Qk-_Ab4GI/edit?ts=6040d0ec#gid=282027074'
# Retrieve data
projectes <- gsheet::gsheet2tbl(url = url_projectes, sheetid = 'PROJECTES')
persones <- gsheet::gsheet2tbl(url = url_personal, sheetid = 'PERSONES')

# Combine data
df <- 
  bind_rows(
    projectes %>%
      dplyr::select(name = Projecte,
                    username = `Compte Twitter`,
                    tag = `Etiqueta Twitter`) %>%
      mutate(type = 'Project') %>%
      mutate(program = name),
    persones %>%
      mutate(name = paste0(Nom, ' ', Cognom),
             tag = NA) %>%
      dplyr::select(name,
                    username = `Usuari Twitter`,
                    tag,
                    program = Programa) %>%
      mutate(type = 'Person')
  )
# Clean
df$username <- 
  gsub('@', '', df$username)
df$username <- tolower(df$username)
df$username <- ifelse(nchar(df$username) <2, NA, df$username)


# Retrieve data

# Get followers data
library(rtweet)
# Read in twitter credentials
library(yaml)
twitter_credentials <- yaml.load_file('credentials/credentials.yaml')
## load rtweet package
library(rtweet)
token <- create_token(
  app = "bcndata",
  consumer_key = twitter_credentials$twitter_api_key,
  consumer_secret = twitter_credentials$twitter_api_secret_key,
  access_token = twitter_credentials$twitter_access_token,
  access_secret = twitter_credentials$twitter_access_token_secret)

# rt <- search_tweets(
#   "#rstats", n = 18000, include_rts = FALSE
# )

if(!dir.exists('users')){
  dir.create('users')
}




for(i in 1:nrow(df)){
  message(i, ' of ', nrow(df))
  this_user <- df$username[i]
  if(!is.na(this_user)){
    # Get followers 
    # followers <- get_followers(this_user, n = 75000, retryonratelimit = TRUE)
    # # Make dataframe
    # followers_df <- tibble(user = this_user,
    #                        follower = followers)
    user <- lookup_users(this_user)
    user <- data.frame(user)
    if(nrow(user) > 0){
      types <- c()
      for(j in 1:ncol(user)){types <- c(types, is.list(user[,j]))}
      user <- user[,!types]
    }

    # # # Get friends (ie, the people that this user is following
    # friends <- get_friends(this_user, n= 75000, retryonratelimit = TRUE)
    this_path <- paste0('users/', this_user)
    # if(!dir.exists(this_path)){
    #   dir.create(this_path)
    # }
    # write_csv(followers, file = paste0(this_path, '/followers.csv'))
    # write_csv(friends, file = paste0(this_path, '/friends.csv'))
    write_csv(user, file = paste0(this_path, '/user.csv'))
    if(file.exists(paste0(this_path, '/friends'))){
      try({
        file.copy(from = paste0(this_path, '/friends'),
                  to = paste0(this_path, '/friends.csv'))
        
        file.remove(paste0(this_path, '/friends'))
      })
    }

  }
}


# Define function for retrieving data
if(!dir.exists('tweets')){
  dir.create('tweets')
}

get_info <- function(username){
  file_name <- paste0(username, '.csv')
  file_path <- paste0('tweets/', file_name)
  query <- paste0('twint -u ', username, ' --csv --output ', file_path)
  system(query)
}

for(i in 1:nrow(df)){
  try({
    message(i, ' of ', nrow(df))
    this_user <- df$username[i]
    if(!is.na(this_user)){
      get_info(username = this_user)
    }
  })
}
