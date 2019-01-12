#==============================================================================
# 04-CA-create-adjacency-matrix.R
# Purpose: create list of users who are followed by at least 10 elected officials, 
# scrape these peoples' followers, and create adjacency matrix
# Author: Haohan Chen & Chris Bail (referred to Pablo Barbera's code)
#==============================================================================

# setup
# library(tweetscores)
options(stringsAsFactors=F)
userfile <- 'Data/userlist.Rda'
matrixfile <- 'Data/adj-matrix.Rda'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## select accounts to load
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# file path (for Chris's machine)
#setwd("/Users/christopherandrewbail/Google Drive/EchoChamberAppShared/Twitter_Data_Collect")


Congressmen <- read.csv("Data/Congressmen.csv")
load("Data/Congressmens_friends_table.Rda")
Congressmen_Friends_Info <- Congressmen_Friends_Info[order(Congressmen_Friends_Info$N_Friends_Congressmen,decreasing=T), ]

# accounts_scraped <- list.files("Data/Followers_lists", full.names=F)
# # A problem... a few congressmen there (because these are congressmen's friends, though I am impressed that congressmen don't befriend one another. 
# paste0(Congressmen$screen_name, ".txt") %in% accounts_scraped
# paste0(Congressmen_Friends_Info$screen_name, ".txt") %in% accounts_scraped

#restrict accounts to only those that are followed by at least 10 elected officials (we call these people opinion leaders)

opinion_leaders<-Congressmen_Friends_Info[Congressmen_Friends_Info$N_Friends_Congressmen>9,]

#bind congressmen with opinion_leaders

#clean up data
Congressmen$X<-NULL
Congressmen$url<-NULL
Congressmen$id<-NULL
opinion_leaders$N_Friends_Congressmen<-NULL
opinion_leaders$created_at<-NULL
opinion_leaders$statuses_count<-NULL
opinion_leaders<-rbind(Congressmen, opinion_leaders)

#now use friendships() function within TwitteR package to identify edges within opinion_leaders individuals

library(twitteR)

#authentice using Chris's credentials in another script 

#get names of opinion leaders and chunk them into 100 person segments
names<-opinion_leaders$screen_name
chunked_names<-split(names, seq_along(names)%/%50)
         



final_data<-as.data.frame(NULL)

users.df <- getUsersBatch(ids=as.character(friends_freq$list_friends), my_oauth = oauth, include_entities="false",
                          verbose=TRUE, output=NULL, fname = "Congressmen_Friends_info.Rda")


for(i in 1:length(opinion_leaders)){
  person<-getUser(opinion_leaders$screen_name[i])
    for(j in length(chunked_names)){
      namers<-unlist(chunked_names[j])
      #determine friendships
      output<-friendships(namers)
      #subset rows with overlap
      overlap<-output[output$following=="TRUE"|output$followed_by=="TRUE",]
      final_data<-rbind(final_data, overlap)
      Sys.sleep(1)
    }
  print(i)
  }

test<-c("ridhikash07","chris_bail","kinggary","BrendanNyhan","Julie_C_Smith")
output<-friendships(names)





# So just make a random choice
accounts <- opinion_leaders$screen_name

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## creating user list
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

followers.list <- list(NULL)
for (i in 1:length(accounts)){
  followers.list[[i]] <- readLines(paste0("Data/Followers_lists/", accounts[i], ".txt"))
  cat(i, "of", length(accounts), " ", accounts[i],"\n")
}

all <- unlist(followers.list)
library(data.table)
dt <- data.table(all)
utab <- dt[, .N , by = all]
#utab <- table(all) # aggregating at user level

# counting number of unique users
cat(nrow(utab))

# keeping list of users who follow 10+ accounts
userlist <- utab$all[utab$N>=10]
cat(length(userlist)) 
save(userlist, file=userfile)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## creating matrix (sparse)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load(userfile)
n <- length(userlist) # users in dataset
m <- length(accounts) # political accounts

rows <- list()
columns <- list()

pb <- txtProgressBar(min=1,max=m, style=3)
# i = 1
for (i in 1:m){
  to_add <- which(userlist %in% followers.list[[i]])
  rows[[i]] <- to_add
  columns[[i]] <- rep(i, length(to_add))
  setTxtProgressBar(pb, i)
}

# Count Number of accounts followed by each political account
length(rows)
sapply(rows, length)

rows = unlist(rows)
columns = unlist(columns)

# preparing sparse Matrix
library(Matrix)
y <- sparseMatrix(i=rows, j=columns)
rownames(y) <- as.vector(userlist)
colnames(y) <- accounts
save(y, file=matrixfile)

