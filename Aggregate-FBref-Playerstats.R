source('FBref-scraper.R')

prevYrs=NULL #null for this season's data, 1 for last season's, 2 for two seasons ago, etc.

outfile='top5players-curr.csv'

#df with page names and matching table names
leaguevals<-data.frame(leaguename=c('Bundesliga', 'Premier-League',
                                    'Serie-A', 'La-Liga', 'Ligue-1'),
                       leaguenumber=c('20', '9',
                                      '11', '12', '13'))

################
### do work ####
################

## initial scraping ####


#currently missing playing time, will need to remove players with zero mins if we want to add that in
tablenames<-c('standard', 'shooting',
              'passing', 'passing_types', 'gca', 'defense',
              'possession', 'misc')


for (league in 1:nrow(leaguevals)){#league is the row index number within the leaguevals df
  
  leaguename<-leaguevals$leaguename[league]
  leaguenumber<-leaguevals$leaguenumber[league]
  
  #define the leagueurl variable to feed to the function we created
  for (table in tablenames){
    if(table=='standard'){ #the 'standard' table has a different url structure
      leagueurl<-paste0("https://fbref.com/en/comps/", leaguenumber, "/stats/", leaguename, "-Stats")
    }else{
      leagueurl<-paste0("https://fbref.com/en/comps/", leaguenumber, "/", table, '/', leaguename, "-Stats")
    }
    
    
    #use our function get the table from the url, stored in df called thisLeague
    
    if (table==tablenames[1]){#initialize df if its the first table for the league
      thisLeague<-getFBrefStats(leagueurl, paste0('#stats_',table), numYearsBack = prevYrs)
    }
    else{#after that add to the df
      tmp = getFBrefStats(leagueurl, paste0('#stats_',table), numYearsBack = prevYrs)
      thisLeague<- cbind(thisLeague, tmp)
    }
    
  }#end for page loop
  
  #add league name to df in case we need it in analyses
  thisLeague$League<-c("League", rep(leaguename, nrow(thisLeague)-1))
  
  if (league==1){ #for the first league, initialize our final dataframe called players
    players<-thisLeague
  }
  else{ #after the first one, add to players
    players<-rbind(players, thisLeague[-1,]) #remove the first row which is variable names
  }
  
}#end league loop


#uncomment to save the raw data in case of crashes
#write.csv(players, 'top5playersraw.csv')


## reformat to make the table easier to use ####

formatted<-players[-1,]
colnames(formatted)<-players[1,]
library(tidyverse)


#rename a bunch of columns

library(tidyverse)

#make a new df called formatted that we'll manipulate 
#remove the first row values and replace the column names with those values
formatted<-players[-1,] 
colnames(formatted)<-players[1,] 

#importantly, we still have the original column names stored in players

#df containing string for marker in top column of players colnames and addon for those columns
replacevals<-data.frame(tag=c('Per 90 Minutes',  
                              'Total', 'Short', 'Medium', 'Long', 
                              'Pass Types','Corner Kicks', 'Height', 'Body Parts', 'Outcomes',
                              'SCA Types', 'GCA Types', 'Tackles', 'Vs Dribbles',
                              'Pressures', 'Blocks', 'Touches', 'Dribbles', 'Carries', 'Aerial Duels'),
                        prefix=c('per90', 
                                 'Pass', 'ShPass', 'MedPass', 'LngPass',
                                 'Pass','CK', 'Pass', 'Pass', 'Pass',
                                 'SCA', 'GCA', 'Tkl', 'vDrib',
                                 'Press','Block', 'Touches','Drib', 'Carry', 'Aer'))
#loop through the df and add prefixes to variables in formatted
for (i in 1:nrow(replacevals)){
  names(formatted)[names(players)==replacevals$tag[i]]<-
    paste0(
      replacevals$prefix[i],
      names(formatted)[names(players)==replacevals$tag[i]]
    )
}

#remove duplicate columns
formatted<-formatted[,which(!duplicated(names(formatted)))] #original scraped data with weird first row

#change some now redundant names
formatted<-formatted %>% 
  rename(Carries=CarryCarries, Touches=TouchesTouches)

#a few variables are here twice, but one of the two for each are similarly redundant names
#remove the columns with redundant names
formatted<-formatted %>% select(-c(TklTkl, BlockBlocks, PressPress, TklTklW))

#remove rows that are just variable names
formatted<-subset(formatted, Player!='Player')

#remove comma in Min column so we can convert it to a number
formatted$Min<-str_replace(formatted$Min, ",", "")

#separate number variables from character variables
#at this point, anything that returns NA from as.numeric is not a true number
#we'll apply as.numeric to everything, then whatever is not an NA is a true number
numbers<-formatted %>% mutate_all(as.numeric) %>% select_if(~ any(!is.na(.)))

#use the number df to identify character variables
#anything not a number is a character
chars<-formatted %>% select(!any_of(names(numbers))) #chars

#merge them back now that the numbers are numeric type
formatted<-cbind(chars, numbers) 

#remove things that are already per 90 (we want to convert everything to per 90)
formatted<-subset(formatted, select=which(!grepl( "90" , names(formatted))))

#identify which variables should not be converted to per 90, including character variables
dontconvert<-which(grepl(pattern = 'Player|Pos|Matches|Squad|League|/Sh|/SoT|%|Min|MP|Starts|Poss|Age|Born|Nation', names(formatted)))


#convert everything else to per 90s
for (row in 1:nrow(formatted)){
  formatted[row,-dontconvert]<-formatted[row,-dontconvert]/(formatted$Min[row]/90)
}

#rename some stray columns
colnames(formatted)[colnames(formatted)=="1/3"]<-'PassF3'
colnames(formatted)[colnames(formatted)=="Prog"]<-'ProgPasses'

#remove % sign in some column names
colnames(formatted)<-str_replace(colnames(formatted), "%", "perc")

#remove / sign in some column names
colnames(formatted)<-str_replace(colnames(formatted), "/", "per")

#remove spaces in column names
colnames(formatted)<-str_replace(colnames(formatted), " ", "")

#remove the colon in column names
colnames(formatted)<-str_replace(colnames(formatted), ":", "")

#remove Rk which is a nonsense variable
colnames<-select(formatted, -Rk)

## save out ####
write.csv(formatted,outfile)
