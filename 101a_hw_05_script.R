
data(vehicles)
head(vehicles)

#a. How many unique vehicle makers (variable `make`) are included in the dataset? 
length(unique(vehicles$make))
#b. How many vehicles made in 2014 are represented in the dataset?
nrow(vehicles %>% filter(year == "2014")) 
#c. For the year 2014, what was the average city mpg (gas mileage) for all compact cars? What was the average city mpg for midsize cars in 2014?
vehicles %>% filter( year == 2014, class == "Compact Cars") %>% summarise(avg_cty_mpg = mean(cty))
vehicles %>% filter(year == 2014, class == "Midsize Cars") %>% summarise(avg_cty_mpg = mean(cty))

#d. For the year 2014, compare makers of midsize cars. Find the average city mpg of midsize cars for each manufacturer. For example, in 2014, Acura has 5 midsize cars with an average city mpg of 20.6, while Audi has 12 midsize cars with an average city mpg of 19.08. 
table <- vehicles %>% filter(year == 2014, class == "Midsize Cars") %>% 
  group_by(make) %>% summarise(avg = mean(cty)) %>% 
  arrange(desc(avg))
head(table, 27)


#             make     1994     1999     2004     2009     2014
# 1          Acura       NA 16.50000 17.33333 17.00000 20.60000
# 2           Audi       NA 15.25000 16.20000 15.83333 19.08333

table_e <- vehicles %>% filter(year %in% c(1994, 1999,2004,2009, 2014), class == "Midsize Cars")%>% group_by(make, year)%>% summarise(avg= mean(cty)) %>% arrange(desc(avg)) %>%  spread(year, avg) %>% print(tibble, n=40)
# load("dr4.Rdata")
load("dr4.Rdata")
table_d <- dr4 %>% mutate(diff21 = visit2-visit1, diff32 = visit3-visit2, diff43 = visit4-visit3, diff54 = visit5-visit4,
                          totaldiff = (ifelse(is.na(diff54),0,diff54)) + (ifelse(is.na(diff43),0,diff43)) + 
                            (ifelse(is.na(diff32),0,diff32)) + (ifelse(is.na(diff21),0,diff21)), totalhappen = 
                            (ifelse(is.na(diff54),0,1)) + (ifelse(is.na(diff43),0,1)) + (ifelse(is.na(diff32),0,1)) + 
                            (ifelse(is.na(diff21),0,1)))
totaldays = sum(table_d$totaldiff)
totalevents = sum(table_d$totalhappen)
average_time = totaldays/totalevents 
print(average_time)

# Part 3. Scrape baseball-reference.com with rvest
library(rvest) 
library(dplyr) 
library(stringr) 

team <- read_html("http://www.baseball-reference.com/teams/") # get the names of the 30 teams 
names <- html_nodes(team, ".left a") # extract text from the html names 
list <- html_text(names) # verify there are 30 teams

num_teams = length(list)

session <- html_session("http://www.baseball-reference.com/teams/") # start html session called session
baseball <- list() # create an emptylist called baseball

for(i in list[1:num_teams]) {
  franchise_html <- session %>% follow_link(i) %>% read_html()
  baseball[[i]] <- franchise_html %>% html_nodes("#franchise_years") %>% html_table()
  baseball[[i]]$current_name <- franchise_html %>% html_nodes("tr:nth-child(1) td:nth-child(2)") %>% html_text()
} #representing the 1st and 2nd element
head(baseball[2]) # To check if the row for 1965 Milwaukee Braves come out as 'Atlanta Braves'

newteams <- plyr::ldply(baseball, data.frame) 
baseball <- newteams[,-1] # because there are repeated variable, we need to remove one
dim(baseball) # we get 2684 rows
head(baseball)

library(rvest)
library(stringr)
all.equal(charToRaw(baseball$Tm[1]), charToRaw("Arizona Diamondbacks"))
char_cols <- which(lapply(baseball, typeof) == "character")
for(i in char_cols) {
  baseball[[i]] <- str_conv(baseball[[i]], "UTF-8")
  baseball[[i]] <- str_replace_all(baseball[[i]],"\\s"," ")
}
all.equal(charToRaw(baseball$Tm[1]), charToRaw("Arizona Diamondbacks"))
# Part 4. dplyr to summarize the baseball data
library(dplyr)

baseball_summary <- filter(baseball, Year >= "2001" & Year <= "2019") %>% group_by(current_name) %>% summarise(wins = sum(W), losses = sum(L), Run_scored = sum(R), Run_allowed = sum(RA), win_percent = wins/(wins+losses)) %>% arrange(desc(win_percent))
print.data.frame(baseball_summary)

