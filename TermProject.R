library(shinythemes)
library(stats)
library(shiny)
library(ggplot2) 
library(reshape)
library(dplyr)
library(DT)
library(stringr)
library(plyr)
library(xtable)
library(XML)
library(data.table)
library(sqldf)
library(tcltk)
library(RSQLite)
library(httr)
options(digits=3)
options(scipen = 999)
setwd("D:/BU/CS688/TermProject/Code")
set.seed(12345)  # for reproducibility

soccerdb = dbConnect(SQLite(), dbname="database.sqlite")
dbListTables(soccerdb)
tbs = as.list(dbListTables(soccerdb))
tbs

#########################################################
# 1. Data Preparation
#########################################################
# 1.1 - sqlite extracts
for (tb in tbs) {
  readtable <- paste0(tb," <- tbl_df(dbGetQuery(soccerdb,'SELECT * FROM ",tb,"'))")
  eval(parse(text=readtable))
}
head(Player)
head(Country)
head(Player_Attributes)
str(Player_Attributes)
head(Team)
head(Team_Attributes)
head(League)
head(Match)
Match= Match[which(Match$league_id=='1729'), ]
premier.league = data.frame(Match$season,Match$date,Match$match_api_id,
                            Match$home_team_goal,Match$away_team_goal)

head(premier.league)
premier.league$MatchResult <- sign(Match$home_team_goal - Match$away_team_goal)


premier.league.home = data.frame(Match$match_api_id,Match$home_team_api_id)
premier.league.away = data.frame(Match$match_api_id,Match$away_team_api_id)
team.short = data.frame(Team$team_api_id,Team$team_long_name,Team$team_short_name)

colnames(premier.league.home) =c("match_api_id","team_api_id")
colnames(premier.league.away) =c("match_api_id","team_api_id")
colnames(team.short) =c("team_api_id","team_long_name","team_short_name")
colnames(premier.league) =c("Season","Date","match_api_id","home_team_goal","away_team_goal","Result")

head(premier.league)
premier.league.home = merge(premier.league.home,team.short,by="team_api_id")
premier.league.away = merge(premier.league.away,team.short,by="team_api_id")

premier.league.combined = merge(premier.league.home,premier.league.away, by="match_api_id")
premier.league.combined = premier.league.combined[,c(1,3,4,6,7)]
head(premier.league.combined)

premier = merge(premier.league,premier.league.combined,by="match_api_id")

head(premier,5)
colnames(premier) =c("MatchID","Season","Date","HomeGoals","AwayGoals",
                     "Result","HomeTeam","HomeShort","AwayTeam","AwayShort")

premier= premier[,c(1,2,3,7,8,9,10,4,5,6)]

premier.df <- na.omit(premier)
head(premier.df)

unique(premier.df$AwayTeam)
teams = unique(c(as.character(premier.df$HomeTeam), as.character(premier.df$AwayTeam)))
seasons <- unique(premier.df$Season)

DT <- data.table(premier.df)

Season.Home.df =sqldf('SELECT Season, HomeTeam,COUNT(MatchID) AS GamesPlayed,
                      SUM(HomeGoals) GoalsFor,(1.0 *SUM(HomeGoals)/COUNT(MatchID)) as AvgGoalsFor,
                      SUM(AwayGoals) as GoalsAgainst,(1.0 * SUM(AwayGoals)/COUNT(MatchID)) as AvgGoalsAgainst,
                      SUM(Result) FROM DT GROUP BY Season,HomeTeam')


Season.Away.df= sqldf('SELECT Season, AwayTeam,COUNT(MatchID) AS GamesPlayed,
                      SUM(AwayGoals) GoalsFor,(1.0 *SUM(AwayGoals)/COUNT(MatchID)) as AvgGoalsFor,
                      SUM(HomeGoals) as GoalsAgainst,(1.0 * SUM(HomeGoals)/COUNT(MatchID)) as AvgGoalsAgainst,
                      SUM(Result) FROM DT GROUP BY Season,AwayTeam')

Home.df =sqldf('SELECT HomeTeam,COUNT(MatchID) AS GamesPlayed,
               SUM(HomeGoals) GoalsFor,(1.0 *SUM(HomeGoals)/COUNT(MatchID)) as AvgGoalsFor,
               SUM(AwayGoals) as GoalsAgainst,(1.0 * SUM(AwayGoals)/COUNT(MatchID)) as AvgGoalsAgainst,
               SUM(Result) FROM DT GROUP BY HomeTeam')


Away.df= sqldf('SELECT AwayTeam,COUNT(MatchID) AS GamesPlayed,
               SUM(AwayGoals) GoalsFor,(1.0 *SUM(AwayGoals)/COUNT(MatchID)) as AvgGoalsFor,
               SUM(HomeGoals) as GoalsAgainst,(1.0 * SUM(HomeGoals)/COUNT(MatchID)) as AvgGoalsAgainst,
               SUM(-Result) FROM DT GROUP BY AwayTeam')

Home.df = Home.df[which(Home.df$GamesPlayed == 152),]
Away.df = Away.df[which(Away.df$GamesPlayed == 152),]

Home.df.Total  <- c(sum(Home.df$GamesPlayed),sum(Home.df$GoalsFor),sum(Home.df$AvgGoalsFor/10),
                    sum(Home.df$GoalsAgainst),sum(Home.df$AvgGoalsAgainst/10),sum(Home.df$Result))
Away.df.Total  <- c(sum(Away.df$GamesPlayed),sum(Away.df$GoalsFor),sum(Away.df$AvgGoalsFor/10),
                    sum(Away.df$GoalsAgainst),sum(Away.df$AvgGoalsAgainst/10),sum(Away.df$Result))

# Attack and Defense strength, based on average goals scored home and away - based on team's average goals against total average goals
Home.df.strengths = data.frame(Home.df$HomeTeam,Home.df$GamesPlayed,
                               Home.df$GoalsFor,Home.df$AvgGoalsFor,(Home.df$AvgGoalsFor/Away.df.Total[3]),
                               Home.df$GoalsAgainst,Home.df$AvgGoalsAgainst,(Home.df$AvgGoalsAgainst/Away.df.Total[5]))
colnames(Home.df.strengths) <- c("Team", "GamesPlayed","GoalsFor","GoalsForAvg","AttackStrength",
                                 "GoalsAgainst","GoaslAgainstAvg","DefenseStrength")

Away.df.strengths = data.frame(Away.df$AwayTeam,Away.df$GamesPlayed,
                               Away.df$GoalsFor,Away.df$AvgGoalsFor,(Away.df$AvgGoalsFor/Home.df.Total[3]),
                               Away.df$GoalsAgainst,Away.df$AvgGoalsAgainst,(Away.df$AvgGoalsAgainst/Home.df.Total[5]))

colnames(Away.df.strengths) = c("Team", "GamesPlayed","GoalsFor","GoalsForAvg","AttackStrength",
                                "GoalsAgainst","GoaslAgainstAvg","DefenseStrength")

home.away.combined = merge(Home.df.strengths,Away.df.strengths,by="Team")
home.away.combined = home.away.combined[,c(1,2,3,4,5,6,7,8,10,11,12,13,14,15)]
colnames(home.away.combined) = c("Team","GamesPlayed","HomeGoalsFor","HomeGoalsForAvg","HomeAttackStrength",
                                 "HomeGoalsAgainst","HomeGoalsAgainstAvg","HomeDefenseStrength",
                                 "AwayGoalsFor","AwayGoalsForAvg","AwayAttackStrength","AwayGoalsAgainst",
                                 "AwayGoalsAgainstAvg","AwayDefenseStrength")
# Calculating the goal expectation, based on team strengths and 
home.away.combined$AvgHomeGoalExp = round(home.away.combined$HomeAttackStrength * home.away.combined$AwayDefenseStrength * (Home.df.Total[5]), digits = 2)
home.away.combined$AvgAwayGoalExp = round(home.away.combined$AwayAttackStrength * home.away.combined$HomeDefenseStrength * (Away.df.Total[5]), digits = 2)

strength.Table = home.away.combined[c(1,3,4,5,6,7,8,9,10),c(1,5,8,11,14,15,16)]
strength.Table
head(DT)
par(mfcol = c(2, 1), mar = rep(2.2, 4))
hist(c(DT$AwayGoals, DT$HomeGoals), 
     xlim = c(-0.5, 10), breaks = -1:10 + 0.5, main = "Distribution of Goals\nScored by a Team in a Match.")

mean_home_goals = mean(DT$HomeGoals)
mean_away_goals = mean(DT$AwayGoals)

var_home_goals =var(DT$HomeGoals)
var_away_goals = var(DT$AwayGoals)

mean_goals <- mean(c(DT$AwayGoals, DT$HomeGoals))
hist(rpois(9999, mean_goals), xlim = c(-0.5, 8), breaks = -1:9 + 0.5, main = "Poisson Distribution with\nthe Same Mean as the Distribution Above.")

attach(home.away.combined)

glm(formula = home.away.combined$AvgHomeGoalExp * 1000 ~ home.away.combined$HomeAttackStrength,data = home.away.combined, family = poisson)


model <- glm(AvgHomeGoalExp ~ HomeAttackStrength + Team, family=poisson(link=log), data=home.away.combined)
summary(model)

par(mfrow = c(2, 1))
x = home.away.combined$HomeGoalsForAvg
lambda= home.away.combined$AvgHomeGoalExp
  
ggplot(data.frame(x=x), aes(x)) +
  geom_point(aes(y=dpois(x, lambda)), colour="red")

dpois(1,1.8)

# Define UI for application 

mytoken = '2ad3265054df40faa54f055280fa3364'
api_call <- function(request, token=mytoken, response = "full"){
  url <- paste0("http://api.football-data.org/",request)
  output <- httr::GET(url,httr::add_headers(`X-Auth-Token` = token, `X-Response-Control` = response))
  return(jsonlite::fromJSON(httr::content(output, as = "text", encoding = "UTF-8")))
}


## List of competitions from the football-data.org API
##
## Returns list of competitions from the football-data.org API
## @param season (sting) The season (e.g. "2015") to which the competitions relate. 
## Defaults to the current season.

##################################################################################################
list.Competitions <- function(season="", token=mytoken, response = "minified"){
  url <- paste0("v1/competitions/",ifelse(season=="","",paste0("?season=",season)))
  api_call(url, token, response)
}

comp.df.2015= list.Competitions("2015",response = "minified")
comp.df.2016= list.Competitions("2016",response = "minified")
comp.df = rbind(comp.df.2015,comp.df.2016)

input.comp.df <- data.frame(lapply(comp.df, as.character), stringsAsFactors = FALSE)
input.comp.df <- as.list(input.comp.df)
head(comp.df,5)

league.choices <- as.list(input.comp.df$id)
names(league.choices) <- input.comp.df$caption

##################################################################################################

list.Comp.Teams <- function(id, token=mytoken, response = "full"){
  url <- paste0("v1/competitions/",id,"/teams")
  api_call(url, token, response)
}


Comp.Teams.FUN <- function(id) {
  temp = list.Comp.Teams(id,response = "minified")
  temp$leagueid= id
  return (temp)
}


Comp.Teams = lapply(input.comp.df$id, function(x) {Comp.Teams.FUN(x)}) 
head(Comp.Teams)

Comp.Teams[2]
Comp.Teams[[1]]$leagueid
##################################################################################################
league.Table = function(id, matchDay = "", token=mytoken, response = "full"){
  url <- paste0("v1/competitions/",id,"/leagueTable",
                ifelse(matchDay=="","",paste0("?matchday=",matchDay)))
  api_call(url, token, response)
}

league.Table.FUN <- function(id) {
  x = league.Table(id,response = "minified",matchDay = "")
  x= as.matrix(x)
  x=c(x[1],x[2],x[3])
  x =data.frame(x)
  x = x[,c(3,4,6,8,9,10,11)]
  colnames(x) = c("Rank","Team","Games Played","Points","Goals Scored","Goals Against","Goal Difference")
    return (x)
}

temp1 =league.Table("426",response = "minified",matchDay = "15")
temp1 = as.matrix(temp1)
temp1=c(temp1[1],temp1[2],temp1[3])
temp1 =data.frame(temp1)
temp1 = temp1[,c(3,4,6,8,9,10,11)]
colnames(temp1) = c("Rank","Team","Games Played","Points","Goals Scored","Goals Against","Goal Difference")
temp1
temp2 =
  temp1 %>%
  filter( Rank >= as.numeric(1),
          Rank <= as.numeric(2)
  )

team.Players <- function(id, token=mytoken, response = "full"){
  url <- paste0("v1/teams/",id,"/players")
  api_call(url, token, response)
}


##################################################################################################





ui = shinyUI(fluidPage(shinythemes::themeSelector(),
                       
  
  # Application title
  headerPanel("European Soccer Leagues"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
             mainPanel(sidebarPanel(
            (selectInput("leagueInput", "League",choices = league.choices)),
             height = 20, width = 20
             ),
            tabsetPanel(tabPanel("Standings", tableOutput("standings")),
                        tabPanel("Team Points", plotOutput("pointsplot"),width = "100%",
                        sliderInput("topNInput", "Teams Slider", 0, 20, c(1, 5), pre = "#")),
                        tabPanel("Strength", tableOutput("strength"))
                            )
                          )
                        )
                      )

server <- function(input, output, session) {
  output$standings = renderTable({ 
    filtered1 = 
      lapply(as.numeric(input$leagueInput[1]), function(x) {league.Table.FUN(x)}) 
    filtered2  = reactive({ filtered1 })
    filtered2()
  })

    output$pointsplot = renderPlot({
    filtered1 = 
      data.frame(lapply(as.numeric(input$leagueInput[1]), function(x) {league.Table.FUN(x)}))
    filtered2 =
      filtered1 %>%
      filter( Rank >= as.numeric(input$topNInput[1]),
              Rank <= as.numeric(input$topNInput[2])
      )
    ggplot(filtered2,aes(Team,Points),fill=Points) +   
      geom_bar(aes(fill = Points), stat = "identity") 
  },height = 500, width = 1200)
    
    output$strength = renderTable({ 
      strength.Table
    })
    
 }

shinyApp(ui = ui, server = server)
###########################################################################

