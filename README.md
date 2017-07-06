# Soccer-Leagues-Prediction

1.	Objective 
To present web scraping soccer data using API calls and ShinyR and to show how goals scored in EPL is a Poisson Distribution

2.	Outcome
8 season worth of English Premier League data was analyzed using R programming language, and goals distribution is found to be a Poisson Distribution. 

3.	Two Data Sources

  •	Football-data API provides football data of all major european leagues in a machine-readable way. Data includes fixtures, teams, players, results and more via a RESTful API in JSON (only) representation.
  •	Kaggle European Soccer Database - Since the API data is available only for 2015/2016 and 2016/2017, collected more soccer data for previous years from a database for analysis

4.	Data Import and Cleaning in R
  •	Football-data API
  
  First task was to register for a free access API key. Using the API access key, a generic function in ‘R’ was built to get data from the website. Subsequently, function calls were built to collect data for:
  Competitions – All major soccer leagues in Europe
  Teams – Teams participating in each competition
  League Table – For each of the competition, standings of teams
  
  •	Kaggle European Soccer Database 
  
  Downloaded ‘database.sqlite’ file and used SQLite to get a list of:
    Matches
    Leagues 
    Teams
    Results
  Used filter to collect data for only English Premier League (8 Seasons - 2008 to 2014)

5.	Analysis – Historical Data

  The data available at Football-data API is only for 2 years. 
    •	Not a sufficient time frame to create any meaningful analysis
    • Not enough to build any models either. 

  The lack of depth in available data created a need to access more detailed, historical data. Kaggle European Soccer Database  is a good source for Soccer game analysis. 
    •	The data set includes 25K+ matches, players, teams etc… across major leagues of Europe. 
    •	English Premier League data is available for 8 seasons (compared to 1.5 seasons in the API)

6.	Poisson Distribution

  A discrete probability distribution that expresses the probability of a given number of events occurring in a fixed interval of time and/or space if these events occur with a known average rate and independently of the time since the last event.
  Distribution often used to model the number of incidences of some characteristic in time or space:
    •	Arrivals of customers in a queue
    •	Numbers of flaws in a roll of fabric
    •	Number of typos per page of text.
        e.g English Premier League

  Total Goals Per Game (Both Teams)
        Mean=1.4
  Goals by Team 
        Home Team:   Mean=1.6  Variance=1.7
        Away Team:   Mean=1.2  Variance=1.3






