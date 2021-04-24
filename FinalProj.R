#'## Lahman Baseball Database
#'### Also uses: "dplyr"package.
#'### Introduction:
#'##### The Lahman baseball package is a database used to import Baseball statistics into the R environment as R data frames. This package includes batting, pitching and fielding data from the 1871 to 2020 seasons. Deep data visualization is changing the game of baseball. The sport has always been a game of numbers and this lends the sport as a great example of how data visualization can change the game. There are statistics for just about everything that happens in a game and the Lahman database stores historical information in an easy to import database for use in R. Without the raw data from a database, data driven decisions about a player's performance would not be possible. 
#'
#'##### In this project I will use the Lahman package to visualize different baseball trends all surrounding my favorite team, the New York Yankees. I will use a combination of the Lahman package and the standard R "plot" & "lines" functions to show Derek Jeter's batting average over his entire MLB career. This example will highlight the basic usage of the Lahman database to extract raw baseball statistics. After importing the data from the Lahman database and using plot and lines to show Jeter's batting average I will use the "dplyr" package to make data extraction from the database easier. After that I will import data on my favorite pitcher from Williamsport, PA; Mike Mussina. I will use the dplyr package to extract data from the Lahman database to analyze Mussina's earned run average for his tenure with the Yankees and the Baltimore Oriole's.  
#'
#'### Derek Jeter's Career Batting Average:
#'##### In baseball, batting average is calculated using total at-Bats (AB) / total Hits(H). At-bats is the number of times a player has been up to the plate. This number does NOT include walks, hit by pitch or sacrifices. These stats are not counted as an at-bat, for example, if a player comes up to bat four times in a game and records, one hit, a walk and two strikeouts then he officially had three at-bats and got one hit. To calculate his batting average for the game; 1/3 = (.333) that is his average for the day (Notice the walk does not count as an at-bat and batting averages in baseball are rounded to three decimal places). In baseball a batting average is considered a decent measure of a batters performance, with .300 representing an above average hitter and average usually falls around .275, but that is changing as the game changes (it is dropping on average).
#'
require(Lahman) # Import the Lahman database
require(dplyr) # Import dplyr package
data("Batting") # Load the Batting subset from Lahman
batting <- battingStats()
JeterBatting <- batting[batting$playerID=="jeterde01",] # Create new data frame that includes all batting statistics from Lahman database for Derek Jeter using his playerID ("jeterde01").
head(JeterBatting)
#'##### Now we have all of Jeter's batting information saved in the "JeterBatting" variable, next we can extract exactly what we want from it; At-bats and Hits.
jeterAB <- sum(JeterBatting$AB) # Total at-bats 
jeterHits <- sum(JeterBatting$H) # Total Hits
(jeterCareerAVG <- (jeterHits / jeterAB)) # Here is the simple career batting AVG calculation.
#'##### Here we see Jeter's career batting average, which is quite good for such a long career. Typically batting AVG is shown as a 3 digit number so it should be rounded.
round(jeterCareerAVG,digits=3) # Use the round function to display 3 digits which is most common for batting average. 
#'##### A nice rounded batting average calculated from raw data imported with the Lahman package.
#'### Derek Jeter's Batting average per season, plotted.
#'##### Now that we have our feet wet with the Lahman database we can start to visualize some statistics over Jeter's career.
yrAVG <- (JeterBatting$H / JeterBatting$AB) # Create batting average per year
plot(JeterBatting$yearID, yrAVG, main="Jeter's AVG each year of his career",xlab="Year", ylab ="AVG") # Load the plot function and add labels to the plot. 
lines(JeterBatting$yearID, yrAVG, col="blue") #Joins the data from plot() and add a color.
#'##### After this first example it becomes evident that while Lahman is a great database, it requires a lot of data manipulation to perform a request. Here is where the great "dplyr" package can be helpful. I will not go into great detail about the package as it is not the main focus of this project. What "dplyr" excels at is data manipulation by using consistent verbs almost like SQL statements. It also allows logical and boolean operations on data frames in R. Next we will see "dplyr" in use to gather some pitching information. 
#'### Lahman and dplyr 
#'##### Using the Lahman database with the dplyr package to manipulate data frames grammatically. We will compare pitcher Mike Mussina's earned run average (ERA) for his tenure with Baltimore and the Yankees. ERA is generally used to compare a pitchers effectiveness, it is calculated using Earned Runs allowed vs. Innings Pitched. An ERA under 4.0 is considered good for a pitcher and the lower the ERA the better. 
#'#####  
AllPitching = merge(Pitching, People, by ="playerID") # Create new data frame by merging all data from Pitching and People tables from Lahman database.
mussPitching = AllPitching %>% # The %>% notation is used as a pipe for the dplyr package and is used to perform a combination of commands. 
  filter(playerID == "mussimi01")# Create a new data frame only including Mike Mussina's information. This is using the dplyr package to "filter" data and use boolean logic against the Lahman database. 
mussBALstats <- mussPitching %>% 
  select(ERA, yearID, teamID) %>% # Select ERA, yearID and teamID from data. dplyr uses a simple verb syntax 
  filter(teamID == "BAL") # Only show years Mussina pitched for Baltimore
#'#### Mike Mussina's career pitching stats with Baltimore
mussBALstats %>%
  arrange(desc(yearID)) # Use the arrange function of dplyr to list his pitching stats by year. Notice the easy syntax. 
#'##### Now we have a nice table that will have all the information we need to calculate Mussina's average ERA for the ten years he pitched for Baltimore and can compare it. And it was easy to extract the data and format it by using "dplyr". Now the same thing should be done to get his pitching information for the years he was a Yankee.
mussNYYstats <- mussPitching %>% # Use pipes in dplyr to string together commands
  select(ERA, yearID, teamID) %>%
  filter(teamID == "NYA") # Filter by "NYA" as the NY Yankees are stored in the Lahman database as their original team name "New York Highlanders".
#'#### Mike Mussina's career pitching stats with the Yankees.
mussNYYstats %>%
  arrange(desc(yearID)) # Arrange list by year again.
#'##### Now that we have our data formatted in two separate data frames (mussBALstats & mussNYYstats), we can use the dplyr mutate function to perform column by column calculations. 
mussERAavgBAL <- mussBALstats %>%
  pull(ERA)# Use dplyr "pull" to return a set of columns as a new vector.
mussERAavgBAL <- mean(mussERAavgBAL) # Calculate the mean ERA for his time in Baltimore
#'##### Now we do the same to his years with the Yankees
mussERAavgNYY <- mussNYYstats %>%
  pull(ERA) 
mussERAavgNYY <- mean(mussERAavgNYY)
mussERAavgNYY # A good ERA for the second half of his career!
mussERAavgBAL # This is the better ERA but to be so consistent over a 18 year career is remarkable.
#'##### Seems he was a slightly more effective pitcher during his tenure with Baltimore. 
#'### Plotting Mike Mussina's ERA throughout his career:
boxplot(mussBALstats$ERA, mussNYYstats$ERA, main= "Mike Mussina's ERA", sub= "Lower ERA is Better", xlab= "Team", ylab="ERA", font.sub=3 , cex.sub=.8, names = c("Baltimore", "Yankees"), col = rainbow(2)) # Create a boxplot comparing Mussina's ERA from Baltimore to New York. Include x/y/main/sub lables set font size and color each boxplot using R's rainbow option. 
#'##### What a consistent career ERA for Mike Mussina!
#'
#'##### I hope you enjoyed learning about the incredibly useful Lahman database, which enables baseball statistical analysis throughout the history of America's pastime. Combining this massive database with the dplyr tool you can make powerful observations about the game quite easily. During the research for this project I also found the "Seamheads" database which is the Lahman package equivalent for the Negro Leagues. It is great that this statistical information is recorded so we can forever save and continue to analyze baseball history of both MLB and the often forgotten Negro leauges. Without these databases, baseball history wouldn't be so rich and we couldn't make interesting historical analysis. 


