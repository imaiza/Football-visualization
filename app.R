# We load the libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(shinyWidgets)
library(ggplot2)
library(ggcorrplot)
library(scales)
library(maps)
library(mapproj)
library(lubridate)
library(data.table)
library(RColorBrewer)

getwd()
#setwd("/Users/inigo/Desktop/Master Data Science/BigData and DataVisualization/Data Visualization/Project/")

# Load the data

df = read.csv('results.csv')

# Some feature engineering to add relevant features

game_outcome <- function(home_score, away_score) {
  outcome <- "D"
  if (home_score > away_score) {outcome <- "H"}
  if (home_score < away_score) {outcome <- "A"}
  return(outcome)
}

winning_team <- function(home_score, away_score, home_team, away_team) {
  winning_team <- NA
  if (home_score > away_score) {winning_team <- home_team}
  if (home_score < away_score) {winning_team <- away_team}
  return(winning_team)
}

losing_team <- function(home_score, away_score, home_team, away_team) {
  losing_team <- NA
  if (home_score < away_score) {losing_team <- home_team}
  if (home_score > away_score) {losing_team <- away_team}
  return(losing_team)
}


df2 <- df %>%
  # mutate(year = format(date),
        # month = format(date),
        # dayofweek = format(date)) %>%
  rowwise() %>%
  mutate(outcome = game_outcome(home_score, away_score),
         winning_team = winning_team(home_score, away_score, home_team, away_team),
         losing_team = losing_team(home_score, away_score, home_team, away_team)) %>%
  ungroup()

# Separate year month and day:

df2 <- separate(df2, "date", c("year", "Month", "Day"), sep = "-")

# Before calling the app, we do the needed data processing part for each part: 


###############################
#### 1ºPlot: Nº matches #######
###############################


tmp <- df2 %>%
  filter(year < 2018) %>%
  mutate(year = as.numeric(year)) %>%
  group_by(year) %>%
  summarise(nb_games = length(outcome))  %>%
  ungroup()

wc_years <- c(1930, 1934, 1938, seq(1950, 2014, 4))

tmp <- tmp %>%
  mutate(is_wc = year %in% wc_years)

##########################################
#### 2ºPlot: Best teams per decade #######
##########################################

# Number of games per year per team
all_teams <- data.frame(teams = c(df2$home_team, df2$away_team), year=as.numeric(c(df2$year, df2$year)))

df_team_games_per_year <- all_teams %>%
  filter(year < 2018) %>%
  group_by(teams, year) %>%
  summarise(nb_games = length(year)) %>%
  mutate(year_date=as.Date(paste(year,"-01-01",sep="")))

# Number of victories per year
df_nb_victories <- df2 %>%
  mutate(year=as.numeric(year)) %>%
  select(year, winning_team) %>%
  filter(!is.na(winning_team)) %>%
  group_by(year, winning_team) %>%
  summarise(nb_victories = length(winning_team))

# Number of losses per year
df_nb_losses <- df2 %>%
  mutate(year=as.numeric(year)) %>%
  select(year, losing_team) %>%
  filter(!is.na(losing_team)) %>%
  group_by(year, losing_team) %>%
  summarise(nb_losses = length(losing_team))

# Putting all this together

df_teams_winrate <- df_team_games_per_year %>%
  left_join(df_nb_victories, by=c("year"="year", "teams"="winning_team")) %>%
  left_join(df_nb_losses, by=c("year", "teams"="losing_team")) %>%
  mutate(nb_victories = ifelse(is.na(nb_victories), 0, nb_victories)) %>%
  mutate(nb_losses = ifelse(is.na(nb_losses), 0, nb_losses)) %>%
  mutate(nb_ties = nb_games - (nb_victories + nb_losses))

# We analyze the best team per decade:
######################################

df_teams_winrate_per_decade <- df_teams_winrate %>%
  mutate(decade = cut(year, seq(1870,2020, 10), dig.lab = 4, right=FALSE)) %>%
  group_by(teams, decade) %>%
  summarise(nb_games = sum(nb_games),
            nb_victories = sum(nb_victories),
            nb_losses = sum(nb_losses),
            nb_ties = sum(nb_ties),
            min_year = floor(year/10) * 10) %>% # To have a columns with min_year = first year of decade
  ungroup() %>%
  mutate(winrate = nb_victories / nb_games * 100,
         lossrate = nb_losses / nb_games * 100,
         tierate = nb_ties / nb_games * 100)

df_teams_winrate_per_decade <- unique(df_teams_winrate_per_decade)

# We filter so that we just work with teams with over 10 games played

# Clean the data:

df_teams_winrate_per_decade_cleaned <- df_teams_winrate_per_decade %>%
  filter(nb_games > 30) %>% 
  group_by(decade) %>%
  mutate(min_year = min(min_year)) %>%
  top_n(n=6, wt=winrate) %>%
  ungroup() %>%
  arrange(desc(decade), desc(winrate)) %>%
  mutate(ord = rev(row_number())) %>%
  mutate(decade_year = paste(min_year, "'s", sep=""))


#########################################################
#### 3ºPlot: Best teams per decade with WORDL MAP #######
#########################################################

# install.packages('rworldmap')
library(rworldmap)

df_teams_winrate_per_decade_cleaned2 <- df_teams_winrate_per_decade %>%
  #filter(nb_games > 10) %>% 
  group_by(decade) %>%
  mutate(min_year = min(min_year)) %>%
  #top_n(n=6, wt=winrate) %>%
  ungroup() %>%
  arrange(desc(decade), desc(winrate)) %>%
  mutate(ord = rev(row_number())) %>%
  mutate(decade_year = paste(min_year, "'s", sep=""))

# PROCESAMIENTO DE DATOS SEGUNDA PARTE:

df_p3 <- df
# Process data
#    Add new colum if home wins
df_p3$home_wins <- ifelse(df_p3$home_score - df_p3$away_score & !df_p3$neutral > 0, TRUE, FALSE)
#    Add new column if there is a draw
df_p3$draw <- ifelse(df_p3$home_score == df_p3$away_score, TRUE, FALSE)
#   Change column date to date type
df_p3$date <- as.Date(df_p3$date)

# Get all the tournaments names
tournament_names <- unique(df_p3[c("tournament")])$tournament

# PROCESAMIENTO DE DATOS TERCERA PARTE:

data1 <- subset(df, tournament=='Friendly')

#Organize the data not to misunderstand home and away matches
matches <- vector()
for (i in 1:length(data1$home_team)){
  matches <- c(matches,paste(as.character(min(data1$home_team[i],data1$away_team[i])), as.character(max(data1$home_team[i],data1$away_team[i])),sep='-'))
  
}

data1$match <- matches


##################################
########## SHINY APP #############
##################################

# install.packages("shiny")
library(shiny)

# Define de user interface:

ui <- fluidPage(
  titlePanel("International football matches App"),
  sidebarLayout(
    sidebarPanel(
      h1("What is this App about?"),
      p(),
      p('This App has been created by', strong(' Iñigo Maiza, Fernando Casabán and Jagoba Zuluaga '), 'as an academic project for the Data Visualization course of the MSc in Data Science on the UPM.'),
      p('We used a public dataset taken from', a(" Kaggle ", href = "https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017"), 
        "that includes 41,865 results of international football matches starting from the very first official match in 1972 up to 2019."),
      p(),
      p('This visualizations aim to answer three different questions:'),
      p(strong('1. Historical trends in international football')),
      p(),
      p("Here, we can select different decades to visualize our data. For the second plot (bar plot):"),
      
      # Select input for first plot
      selectInput("var2", 
                  label = "Choose a decade",
                  choices = c("1900's", 
                              "1910's",
                              "1920's", 
                              "1930's",
                              "1940's",
                              "1950's",
                              "1960's",
                              "1970's",
                              "1980's",
                              "1990's",
                              "2000's",
                              "2010's"),
                  selected = "1990's"),
      
      p("In the world map a decade can be selected too (third plot, world map):"),
      
      # Select input for second plot
      selectInput("var1", 
                  label = "Choose a decade",
                  choices = c("1900's", 
                              "1910's",
                              "1920's", 
                              "1930's",
                              "1940's",
                              "1950's",
                              "1960's",
                              "1970's",
                              "1980's",
                              "1990's",
                              "2000's",
                              "2010's"),
                  selected = "1990's"),
      
      # Parte Fer:
      p(strong('2. Relation of hosting-winning tournament:')),
      p('To change the correlation and the heatmal plots we can also modify the values:'),
      
      pickerInput("tournament",p("Select tournament/s"), multiple = T,
                  choices=tournament_names, selected=tournament_names,
                  options = list('actions-box' = TRUE, 'selected-text-format'= paste0("count > ", length(tournament_names)-1),
                                 'count-selected-text' = "All tournaments")),
      
      dateRangeInput("date_range", p("Select a range of dates"), min=head(df_p3,1)$date, max=tail(df_p3,1)$date,
                     start = "2000-01-01", end = "2010-01-01"),
      
      helpText("Dates range from 1872-11-30 to 2020-12-09"),
      
      selectInput("plot_select", p("Select pair of variables to be plotted"),
                  choices = list("Home score and home wins" = "home_score-home_wins",
                                 "Home score and draw" = "home_score-draw",
                                 "Home score and away score" = "home_score-away_score",
                                 "Home score and played on neutral country" = "home_score-neutral",
                                 "Away score and home wins" = "away_score-home_wins",
                                 "Away score and draw" = "away_score-draw",
                                 "Away score and home score" = "away_score-home_score",
                                 "Away score and played on neutral country" = "away_score-neutral"),
                  selected = "home_score-home_wins"),
      
      helpText("This selection will change the heatmap/scatterplot"),
      
      # Parte Jagoba
      p(),
      p(strong('3. Geopolitics relation with international football matches:')),
      p("To modify the range of dates of the last plot:"),
      sliderInput(inputId = 'range',
                  label = 'Period Range',
                  min = as.Date("1871-01-01","%Y-%m-%d"),
                  max = as.Date("2020-12-01","%Y-%m-%d"),
                  value=as.Date(c("1990-01-01","2016-12-01")),
                  timeFormat="%Y-%m-%d")
      
      
      
      
    ),  # cierra sidebarpanel
   
  mainPanel(
    #h1("What trends have there been in international football throughout the ages?"),
    h2(" Historical trends on international football"),
    p(" In this first section, we analyze what trends have there been in international football throughout the decades using different
       visualizations. First we plot the evolution of the number of international football matches (the green segments define world wars)"),
    plotOutput("plot3"),
    p(),
    p("In this first plot we can check how the number of matches has been increasing significantly since the end of the WWII. This increase
       seems to be stabilizing since the 2000's. It is also worth pointing out how the number matches decreases the world cup year. Times of
       war also affect the number of international matches as expected."),
    p(),
    p(" To analyze what countries have dominated different eras of football, we can plot the winrates of the top6 teams on each decade."),
    p(),
    plotOutput("plot2"),
    p(),
    p(' We can check how teams like England domined the first years of football or how Brazil consistently dominated the XX century. We 
      can also see how Spain started being the top country on the 2000 and 2010 decades.'),
    p(),
    p('To visualize the winrates of all the teams of the world and not only the top 6, we can plot a world map with the countries football
       teams winrates. We obtain:'),
    plotOutput("plot1"),
    p(" Here we have to point out that there is not enough data to represent some countries winrate (mainly for years < 1940). Also, we see
       how European and south American countries have the best winrates, while Asian or African teams tend to have lower ones."),
    p(),
    
    # PARTE FER
    p(),
    h2("How much, if at all, does hosting a major tournament help a country's chances in the tournament?"),
    p("This question is about whether there is a correlation between hosting a match and the outcome of the match."),
    br(),
    h4("Correlation plot"),
    plotOutput("corrPlot"),
    p("Pay special attention to correlations involving the neutral variable, for example home_wins with neutral."),
    br(),
    h4("Heatmap plot"),
    p("The intention of this graph is to explore the distribution over time (in most cases) selected in the filter as well 
              as to discover outliers. Please select a pair of variables to plot"),
    plotOutput("scattPlot", click = "scatt_click"),
    p("As mention before, this plot is also useful to find outliers, to do this click on the point that interests you."),
    br(),
    h4("Information about the selected data points"),
    tableOutput("info_point"),
    # Parte Jagoba
    
    p(),
    h2("Geopolitics relation with international football matches"),
    p(" To visualize the relations between cuntries, we can plot the number of",strong("friendly")," matches played between
      countries. We obtain:"),
    h4('Most Frequen Matches',align = "center"),
    plotOutput(outputId = "distPlotLactual"),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    p('It can be apreciated that countries nearby tend to play more friendly matches like Hungary - Austria for example. At the same time,
     we see that European countries tend to play more matches between them.')
  ) # cierra mainpanel
  ) # cierra sidebarlayout
)




# Define the server:

server <- function(input,output){
  
  # Place to put more code
  
  output$plot1 <- renderPlot({
    
    df <- switch(input$var1, 
                  "1900's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1900),
                  "1910's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1910),
                  "1920's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1920),
                  "1930's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1930),
                  "1940's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1940),
                  "1950's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1950),
                  "1960's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1960),
                  "1970's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1970),
                  "1980's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1980),
                  "1990's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 1990),
                  "2000's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 2000),
                  "2010's" = df_teams_winrate_per_decade_cleaned2 %>%filter(min_year == 2010)
                 )
    
    
    joinData <- joinCountryData2Map( df,
                                     joinCode = "NAME",
                                     nameCountryColumn = "teams",
                                     nameJoinColumn = "teams",
                                     verbose = FALSE)
    
    mapCountryData( joinData, nameColumnToPlot="winrate", addLegend=TRUE,colourPalette ='heat',mapTitle = "National football teams' winrates"  )   
    
  })
  
  output$plot2 <- renderPlot({
    
    df_teams_winrate_per_decade_cleaned <- switch(input$var2, 
                 "1900's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1900),
                 "1910's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1910),
                 "1920's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1920),
                 "1930's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1930),
                 "1940's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1940),
                 "1950's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1950),
                 "1960's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1960),
                 "1970's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1970),
                 "1980's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1980),
                 "1990's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 1990),
                 "2000's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 2000),
                 "2010's" = df_teams_winrate_per_decade_cleaned %>%filter(min_year == 2010)
    )
     
    ggplot(df_teams_winrate_per_decade_cleaned, aes(x=ord, y=winrate, fill=teams)) + 
      geom_bar(stat="identity") +
      facet_wrap(~decade_year, scales="free_y") +
      coord_flip() +
      scale_x_continuous(labels=df_teams_winrate_per_decade_cleaned$teams, 
                         breaks=df_teams_winrate_per_decade_cleaned$ord) +
      labs(x="", y="Win rate (%)", title="Top 6 best soccer teams per decade") +
      guides(fill=FALSE) + theme(axis.text.x=element_text(size=6))
    
  })
  
  output$plot3 <- renderPlot({
    
    ggplot(tmp, aes(x=year, y=nb_games, group=1)) +
      geom_line() +
      geom_point(data = tmp %>% filter(is_wc), aes(colour=is_wc)) +
      labs(x="Year", title="Number of international soccer games", y="", colour="World cup year") +
      geom_vline(xintercept=c(1914,1918,1939,1945), lwd=0.3, colour="green") +
      scale_x_continuous(breaks=seq(1870, 2020, 10))
    
    
  })
  
  # PARTE FER:
  
  # REACTIVE -> Deal with the tournament and date filters
  dataInput <- reactive({
    sub_df <- subset(df_p3, is.element(df_p3$tournament, input$tournament))
    sub_df <- subset(sub_df, sub_df$date >= as.Date(input$date_range[1]) & sub_df$date <= as.Date(input$date_range[2]))
  })
  
  # REACTIVE -> Recalculate the correlation matrix for the corrplot
  dataInputCorr <- reactive({
    corr <- round(cor(select(dataInput(),home_score,away_score, home_wins, draw, neutral)),3)
  })
  # Plot corrplot
  output$corrPlot <- renderPlot({
    corr <- dataInputCorr()
    ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)
    
  })
  
  # Plot heatmap/scatterplot dealing with the pair of variables filter
  output$scattPlot <- renderPlot({
    # Get the filtered data
    sub_df <- dataInput()
    # Split the selected filter
    ins <- strsplit(input$plot_select, "-")
    
    # Plot heatmap/scatterplot depending of having a logical column type (for color) or both numerical
    if (typeof(get(ins[[1]][2],sub_df)) == "logical"){
      ggplot(sub_df, aes_string(x="date", y=ins[[1]][1], color=ins[[1]][2])) + scale_x_date(labels = date_format("%Y-%m"))  + 
        geom_bin2d() + scale_fill_gradientn(limits=c(0,50), breaks=seq(0, 40, by=10),colours = brewer.pal(3,"Blues"))
    }else {
      ggplot(sub_df, aes_string(x=ins[[1]][2], y=ins[[1]][1], color="neutral")) + geom_bin2d()
    }
  })
  
  # Plot selected datapoints on heatmap/scatterplot
  output$info_point <- renderTable({
    sub_df <- dataInput()
    tab = nearPoints(sub_df, input$scatt_click)
    tab$date <- format(tab$date, "%Y-%m-%d")
    tab
  })
 
  # Parte Jagoba
  
  output$distPlotLactual <- renderPlot({
    
    #Get data in the range that is provided
    data_range <- subset(data1,date>as.Date(input$range[1],format='%Y-%m-%d') & date<as.Date(input$range[2],format='%Y-%m-%d'))
    #Get a table which frequencies
    fre <- as.data.frame(table(data_range$match))
    #Sort the number of matches
    sorted <- sort(fre$Freq,decreasing = TRUE)
    sorted_index <- order(fre$Freq,decreasing = TRUE)
    fre2 <-fre[sorted_index,]
    
    #Coustimize the batplot and get the 10 more frequent matches
    op <- par(mar = c(10,4,4,2) + 0.1)
    b <- barplot(fre2[1:10,]$Freq,names.arg = fre2[1:10,]$Var1,las=3,cex.axis=0.9,cex.names=0.8,ylim=c(0,max(fre2[1:10,]$Freq*1.3)),
                 col=rgb(8/255,7/255,160/255))
    text(x=b, y= fre2[1:10,]$Freq*1.1, labels=as.character(fre2[1:10,]$Freq))
    
    
  }, height = 500, width =500)
  
  
  
}

shinyApp(ui = ui, server = server)

