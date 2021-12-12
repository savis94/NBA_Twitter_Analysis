library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(shinythemes)
library(dplyr)
library(plotly)
library(ggwordcloud)
#setwd("~/Avis_Miller_Project/ShinyAppProject/")
#load data

team <- read.csv('teamsfinal.csv')
position <- read.csv('positiontab.csv')
player <- read.csv('playertab.csv')


#round the value in player dataset
player$twitter_favorite_count <- round(player$twitter_favorite_count, 0)
player$twitter_retweet_count <- round(player$twitter_retweet_count, 0)
#Increasing the salary to make it more even balanced, and changing Variable name
position$value <- ifelse(position$stat == "Average Salary", position$value*10, position$value)
position$stat <- ifelse(position$stat == "Average Salary", "Average Salary (Hundred Thousands)", position$stat)

####################
################

ui <- fluidPage(
  theme = shinytheme('darkly'),
  titlePanel("The NBA. The Players. And Twitter?"), # Application title
  p('Looking at players social media activity (Twitter) along side their salary. Also take a dive into the Twitter activity and salary broken down by Team and Position'),
  tabsetPanel(type = 'tabs',
              tabPanel('Individual',
                       sidebarLayout(
                         sidebarPanel( h6('Create Wordcloud Based On:'),

                                       selectInput('wordcloud', label ='Statistic',
                                                   choices = c('Player Salary', 'Twitter Favorite Count', 'Twitter Retweet Count'),
                                                   multiple = FALSE),

                                       hr(),

                                       h6('Search and Compare by Player:'),

                                       actionButton('playerbutton', label = 'Search'),

                                       selectInput('chooseplayer', label='Player',
                                                   choices=c(player$player),
                                                   multiple=TRUE),


                         ),

                         mainPanel(#output will be player stats vs league (can do a stacked bar chart. Surplus will be green
                           # and below will turn red). Also a chart on twitter usage and
                           plotOutput('wordcloudstats'),
                           plotlyOutput('playerstats'),
                           # plotOutput('playersalary')
                         )
                       )
              ),

              #TEAM TAB
              tabPanel('Team',
                       sidebarLayout(
                         sidebarPanel(h6('Change Size of Points Based On:'),

                                      selectInput('team', label ='Select:',
                                                  choices = c('Total Team Salary', 'Wins'),
                                                  multiple = FALSE)
                         ),

                         mainPanel(
                           h3('Is fan attendance correlated with twitter usage?'),
                           plotlyOutput('teamsalary')

                         )
                       )


              ),
              tabPanel("Position",
                       sidebarLayout(
                         sidebarPanel(h6('Select Which Positions to Compare'),
                                      selectInput('chooseposition', label='Position',
                                                  choices=c('PG', 'SG', 'SF', 'PF', 'C'),
                                                  multiple=TRUE),


                         h6('PG = Point Guard'),
                         h6('SG = Shooting Guard'),
                         h6('SF = Small Forward'),
                         h6('PF = Power Forward'),
                         h6('C = Center')),

                         mainPanel(
                           h3('Differences in Salary and Twitter Activity by Position'),
                           plotlyOutput('positionstats')
                         )
                       )
              )
  )#closes the tabsetPanel
) #closes fluidpage

server <- function(input, output) {
  #position_subset <- eventReactive(input$positionbutton, {position %>% filter(positioncol %in% input$chooseposition)})
  position_subset <- reactive({position %>% filter(positioncol %in% input$chooseposition)})

  #transpose the data
  library(reshape)
  sal_play <- player
  sal_play$salary_millions <- sal_play$salary_millions*100
  melt_player <- melt(sal_play,id = c('player'))
  playerbutton <- eventReactive(input$playerbutton, {melt_player %>% filter(player %in% input$chooseplayer)})

  output$wordcloudstats <- renderPlot({
    a <- ggplot(player, aes(label = player, size=salary_millions, color=player)) +
      geom_text_wordcloud_area(rm_outside=TRUE, eccentricity=0.5, shape="diamond") +
      scale_radius(range = c(0, 15), limits = c(0, NA)) +
      theme_minimal()
    b <- ggplot(player, aes(label = player, size=twitter_retweet_count, color=player)) +
      geom_text_wordcloud_area(rm_outside=TRUE, eccentricity=0.5, shape="diamond") +
      scale_radius(range = c(2, 25), limits = c(0, NA)) +
      theme_minimal()
    c <- ggplot(player, aes(label = player, size=twitter_favorite_count, color=player)) +
      geom_text_wordcloud_area(rm_outside=TRUE, eccentricity=0.5, shape="diamond") +
      scale_radius(range = c(2, 25), limits = c(0, NA)) +
      theme_minimal()
    {if(input$wordcloud=='Player Salary') a
      else if(input$player=='Twitter Retweet Count') b
      else if (input$player=='Twitter Favorite Count') c}
  })


  output$playerstats <-renderPlotly({
    playerstats <- ggplot(playerbutton(), aes(x=player, y=value, fill=variable)) +
      geom_bar(position='dodge', stat='identity') + xlab("Player") + ylab("Value") +
      scale_fill_manual("Legend", values = c("twitter_favorite_count" = 'red', "twitter_retweet_count" = 'blue', "salary_millions" = 'green4'),
                        labels = c("Twitter Favorite Count","Twitter Retweet Count", "Salary (Ten Thousands"))+
      theme_classic()

    ggplotly(playerstats)
  })


  output$teamsalary <- renderPlotly({
    teamsalary <- ggplot(team, aes(Average_Attendance, Total_Twitter_Activity, size=Total_team_salary)) +
      xlab("Average Attendance per Game") +
      ylab("Twitter Retweets + Favorites") +
      theme_classic() +
      {if(input$team=='Total Team Salary') geom_point(aes(size=Total_Team_salary, color=Team))} +
      {if(input$team=='Wins') geom_point(aes(size=Wins, color=Team))} +
      theme(legend.position="none") +
      scale_color_manual(values=c("red", "springgreen4", "black", "turquoise3", "red1", "violetred4", "blue1", "midnightblue", "blue2", "goldenrod2", "firebrick1", "navy", "red2", "darkmagenta", "slateblue4", "darkred", "darkgreen", "steelblue", "navyblue", "orange", "deepskyblue1", "royalblue2", "royalblue1", "purple4", "black", "purple2", "grey81", "firebrick3", "purple", "brown2")) +
      geom_text(size=2, aes(label=Team))

    ggplotly(teamsalary)
  })


  output$positionstats <-renderPlotly({

    positionstats <- ggplot(position_subset(), aes(x=positioncol, y=round(value, 2), fill=stat)) +
      geom_bar(position='dodge', stat='identity') + xlab("Position") + ylab("Value") +
      scale_fill_manual("Legend", values = c("Average Twitter Favorite Count" = 'red',
                                             "Average Twitter Retweet Count" = 'blue',
                                             "Average Salary (Hundred Thousands)" = 'green4')) +
      theme_classic()

    ggplotly(positionstats)
  })
}

shinyApp(ui = ui, server = server)

