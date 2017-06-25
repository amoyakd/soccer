library(shiny)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(qtlcharts)
library(shinythemes)
library(DT)
source('main.R')

ui <- fluidPage(
  navbarPage("European Soccer",
             theme = shinytheme("flatly"),
             
    tabPanel("League Table",
          fluidRow(
              column(3,uiOutput("choose_league")),
              column(3,uiOutput("choose_season"))
          ),
          dataTableOutput("tbl") 
    ),
    tabPanel("Team Points Comparison",
      fluidRow(
        column(3, uiOutput("choose_league2")),
        column(3, uiOutput("choose_team1"))
        #column(6)
      ),#End Row 1
      fluidRow(
        column(12, plotOutput("plot1"))
        #column(12, dataTableOutput("table2"))
      )#End Row 2
    ),#End Tab Panel
    tabPanel("Attribute Analysis",
      fluidRow(
        column(3, uiOutput("choose_league3")),
        column(3, uiOutput("choose_team2")),
        column(3, uiOutput("choose_attr1"))
      ),#End Row 1
      fluidRow(
        column(12, plotOutput("plot2"))
      ), #End of ROw 3
      fluidRow(
        column(10, offset = 2, iplotCorr_output("corrPlot"))
      )
    ),#End Tab Panel 3
    tabPanel("Team Performance",
      fluidRow(
        column(3, uiOutput("choose_TP.League"))
      ),# End of ROw 1
      fluidRow(
        column(12, dataTableOutput("TP.dataTable"))
      )# End of Row 2
    )#End Tab Panel 4
  )
)

server <- function(input, output) {
  
  #Create a name-value list
  leagueNames <- setNames(league$id, league$name)
  seasons <- match %>% select(season) %>% distinct(season)
  
  # <----------- Component 'League' START ---------- >
  #Dropdown for league
  output$choose_league <- renderUI({
    selectInput("league", "Select League", choices = leagueNames)
  })

  #Dropdown for season
  output$choose_season <- renderUI({
    selectInput("season", "Select Season", choices = as.list(seasons))
  })

  output$tbl <- renderDataTable({
    tbl <- getleagueSeasonTable(team_season_details, input$league, input$season) %>% 
                                  select(-c(country_id:team_api_id, team_short_name))
    tbl<- tbl %>% rename('Total Points' = total_season_points,
                         'Team' = team_long_name,
                         'Home Matches' = home_matches,
                         'Home Wins' = total_home_wins,
                         'Home Losses' = total_home_losses,
                         'Home Draws' = total_home_draws,
                         'Home Points' = total_home_points,
                         'Away Matches' = away_matches,
                         'Away Wins' = total_away_wins,
                         'Away Losses' = total_away_losses,
                         'Away Draws' = total_away_draws,
                         'Away Points' = total_away_points,
                         'Total Matches' = total_matches,
                         'Total Wins' = total_wins,
                         'Total Losses' = total_loss,
                         'Total Draws' = total_draws
                          ) %>%
          arrange(desc(`Total Points`))
    tbl <- tbl[, c(3:18)]
    #print(tbl)
  })
  # <-----------  Component 'League' END ---------->
  
  
  
  
  # <-----------  Component 'Team Comparison' START ------>
  # Dropdown for League
  output$choose_league2 <- renderUI({
    selectInput("league2", "Select League", choices = leagueNames)
  })
  
  output$choose_team1 <- renderUI({
    teams <- getLeagueTeams(team_season_details, input$league2)[,3:4]
    teamNames <- setNames(teams$team_api_id,teams$team_long_name)
    selectInput("team1", "Select Team", choices = teamNames, multiple = TRUE)
  })
  
  #Render Plot for Team Performance
  output$plot1 <- renderPlot({
    teamAllSeasons <- getNTeamAllSeasons(team_season_details, input$team1)
    p <-  ggplot(teamAllSeasons, aes(season, total_season_points, group = team_long_name)) +
          geom_line(aes(color = team_long_name) , size=1.0) +
          geom_point(aes(color = team_long_name), size=4, shape=21, fill="white") + 
          expand_limits(y=0) +
          xlab("Season") + ylab("Points") + 
          ggtitle("Total Points by Season")
    print(p)
  })
  # output$table2 <- renderDataTable({
  #   print(input$team1)
  #   teamAllSeasons <- getNTeamAllSeasons(team_season_details, input$team1)
  # })
  # <-----------  Component 'Team Comparison' END   ------>
  
  
  
  # <-----------  Component 'Attribute Analysis' START ------>
  # Dropdown for League
  output$choose_league3 <- renderUI({
    selectInput("league3", "Select League", choices = leagueNames)
  })
  
  #Dropdown for Team
  output$choose_team2 <- renderUI({
    teams <- getLeagueTeams(team_season_details, input$league3)[,3:4]
    teamNames <- setNames(teams$team_api_id,teams$team_long_name)
    selectInput("team2", "Select Team", choices = teamNames)
  })
  
  #Dropdown for Attriutes
  output$choose_attr1 <- renderUI({
    attributes <- names(team_merged)[-c(1:7)]
    selectInput("attr1", "Select Attributes", choices = attributes, multiple = TRUE)
  })
  
  #Render Plot for each Attribute
  output$plot2 <- renderPlot({
    df <-  team_merged %>% filter(team_api_id == input$team2)
    df <- df[,c(input$attr1, "season")]
    
    require(reshape2)
    mdf <- melt(df, id = "season")
    
    p <-  ggplot(mdf, aes(x = season, y = value, colour = variable, group = variable)) +
      geom_line(size=1.0) +
      geom_point(size=4, shape=21, fill="white") +
      expand_limits(y=0) +
      xlab("Season") + ylab("Value") +
      ggtitle("Team Attribute values Season-wise")
    print(p)
  })
  
  #Render Correlation Matrix
  output$corrPlot <- iplotCorr_render({
    corr.df <- team_merged[,c(8:17)]
    p <- iplotCorr(corr.df, reorder=FALSE)
    #print(p)
  })
  # <-----------  Component 'Attribute Analysis' END ------>
  
  
  
  # <-----------  Component 'Team Performance' START ------>
  # Dropdown for League
  output$choose_TP.League <- renderUI({
    selectInput("TP.League", "Select League", choices = leagueNames)
  })
  
  #Render Data Table
  output$TP.dataTable <- renderDataTable({
    tbl <- team_Summary %>% filter(league_id == input$TP.League) 
    tbl <- tbl[,c('Team', 'Wins', 'Losses','Win %','Total Matches')]
    tbl <- datatable(tbl) %>% 
            formatStyle(
                c('Wins',
                'Losses',
                'Total Matches'),
                background = styleColorBar(range(team_Summary$`Total Matches`),
                'skyblue'),
                backgroundSize = '75% 80%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'right'
              )
  })
  # <-----------  Component 'Team Performance' END ------>
}

shinyApp(ui = ui, server = server)