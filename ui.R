library(shiny)
library(bslib)
library(bs4Dash)
library(scales)
library(styler)
library(htmltools)
library(plotly)

#style_file("ui.R")

ui <- tagList(
  # Add stylesheet to the page head (served from www/style.css)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css?v=1")
  ),
  bs4DashPage(
    title = "Mercer Baseball Analytics",
    header = bs4DashNavbar(
      border = FALSE,
      controlbarIcon = icon("cog"),
      sidebarIcon = icon("bars"),
      fixed = TRUE,
      title = "Mercer Baseball Analytics"
    ),
    sidebar = bs4DashSidebar(
      # skin = "light",
      sidebarMenu(
        # Uncomment home and leaderboard when done working on singular page
        #menuItem("Home", tabName = "home", icon = icon("home")),
        #menuItem("Pitchers",
        #  icon = icon("baseball"),
        #  startExpanded = FALSE,
        #  #menuItem("Leaderboard", tabName = "pitcherleaderboard", icon = icon("list")),
        #  menuItem("Dashboard", tabName = "pitcher_dashboard", icon = icon("list"))
        #),
        menuItem("Hitters", 
          icon = icon("baseball-bat-ball"),
          startExpanded = FALSE,
          menuItem("Dashboard", tabName = "hitter_dashboard", icon = icon("grip"))
        ),
        menuItem("Performance Metrics",
          icon = icon("chart-bar"),
          startExpanded = FALSE,
          menuItem("Leaderboards", tabName = "leaderboards", icon = icon("list")),
          menuItem("Quality AB Leaderboard", tabName = "qab", icon = icon("check")),
          menuItem("At Bat Breakdowns", tabName = "atbats", icon = icon("clipboard")),
          menuItem("Heat Maps", tabName = "heatmaps", icon = icon("fire")),
          menuItem("Hitter Reports", tabName = "hitter_reports", icon = icon("user")),
          menuItem("Steal Breaks", tabName = "steal_breaks", icon = icon("running"))
        )
      )
    ),
    body = bs4DashBody(
      tabItems(
        tabItem(tabName = "home", h2("Home Page")),
        tabItem(
          tabName = "pitcherleaderboard",
          h2("Pitcher Standings"),
          bs4Card(
            title = "TOP GUN",
            width = 3,
            status = "orange",
            solidHeader = TRUE,
            collapsible = TRUE,
            DT::dataTableOutput("TopGunTable")
          )
        ),
        tabItem(
          tabName = "pitcher_dashboard",
          fluidRow(
            column(1, selectInput("pitcher_season_select", "Select Season:",
              choices = c("FALL", "SPRING"), selected = "FALL"
            )),
            column(2, selectInput("pitcher_select", "Select Pitcher:", choices = NULL)),
            column(1, selectInput("pitcher_date_select", "Select Date:", choices = NULL)),
            column(1, selectizeInput("batter_hand_select", "Batter Hand:",
                                     choices = c("L"="Left","R"='Right'), selected = c("L"="Left","R"='Right'), multiple = TRUE,
                                     options = list(plugins = list("remove_button"))
            )),
            column(2, selectizeInput("pitcher_pitch_type_select", "Pitch Type:",
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )),
            column(2, selectizeInput("pitcher_pitch_call_select", "Pitch Call:",
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )),
            column(3, selectizeInput("pitcher_hit_type_select", "Hit Type:",
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
            ))
          ),
          fluidRow(
            column(2, selectizeInput("pitcher_outs_select", "Outs:",
              choices = c("0", "1", "2"), selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )),
            conditionalPanel(
              condition = "input.pitcher_balls_select == '' && input.pitcher_strikes_select == ''",
            selectizeInput("pitcher_count_select", "Count:",
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
              ),
            ),
            conditionalPanel(
              condition = "input.pitcher_count_select == ''",
              selectizeInput("pitcher_balls_select", "Balls:",
                                       choices = NULL, selected = NULL, multiple = TRUE,
                                       options = list(plugins = list("remove_button"))
              ),
            ),
            conditionalPanel(
              condition = "input.pitcher_count_select == ''",
              selectizeInput("pitcher_strikes_select", "Strikes:",
                                       choices = NULL, selected = NULL, multiple = TRUE,
                                       options = list(plugins = list("remove_button"))
              ),
            ),
            selectizeInput('pitcher_PA_result_select', 'PA Result:',
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
            ),
          ),
          DT::dataTableOutput("PitcherMetrics"),
          tabsetPanel(
            tabPanel(
              "Pitcher Report",
              fluidRow(
                column(4,div(style = "margin-top: 24px;", plotlyOutput("PitchMovementPlot1"))),
                column(4,div(style = "margin-top: 24px;", plotlyOutput("PitcherStrikeZone1"))),
                column(4,div(style = "margin-top: 24px;", plotlyOutput("ReleasePoint1")))
              )
            ),
            tabPanel("Pitch Velocity"),
            tabPanel("Pitch Movement", plotOutput("PitchMovementPlot"))
          )
        ),
        tabItem(tabName = "hitter_dashboard",
          fluidRow(
            column(1, selectInput("hitter_season_select", "Select Season:",
                                  choices = c("FALL", "SPRING"), selected = "FALL"
            )),
            column(2, selectInput("hitter_select", "Select Hitter:", choices = NULL)),
            column(2, selectInput("hitter_date_select", "Select Date:", choices = NULL)),
            column(1, selectizeInput("pitcher_hand_select", "Pitcher Hand:",
                                     choices = c("Left", "Right"), selected = c("Left", "Right"), multiple = TRUE,
                                     options = list(plugins = list("remove_button"))
            )),
            column(2, selectizeInput("hitter_pitch_type_select", "Pitch Type:",
                                     choices = NULL, selected = NULL, multiple = TRUE,
                                     options = list(plugins = list("remove_button"))
            )),
            column(2, selectizeInput("hitter_pitch_call_select", "Pitch Call:",
                                     choices = NULL, selected = NULL, multiple = TRUE,
                                     options = list(plugins = list("remove_button"))
            )),
            column(2, selectizeInput("hitter_hit_type_select", "Hit Type:",
                                     choices = NULL, selected = NULL, multiple = TRUE,
                                     options = list(plugins = list("remove_button"))
            ))
          ),
          fluidRow(
            column(2, selectizeInput("hitter_outs_select", "Outs:",
                                     choices = c("0", "1", "2"), selected = NULL, multiple = TRUE,
                                     options = list(plugins = list("remove_button"))
            )),
            conditionalPanel(
              condition = "input.hitter_balls_select == '' && input.hitter_strikes_select == ''",
              selectizeInput("hitter_count_select", "Count:",
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(plugins = list("remove_button"))
              ),
            ),
            conditionalPanel(
              condition = "input.hitter_count_select == ''",
              selectizeInput("hitter_balls_select", "Balls:",
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(plugins = list("remove_button"))
              ),
            ),
            conditionalPanel(
              condition = "input.hitter_count_select == ''",
              selectizeInput("hitter_strikes_select", "Strikes:",
                             choices = NULL, selected = NULL, multiple = TRUE,
                             options = list(plugins = list("remove_button"))
              ),
            ),
            selectizeInput('hitter_PA_result_select', 'PA Result:',
                           choices = NULL, selected = NULL, multiple = TRUE,
                           options = list(plugins = list("remove_button"))
            ),
          ),
          DT::dataTableOutput("HitterMetricsTable"),
          tabsetPanel(
            tabPanel("Hitter Report",
              fluidRow(
                column(4,div(style = "margin-top: 24px;", plotlyOutput("HitterContactChart1"))),
                column(4,div(style = "margin-top: 24px;", plotlyOutput("HitterStrikeZone1"))),
                column(4,div(style = "margin-top: 24px;", plotlyOutput("HitterLaunchExit1")))
              )
            )
          )
        ),
        tabItem(tabName = "leaderboards", h2("Leaderboards Page")),
        tabItem(tabName = "qab", h2("Quality AB Leaderboard")),
        tabItem(tabName = "atbats", h2("At Bat Breakdowns")),
        tabItem(tabName = "heatmaps", h2("Heat Maps")),
        tabItem(tabName = "hitter_reports", h2("Hitter Reports")),
        tabItem(tabName = "steal_breaks", h2("Steal Breaks"))
      )
    ),
    footer = bs4DashFooter()
  )
)
