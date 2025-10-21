library(shiny)
library(bslib)
library(bs4Dash)
library(scales)
library(styler)

style_file("ui.R")

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
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Pitching",
          icon = icon("baseball"),
          startExpanded = FALSE,
          menuItem("Standings", tabName = "pitcherstandings", icon = icon("list")),
          menuItem("Dashboard", tabName = "dashboard", icon = icon("list"))
        ),
        menuItem("Hitters", tabName = "hitters", icon = icon("users")),
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
          tabName = "pitcherstandings",
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
          tabName = "dashboard",
          fluidRow(
            column(1, selectInput("season_select", "Select Season:",
              choices = c("FALL", "Spring"), selected = "FALL"
            )),
            column(2, selectInput("pitcher_select", "Select Pitcher:", choices = NULL)),
            column(1, selectInput("date_select", "Select Date:", choices = NULL)),
            column(2, selectizeInput("pitch_type_select", "Select Pitch Type:",
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )),
            column(3, selectizeInput("batter_hand_select", "Select Batter Handedness:",
              choices = c("L", "R", "S"), selected = c("L", "R", "S"), multiple = TRUE
            ))
          ),
          DT::dataTableOutput("PitcherMetrics"),
          tabsetPanel(
            tabPanel("Pitch Location", plotOutput("veloPlot")),
            tabPanel("Pitch Velocity", plotOutput("pitch_velocity_plot")),
            tabPanel("Pitch Movement", plotOutput("pitch_movement_plot"))
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
