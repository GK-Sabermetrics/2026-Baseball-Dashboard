library(shiny)
library(bslib)
library(bs4Dash)

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
      skin = "light",
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Pitching", icon = icon("baseball"),
                 startExpanded = FALSE,
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("list"))
        ),
        menuItem("Hitters", tabName = "hitters", icon = icon("users")),
        menuItem("Performance Metrics", icon = icon("chart-bar"),
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
        tabItem(tabName = "leaderboards", h2("Leaderboards Page")),
        tabItem(tabName = "qab", h2("Quality AB Leaderboard")),
        tabItem(tabName = "atbats", h2("At Bat Breakdowns")),
        tabItem(tabName = "heatmaps", h2("Heat Maps")),
        tabItem(tabName = "hitter_reports", h2("Hitter Reports")),
        tabItem(tabName = "steal_breaks", h2("Steal Breaks")),
        tabItem(tabName = "dashboard", h2("dashboard"))
      )
    ),
    
    footer = bs4DashFooter()
  )
)