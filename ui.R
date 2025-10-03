library(shiny)
library(bslib)
library(bs4Dash)

library(shiny)
library(bs4Dash)

# make the top bar color orange and the sidebar color light


ui <- bs4DashPage(
  header = bs4DashNavbar(
    skin = "orange", # top bar color
    border = FALSE,
    controlbarIcon = NULL,
    sidebarIcon = NULL,
    fixed = TRUE,
    title = "Mercer Baseball Analytics"
  ),
  
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "warning",
    title = "Navigation",
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Pitchers", tabName = "pitchers", icon = icon("user")),
      menuItem("Hitters", tabName = "hitters", icon = icon("users")),
      menuItem("Performance Metrics", tabName = "metrics", icon = icon("chart-bar"))
    )
  ),
  
  body = bs4DashBody(
    tabItems(
      tabItem(tabName = "home", h2("Home Page")),
      tabItem(tabName = "pitchers", h2("Pitcher Data")),
      tabItem(tabName = "metrics", h2("Performance Metrics"))
    )
  )
)
