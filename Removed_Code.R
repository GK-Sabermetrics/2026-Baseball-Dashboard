# Removed code

page_navbar(
  title = "MU Baseball",
  header = tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
  nav_spacer(),
  # nav_panel("Team"),
  nav_menu(
    "Pitching",
    nav_panel("Pitcher Cards",
              page_fluid(
                card()
              )
    ),
    nav_panel(
      "Pitching Reports",
      page_sidebar(
        sidebar = sidebar(
          width = 250,
          bg = "lightgrey"
        )
      )
    )
  ),
  nav_panel("Team"),
  nav_panel("Team"),
)


.navbar {
  background-color: #f76800;
    padding-top: 0px;
}

.navbar-nav .nav-link {
  color: white ;
  font-size: 18px;
  margin-right: 5px;
  margin-top: -2px;
}

.nav-link:hover {
  background-color: white;
  color: black;
}

.nav-link.active {
  font-weight: 700 !important;
  color: white !important;
  background-color: #f76800;
}

.navbar-brand {
  color: white !important;
  font-weight: 700;
}

.main-header.navbar {
  background-color: #f76800;
}



