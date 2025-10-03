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