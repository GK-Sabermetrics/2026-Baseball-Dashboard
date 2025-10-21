# Server
library(tidyverse)
library(DT)


data = read.csv("20250308-MercerUniversity-2_unverified.csv")

game =
  filter(data, TaggedPitchType != 'Other') %>% 
  mutate(
    Date = Date %>% as.character(),
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = TaggedPitchType,
    Pitch = recode(Pitch, Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = 'SI', 
                   Cutter = 'CT', Splitter = 'SP', ChangeUp = 'CH', Slider = 'SL',
                   Curveball = 'CB', KnuckleBall = 'KC'),
    PitchCall = recode(PitchCall, BallCalled = 'Ball', BallinDirt = 'Ball',
                       FoulBallNotFieldable = 'Foul', FoulBallFieldable = 'Foul'),
    `Top.Bottom` = recode(`Top.Bottom`, Top = "T", Bottom = "B"),
    Inn = paste(`Top.Bottom`, Inning, sep = " "),
    KorBB = recode(KorBB, Strikeout = 'Strikeout', Walk = 'Walk', Undefined = ""),
    ArmRad = atan2(RelHeight, RelSide),
    ArmDeg = ArmRad * (180/pi),
    ArmDeg = ifelse(PitcherThrows == "Right", ArmDeg, 180-ArmDeg),
    ABlabel = paste(Inn, PAofInning, sep = "-")
  ) %>% 
  rename(
    PAOutcome = KorBB,
    PitchType = TaggedPitchType,
    HitType = TaggedHitType,
    Velo = RelSpeed,
    Spin = SpinRate,
    IVB = InducedVertBreak,
    HB = HorzBreak
  )

game = 
  game %>% 
  group_by(Date, Batter) %>% 
  mutate(AB = dense_rank(ABlabel), .after = Time) %>% 
  ungroup()



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {

  # Color Map ----
  color_map = c(
    'FB' = '#d22d49',
    '2SFB' = '#93afd4',
    'SI' = '#de6a04',
    'SP' = '#ddb33a',
    'CT' = '#933f2c',
    'CH' = '#1dbe3a',
    'SL' = '#c3bd0e',
    'CB' = '#00d1ed',
    'KC' = '#854cb5'
  )
  
  # Pitch Order ----
  pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")
  
  # --- updated reactive/observer section: preserves selections both ways ---
  
  # Reactive that filters by season (if your data has Season)
  season_filtered <- reactive({
    req(input$season_select)
    df <- game
    df = filter(df, PitcherTeam == "MER_BEA")
    if ("Season" %in% names(df)) {
      df <- df %>% filter(Season == input$season_select)
    }
    df
  })
  
  # When season changes: populate pitcher choices but leave selection empty by default;
  # preserve existing pitcher/date if still valid for the new season.
  observeEvent(input$season_select, {
    df <- season_filtered()
    pitchers <- sort(unique(df$Pitcher))
    dates <- sort(unique(df$Date))
    
    # Preserve existing pitcher if still present in the new season, otherwise clear
    new_pitcher_selected <- if (!is.null(input$pitcher_select) && input$pitcher_select %in% pitchers) {
      input$pitcher_select
    } else {
      character(0)  # empty selection
    }
    
    # Preserve existing date if still present in the new season, otherwise clear
    new_date_selected <- if (!is.null(input$date_select) && input$date_select %in% dates) {
      input$date_select
    } else {
      character(0)
    }
    
    updateSelectInput(session, "pitcher_select",
                      choices = pitchers,
                      selected = new_pitcher_selected)
    
    # Keep date list empty until a pitcher is selected (UX choice). If you prefer all dates shown here,
    # change choices = dates and selected = new_date_selected
    updateSelectInput(session, "date_select",
                      choices = character(0),
                      selected = new_date_selected)
  }, ignoreNULL = FALSE)
  
  # When a pitcher is selected: update date choices to only that pitcher's dates,
  # and preserve the previously chosen date if it's still valid
  observeEvent(input$pitcher_select, {
    # If no pitcher selected (cleared), clear dates and return
    if (is.null(input$pitcher_select) || input$pitcher_select == "") {
      updateSelectInput(session, "date_select",
                        choices = character(0),
                        selected = character(0))
      return()
    }
    
    df_season <- season_filtered()
    df_pitcher = season_filtered() %>% filter(Pitcher == input$pitcher_select)
    
    # compute dates for this pitcher
    dates <- df_season %>%
      filter(Pitcher == input$pitcher_select) %>%
      pull(Date) %>%
      unique() %>%
      sort()
    
    date_choices <- c("All", dates)
    
    # preserve existing date selection if still valid for this pitcher; otherwise default to "All"
    new_date_selected <- if (!is.null(input$date_select) && input$date_select %in% date_choices) {
      input$date_select
    } else {
      "All"
    }
    
    updateSelectInput(session, "date_select",
                      choices = date_choices,
                      selected = new_date_selected)
    updateSelectizeInput(session, 'pitch_type_select',
                         choices = pitch_order[pitch_order %in% unique(df_pitcher$Pitch)],
                         selected = pitch_order[pitch_order %in% unique(df_pitcher$Pitch)])
    #updateCheckboxGroupInput(session, "pitch_type_select",
    #                  choices = pitch_order[pitch_order %in% unique(df_pitcher$Pitch)], inline = TRUE)
    
  }, ignoreNULL = FALSE)
  
  # When a date is selected: update pitcher choices to only pitchers who pitched that date,
  # and preserve the previously chosen pitcher if still valid (vice versa)
  observeEvent(input$date_select, {
    # If date is empty (cleared), restore full pitcher list for the season but preserve selection if valid
    df_season <- season_filtered()
    
    if (is.null(input$date_select) || input$date_select == "") {
      all_pitchers <- sort(unique(df_season$Pitcher))
      new_pitcher_selected <- if (!is.null(input$pitcher_select) && input$pitcher_select %in% all_pitchers) {
        input$pitcher_select
      } else {
        character(0)
      }
      updateSelectInput(session, "pitcher_select",
                        choices = all_pitchers,
                        selected = new_pitcher_selected)
      return()
    }
    
    # if Date == "All" show all pitchers for the season
    if (input$date_select == "All") {
      pitchers_for_date <- sort(unique(df_season$Pitcher))
    } else {
      pitchers_for_date <- df_season %>%
        filter(Date == input$date_select) %>%
        pull(Pitcher) %>%
        unique() %>%
        sort()
    }
    
    # preserve existing pitcher if still in the filtered list; otherwise clear selection (empty)
    new_pitcher_selected <- if (!is.null(input$pitcher_select) && input$pitcher_select %in% pitchers_for_date) {
      input$pitcher_select
    } else {
      character(0)
    }
    
    updateSelectInput(session, "pitcher_select",
                      choices = pitchers_for_date,
                      selected = new_pitcher_selected)
  }, ignoreNULL = FALSE)
  
  # Filter reactives: require a pitcher be selected before returning data.
  pitcher_filtered <- reactive({
    req(input$pitcher_select)  # ensures downstream outputs wait for pitcher
    df <- season_filtered()
    df <- df %>% filter(Pitcher == input$pitcher_select)
    df
  })
  
  date_and_other_filtered <- reactive({
    df <- pitcher_filtered()  # req in pitcher_filtered forces pitcher selection
    if (!is.null(input$date_select) && input$date_select != "" && input$date_select != "All") {
      df <- df %>% filter(Date == input$date_select)
    }
    df
  })
  
  
  # Top Gun Table ----
  TopGunTable = 
    game %>% 
    filter(PitcherTeam == "MER_BEA") %>% 
    group_by(Pitcher) %>% 
    summarise(
      Velo = max(Velo, na.rm = T),
    ) %>%
    arrange(desc(Velo))
  
  output$TopGunTable <- DT::renderDataTable({
    DT::datatable(
      TopGunTable,
      options = list(dom = 't',paging = F, ordering = F),
      rownames = FALSE
    ) %>% 
      formatRound('Velo', 2)
  })
  
  # Pitcher Metrics Table ----
  output$PitcherMetrics = DT::renderDataTable({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "")
    )
    df <- date_and_other_filtered()
    
    table = df %>%
      group_by(Pitch) %>% 
      summarise(
        "#" = n(),
        Usage = percent(n()/length(.$Pitch)),#3
        Max = floor(max(Velo, na.rm = TRUE)) %>% as.integer(),
        Avg = floor(mean(Velo, na.rm = TRUE)) %>% as.integer(),
        Spin = mean(Spin, na.rm = T) %>% as.integer(),
        Axis = mean(SpinAxis, na.rm = T),
        Sa = Axis / 30,
        HHa = sapply(Sa, function(x) {
          if (Axis > 180 & Axis < 360) {
            floor(x - 6)
          } else if (Axis == 180) {
            12
          } else {
            floor(x + 6)
          }
        }),
        HHa = ifelse(HHa == 0, HHa + 12, HHa),
        MMa = round((Sa %% 1) * 60, digits = 0),
        HH = ifelse(MMa > 52, HHa + 1, HHa),
        MMb = sapply(MMa, function(x) {
          if (x < 8) {
            0
          } else if (x >= 8 & x <= 52) {
            round(x / 15) * 15
          } else {
            0
          }
        }),
        MM = formatC(MMb, width = 2, flag = "0"),
        Tilt = paste(HH, MM, sep = ":"),
        HB = mean(HB, na.rm = T) %>% round(2),
        IVB = mean(IVB, na.rm = T) %>% round(2),
        VAA = mean(VertApprAngle, na.rm = T) %>% round(2),
        HAA = mean(HorzApprAngle, na.rm = T) %>% round(2),
        VRA = mean(VertRelAngle, na.rm = T) %>% round(2),
        HRA = mean(HorzRelAngle, na.rm = T) %>% round(2),
        Ext = mean(Extension, na.rm = T) %>% round(2)
      ) %>% 
      select(-Axis, -Sa, -HHa, -MMa, -HH, -MMb, -MM) %>%
      mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
      arrange(Pitch)
    
    DT::datatable(table, 
              rownames = FALSE,
              options = list(
                dom = 't',
                paging = FALSE,
                ordering = FALSE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all")
                )
              )
    ) %>%
      # color only the Pitch column cells, not the entire row
      formatStyle(
        'Pitch',
        backgroundColor = styleEqual(names(color_map), color_map),
        color = 'white',
        fontWeight = 'bold'
      )
    
  })
  
  
  # Example plot that waits for pitcher selection:
  output$veloPlot <- renderPlot({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    df <- date_and_other_filtered()
    req(nrow(df) > 0)
    ggplot(df, aes(x = Velo)) + geom_density(fill = "steelblue", alpha = 0.6)
  })
  
  
}