# Server
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(readxl)


#data = read.csv('/Users/garrettkemp/Documents/Mercer Baseball/Data/2025-2026_Data.xlsx')

data = read_xlsx('2025-2026_Data copy.xlsx', sheet = 1)

data = data %>% select(-4,-8,-12,-15,-29, -52:-54,-56, -61:-69,-72:-76,-81:-83,
                       -87:-96, -98, -101:-110, -112:-138, -146:-168)

game =
  filter(data, TaggedPitchType != 'Other') %>% 
  mutate(
    Date = Date %>% as.character(),
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = TaggedPitchType,
    Pitch = recode(Pitch, Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = 'SI', 
                   Cutter = 'CT', Splitter = 'SP', ChangeUp = 'CH', Slider = 'SL',
                   Curveball = 'CB', KnuckleBall = 'KC'),
    PlayResult = ifelse(KorBB != "Undefined", KorBB, PlayResult),
    PitchCall = recode(PitchCall, BallCalled = 'Ball', BallinDirt = 'Ball',
                       FoulBallNotFieldable = 'Foul', FoulBallFieldable = 'Foul'),
    `Top/Bottom` = recode(`Top/Bottom`, Top = "T", Bottom = "B"),
    Inn = paste(`Top/Bottom`, Inning, sep = " "),
    KorBB = recode(KorBB, Strikeout = 'Strikeout', Walk = 'Walk', Undefined = ""),
    ArmRad = atan2(RelHeight, RelSide),
    ArmDeg = ArmRad * (180/pi),
    ArmDeg = ifelse(PitcherThrows == "Right", ArmDeg, 180-ArmDeg),
    ABlabel = paste(Inn, PAofInning, sep = "-"),
    theta = 90 - Angle
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
  mutate(AB = dense_rank(ABlabel), .after = Date) %>% 
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
  
  # Hit Types ----
  hit_types <- c(
    'Bunt' = "Bunt",
    'GB' = "GroundBall",
    'LD' = "LineDrive",
    'FB' = "FlyBall",
    'PU' = "Popup")
    
  count_types = c(
    "0-0", "0-1", "0-2", "1-0", "1-1", "1-2", "2-0", "2-1", "2-2", "3-0", "3-1", "3-2"
  )
  
  play_results = c(
    'Out' = 'Out', '1B' = 'Single', '2B' = 'Double', '3B' = 'Triple',
    'HR' = 'HomeRun', 'K' = 'Strikeout', 'BB' = 'Walk',
    'FC' = 'FielderChoice', 'E' = 'Error'
  )
  
  pitch_calls = c(
    'Ball' = 'Ball', 'KC' = 'StrikeCalled', 'KS' = 'StrikeSwinging', 'Foul' = 'Foul',
    'In Play' = 'InPlay', 'HBP' = 'HitByPitch' 
  )
  
  # Hitter Shape lists ----
  
  hitterPlate <- data.frame(
    x0 = c(-0.708, -0.708, 0.708, -0.708, 0),
    y0 = c(0.25, 0.25, 0.25, 0.15, 0),
    x1 = c(0.708, -0.708, 0.708, 0, 0.708),
    y1 = c(0.25, 0.15, 0.15, 0, 0.15)
  )
  
  ZoneLines = data.frame(
    x0 = c(-0.2763888, 0.2763888, -0.8291667, -0.8291667),
    y0 = c(1.5, 1.5, 2.166667, 2.833333),
    x1 = c(-0.2763888, 0.2763888, 0.8291667, 0.8291667),
    y1 = c(3.5, 3.5, 2.166667, 2.833333)
  )
  
  # Add a rectangle from (2, 2) to (3, 3)
  Zone_list <- list(
    type = "rect",
    x0 = -0.8291667, y0 = 1.5,
    x1 = 0.8291667, y1 = 3.5,
    line = list(color = "black", width = 3),
    layer = 'below'
  )
  
  # Convert each row to a shape list
  ZoneMark_list <- pmap(ZoneLines, function(x0, y0, x1, y1) {
    list(type = "line",
         x0 = x0, y0 = y0, x1 = x1, y1 = y1,
         line = list(color = "black", width = 3, dash = 'dash'),
         layer = 'below'
    )
  })
  
  # Convert each row to a shape list
  HitterPlate_list <- pmap(hitterPlate, function(x0, y0, x1, y1) {
    list(type = "line",
         x0 = x0, y0 = y0, x1 = x1, y1 = y1,
         line = list(color = "black", width = 3),
         layer = 'below'
    )
  })
  
  # Combine the shapes into a single list
  hitter_all_shapes <- append(HitterPlate_list, c(list(Zone_list), ZoneMark_list))

  contact_shapes <- data.frame(
    x0 = c(-0.708, -0.708, 0.708, 0.708, -0.708, -1.191, -1.191, 1.191, 1.191),
    y0 = c(1.416, 1.416, 1.416, 0.7076667, 0.7076667, -1, 3.6, -1, 3.6),
    x1 = c(0.708, -0.708, 0.708, 0, 0, -1.191, -2, 1.191, 2),
    y1 = c(1.416, 0.7076667, 0.7076667, 0, 0, 3.6, 3.6, 3.6, 3.6)
  )
  
  # Convert each row to a shape list
  contact_shapes_list <- pmap(contact_shapes, function(x0, y0, x1, y1) {
    list(type = "line",
         x0 = x0, y0 = y0, x1 = x1, y1 = y1,
         line = list(color = "black", width = 3),
         layer = 'below'
    )
  })
  
  
  
# Pitching -----

#> Filtering and Display Logic ----
  # Reactive that filters by season (if your data has Season)
  pitcher_season_filtered <- reactive({
    req(input$pitcher_season_select)
    df <- game
    df = filter(df, PitcherTeam == "MER_BEA")
    if ("Season" %in% names(df)) {
      df <- df %>% filter(Season == input$pitcher_season_select)
    }
    df
  })
  
  # When season changes: populate pitcher choices but leave selection empty by default;
  # preserve existing pitcher/date if still valid for the new season.
  observeEvent(input$pitcher_season_select, {
    df <- pitcher_season_filtered()
    pitchers <- sort(unique(df$Pitcher))
    dates <- sort(unique(df$Date))
    
    # Preserve existing pitcher if still present in the new season, otherwise clear
    new_pitcher_selected <- if (!is.null(input$pitcher_select) && input$pitcher_select %in% pitchers) {
      input$pitcher_select
    } else {
      character(0)  # empty selection
    }
    
    # Preserve existing date if still present in the new season, otherwise clear
    new_date_selected <- if (!is.null(input$pitcher_date_select) && input$pitcher_date_select %in% dates) {
      input$date_select
    } else {
      character(0)
    }
    
    updateSelectInput(session, "pitcher_select",
                      choices = pitchers,
                      selected = new_pitcher_selected)
    
    # Keep date list empty until a pitcher is selected (UX choice). If you prefer all dates shown here,
    # change choices = dates and selected = new_date_selected
    updateSelectInput(session, "pitcher_date_select",
                      choices = character(0),
                      selected = new_date_selected)
  }, ignoreNULL = FALSE)
  
  # When a pitcher is selected: update date choices to only that pitcher's dates,
  # and preserve the previously chosen date if it's still valid
  observeEvent(input$pitcher_select, {
    # If no pitcher selected (cleared), clear dates and return
    if (is.null(input$pitcher_select) || input$pitcher_select == "") {
      updateSelectInput(session, "pitcher_date_select",
                        choices = character(0),
                        selected = character(0))
      return()
    }
    
    df_season <- pitcher_season_filtered()
    df_pitcher = pitcher_season_filtered() %>% filter(Pitcher == input$pitcher_select)
    
    # compute dates for this pitcher
    dates <- df_season %>%
      filter(Pitcher == input$pitcher_select) %>%
      pull(Date) %>%
      unique() %>%
      sort()
    
    date_choices <- c("All", dates)
    
    # preserve existing date selection if still valid for this pitcher; otherwise default to "All"
    new_date_selected <- if (!is.null(input$pitcher_date_select) && input$pitcher_date_select %in% date_choices) {
      input$pitcher_date_select
    } else {
      "All"
    }
    
    updateSelectInput(session, "pitcher_date_select",
                      choices = date_choices,
                      selected = new_date_selected)
    updateSelectizeInput(session, 'pitcher_pitch_type_select',
                         choices = pitch_order[pitch_order %in% unique(df_pitcher$Pitch)],
                         selected = pitch_order[pitch_order %in% unique(df_pitcher$Pitch)])
    updateSelectizeInput(session, 'pitcher_pitch_call_select',
                         choices = pitch_calls[pitch_calls %in% unique(df_pitcher$PitchCall)],
                         selected = NULL)
    updateSelectizeInput(session, 'pitcher_outs_select',
                         choices = sort(unique(as.character(df_pitcher$Outs))),
                         selected = NULL)
    updateSelectizeInput(session, 'pitcher_hit_type_select',
                         choices = hit_types[hit_types %in% unique(df_pitcher$HitType)],
                         selected = NULL)
    updateSelectizeInput(session, 'pitcher_count_select',
                         choices = count_types[count_types %in% unique(df_pitcher$Count)],
                         selected = NULL)
    updateSelectizeInput(session, 'pitcher_balls_select',
                         choices = sort(unique(as.character(df_pitcher$Balls))),
                         selected = NULL)
    updateSelectizeInput(session, 'pitcher_strikes_select',
                         choices = sort(unique(as.character(df_pitcher$Strikes))),
                         selected = NULL)
    updateSelectizeInput(session, 'pitcher_PA_result_select',
                         choices = play_results[play_results %in% unique(df_pitcher$PlayResult)],
                         selected = NULL)
    
  }, ignoreNULL = FALSE)
  
  # When a date is selected: update pitcher choices to only pitchers who pitched that date,
  # and preserve the previously chosen pitcher if still valid (vice versa)
  observeEvent(input$pitcher_date_select, {
    # If date is empty (cleared), restore full pitcher list for the season but preserve selection if valid
    df_season <- pitcher_season_filtered()
    
    if (is.null(input$pitcher_date_select) || input$pitcher_date_select == "") {
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
    if (input$pitcher_date_select == "All") {
      pitchers_for_date <- sort(unique(df_season$Pitcher))
    } else {
      pitchers_for_date <- df_season %>%
        filter(Date == input$pitcher_date_select) %>%
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
  
  #> Pitcher Filtered ----
  # Filter reactives: require a pitcher be selected before returning data.
  pitcher_filtered <- reactive({
    req(input$pitcher_select)  # ensures downstream outputs wait for pitcher
    df <- pitcher_season_filtered()
    df <- df %>% filter(Pitcher == input$pitcher_select)
    df
  })
  
  #> Date and other filters reactive ----
  pitcher_final_filtered <- reactive({
    df <- pitcher_filtered()  # req in pitcher_filtered forces pitcher selection
    if (!is.null(input$pitcher_date_select) && input$pitcher_date_select != "" && input$pitcher_date_select != "All") {
      df <- df %>% filter(Date == input$pitcher_date_select)
    }
    # Pitch type (multiple selection)
    if (!is.null(input$pitcher_pitch_type_select) && length(input$pitcher_pitch_type_select) > 0) {
      df <- df %>% filter(Pitch %in% input$pitcher_pitch_type_select)
    }
    # Batter Hand
    if (!is.null(input$batter_hand_select) && length(input$batter_hand_select) > 0) {
      df <- df %>% filter(BatterSide %in% input$batter_hand_select)
    }
    if (!is.null(input$pitcher_pitch_call_select) && length(input$pitcher_pitch_call_select) > 0) {
      df <- df %>% filter(PitchCall %in% input$pitcher_pitch_call_select)
    }
    if (!is.null(input$pitcher_outs_select) && length(input$pitcher_outs_select) > 0) {
      df <- df %>% filter(as.character(Outs) %in% input$pitcher_outs_select)
    }
    if (!is.null(input$pitcher_hit_type_select) && length(input$pitcher_hit_type_select) > 0) {
      df <- df %>% filter(HitType %in% input$pitcher_hit_type_select)
    }
    if (!is.null(input$pitcher_count_select) && length(input$pitcher_count_select) > 0) {
      df <- df %>% filter(Count %in% input$pitcher_count_select)
    }
    if (!is.null(input$pitcher_balls_select) && length(input$pitcher_balls_select) > 0) {
      df <- df %>% filter(Balls %in% input$pitcher_balls_select)
    }
    if (!is.null(input$pitcher_strikes_select) && length(input$pitcher_strikes_select) > 0) {
      df <- df %>% filter(Strikes %in% input$pitcher_strikes_select)
    }
    if (!is.null(input$pitcher_PA_result_select) && length(input$pitcher_PA_result_select) > 0) {
      df <- df %>% filter(PlayResult %in% input$pitcher_PA_result_select)
    }
    df
  })
  
# pitcher = reactive({
#   req(input$pitcher_select)
#   df = pitcher_final_filtered()
#   df
# })
  
  # Hitting ----
  # Reactive that filters by season (if your data has Season)
  hitter_season_filtered <- reactive({
    req(input$hitter_season_select)
    df <- game
    df = filter(df, BatterTeam == "MER_BEA")
    if ("Season" %in% names(df)) {
      df <- df %>% filter(Season == input$hitter_season_select)
    }
    df
  })
  
  # When season changes: populate pitcher choices but leave selection empty by default;
  # preserve existing pitcher/date if still valid for the new season.
  observeEvent(input$hitter_season_select, {
    df <- hitter_season_filtered()
    hitters <- sort(unique(df$Batter))
    dates <- sort(unique(df$Date))
    
    # Preserve existing pitcher if still present in the new season, otherwise clear
    new_hitter_selected <- if (!is.null(input$hitter_select) && input$hitter_select %in% hitters) {
      input$hitter_select
    } else {
      character(0)  # empty selection
    }
    
    # Preserve existing date if still present in the new season, otherwise clear
    new_date_selected <- if (!is.null(input$hitter_date_select) && input$hitter_date_select %in% dates) {
      input$hitter_date_select
    } else {
      character(0)
    }
    
    updateSelectInput(session, "hitter_select",
                      choices = hitters,
                      selected = new_hitter_selected)
    
    # Keep date list empty until a pitcher is selected (UX choice). If you prefer all dates shown here,
    # change choices = dates and selected = new_date_selected
    updateSelectInput(session, "hitter_date_select",
                      choices = character(0),
                      selected = new_date_selected)
  }, ignoreNULL = FALSE)
  
  # When a pitcher is selected: update date choices to only that pitcher's dates,
  # and preserve the previously chosen date if it's still valid
  observeEvent(input$hitter_select, {
    # If no pitcher selected (cleared), clear dates and return
    if (is.null(input$hitter_select) || input$hitter_select == "") {
      updateSelectInput(session, "hitter_date_select",
                        choices = character(0),
                        selected = character(0))
      return()
    }
    
    df_season <- hitter_season_filtered()
    df_hitter = hitter_season_filtered() %>% filter(Batter == input$hitter_select)
    
    # compute dates for this pitcher
    dates <- df_season %>%
      filter(Batter == input$hitter_select) %>%
      pull(Date) %>%
      unique() %>%
      sort()
    
    date_choices <- c("All", dates)
    
    # preserve existing date selection if still valid for this pitcher; otherwise default to "All"
    new_date_selected <- if (!is.null(input$hitter_date_select) && input$hitter_date_select %in% date_choices) {
      input$hitter_date_select
    } else {
      "All"
    }
    
    updateSelectInput(session, "hitter_date_select",
                      choices = date_choices,
                      selected = new_date_selected)
    updateSelectizeInput(session, 'hitter_pitch_type_select',
                         choices = pitch_order[pitch_order %in% unique(df_hitter$Pitch)],
                         selected = pitch_order[pitch_order %in% unique(df_hitter$Pitch)])
    updateSelectizeInput(session, 'hitter_pitch_call_select',
                         choices = pitch_calls[pitch_calls %in% unique(df_hitter$PitchCall)],
                         selected = NULL)
    updateSelectizeInput(session, 'hitter_outs_select',
                         choices = sort(unique(as.character(df_hitter$Outs))),
                         selected = NULL)
    updateSelectizeInput(session, 'hitter_hit_type_select',
                         choices = hit_types[hit_types %in% unique(df_hitter$HitType)],
                         selected = NULL)
    updateSelectizeInput(session, 'hitter_count_select',
                         choices = count_types[count_types %in% unique(df_hitter$Count)],
                         selected = NULL)
    updateSelectizeInput(session, 'hitter_balls_select',
                         choices = sort(unique(as.character(df_hitter$Balls))),
                         selected = NULL)
    updateSelectizeInput(session, 'hitter_strikes_select',
                         choices = sort(unique(as.character(df_hitter$Strikes))),
                         selected = NULL)
    updateSelectizeInput(session, 'hitter_PA_result_select',
                         choices = play_results[play_results %in% unique(df_hitter$PlayResult)],
                         selected = NULL)
    
  }, ignoreNULL = FALSE)
  
  # When a date is selected: update pitcher choices to only pitchers who pitched that date,
  # and preserve the previously chosen pitcher if still valid (vice versa)
  observeEvent(input$hitter_date_select, {
    # If date is empty (cleared), restore full pitcher list for the season but preserve selection if valid
    df_season <- hitter_season_filtered()
    
      if (is.null(input$hitter_date_select) || input$hitter_date_select == "") {
      all_hitters <- sort(unique(df_season$Batter))
      new_hitter_selected <- if (!is.null(input$hitter_select) && input$hitter_select %in% all_hitters) {
        input$hitter_select
      } else {
        character(0)
      }
      updateSelectInput(session, "hitter_select",
                        choices = all_hitters,
                        selected = new_hitter_selected)
      return()
      }
    
    # if Date == "All" show all pitchers for the season
    if (input$hitter_date_select == "All") {
      hitters_for_date <- sort(unique(df_season$Batter))
    } else {
      hitters_for_date <- df_season %>%
        filter(Date == input$hitter_date_select) %>%
        pull(Batter) %>%
        unique() %>%
        sort()
    }
    
    # preserve existing pitcher if still in the filtered list; otherwise clear selection (empty)
    new_hitter_selected <- if (!is.null(input$hitter_select) && input$hitter_select %in% hitters_for_date) {
      input$hitter_select
    } else {
      character(0)
    }
    
    updateSelectInput(session, "hitter_select",
                      choices = hitters_for_date,
                      selected = new_hitter_selected)
  }, ignoreNULL = FALSE)
  
  #> Hitter Filtered ----
  # Filter reactives: require a pitcher be selected before returning data.
  hitter_filtered <- reactive({
    req(input$hitter_select)  # ensures downstream outputs wait for pitcher
    df <- hitter_season_filtered()
    df <- df %>% filter(Batter == input$hitter_select)
    df
  })
  
  #> Hitter final filters reactive ----
  hitter_final_filtered <- reactive({
    df <- hitter_filtered()  # req in pitcher_filtered forces pitcher selection
    if (!is.null(input$hitter_date_select) && input$hitter_date_select != "" && input$hitter_date_select != "All") {
      df <- df %>% filter(Date == input$hitter_date_select)
    }
    # Pitch type (multiple selection)
    if (!is.null(input$hitter_pitch_type_select) && length(input$hitter_pitch_type_select) > 0) {
      df <- df %>% filter(Pitch %in% input$hitter_pitch_type_select)
    }
    # Batter Hand
    if (!is.null(input$hitter_batter_hand_select) && length(input$hitter_batter_hand_select) > 0) {
      df <- df %>% filter(BatterSide %in% input$hitter_batter_hand_select)
    }
    if (!is.null(input$hitter_pitch_call_select) && length(input$hitter_pitch_call_select) > 0) {
      df <- df %>% filter(PitchCall %in% input$hitter_pitch_call_select)
    }
    if (!is.null(input$hitter_outs_select) && length(input$hitter_outs_select) > 0) {
      df <- df %>% filter(as.character(Outs) %in% input$hitter_outs_select)
    }
    if (!is.null(input$hitter_hit_type_select) && length(input$hitter_hit_type_select) > 0) {
      df <- df %>% filter(HitType %in% input$hitter_hit_type_select)
    }
    if (!is.null(input$hitter_count_select) && length(input$hitter_count_select) > 0) {
      df <- df %>% filter(Count %in% input$hitter_count_select)
    }
    if (!is.null(input$hitter_balls_select) && length(input$hitter_balls_select) > 0) {
      df <- df %>% filter(Balls %in% input$hitter_balls_select)
    }
    if (!is.null(input$hitter_strikes_select) && length(input$hitter_strikes_select) > 0) {
      df <- df %>% filter(Strikes %in% input$hitter_strikes_select)
    }
    if (!is.null(input$hitter_PA_result_select) && length(input$hitter_PA_result_select) > 0) {
      df <- df %>% filter(PlayResult %in% input$hitter_PA_result_select)
    }
    df
  })
  
  
  
  # All Outputs ----
  
  #> Pitching Outputs ----
  
  #>> Top Gun Table ----
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
  
  #>> Pitcher Metrics Table ----
  output$PitcherMetrics = DT::renderDataTable({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "")
    )
    df <- pitcher_final_filtered()
    
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
        HH = ifelse(HH == 13, 1, HH),
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
  
  #>> Pitch Movement Plot 1 ----
  output$PitchMovementPlot1 = renderPlotly({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    
    df = pitcher_final_filtered()
    
    plot_ly(df, color = ~Pitch, colors = color_map) %>% 
      add_trace(x = ~HB, y = ~IVB, type = 'scatter', mode = 'markers',
                marker = list(size = 8, line = list(color = 'black',width = 1)),
                text = ~paste(
                  'HB:', round(HB, 1),'in',
                  '<br>VB:', round(IVB, 1),'in',
                  '<br>Spin:',round(df$Spin),'RPM',
                  '<br>Ext:', round(df$Extension,2), 'ft'
                ),
                #hoverinfo = 'text'
                hovertemplate = "%{text}"
      ) %>% 
      config(displaylogo = F, displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-25,25)),
        yaxis = list(range = c(-25,25)),
        title = "Pitch Movement",
        showlegend = F,
        legend = list(orientation ='h', 
                      x = 0, 
                      y = -200, 
                      xanchor = 'left',
                      yanchor = 'top',
                      itemwidth = -1,
                      traceorder = 'normal')
      )
    
    
  })
  
#>> Strike Zone Plots ----
  output$PitcherStrikeZone1 = renderPlotly({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    df = pitcher_final_filtered()
    plot_ly(df, color = ~Pitch, colors = color_map) %>% 
      add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
                marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none',
                #text = ~paste(
                #  PitchingDF()$PitchCall,
                #  "<br>",PitchingDF()$HitType,
                #  "<br>",PitchingDF()$PlayResult
                #              ), 
                #hoverinfo = 'text'
                text = ~PitchCall,
                customdata = paste0(df$HitType, "\n", df$PlayResult),
                hovertemplate = "%{text}<extra>%{customdata}</extra>"
      ) %>% 
      config(displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-3,3), showgrid = T, zeroline = F, title = NA),
        yaxis = list(range = c(-0.5,5), showgrid = T, zeroline = F, title = NA),
        title = "Strike Zone",
        showlegend = F,
        shapes = list(
          list(
            type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5, layer = 'below'
          ),
          #Draw Plate
          list(
            type = "line",x0 = -0.708,x1 = 0.708,y0 = 0.15,y1 = 0.15, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = -0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0.708,y0 = 0.15,y1 = 0.3, layer = 'below'
          ),
          list(
            type = "line",x0 = 0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          list(
            type = "line",x0 = -0.708,x1 = 0,y0 = 0.3,y1 = 0.5, layer = 'below'
          ),
          #End Draw Plate
          list(
            type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.167,y1 = 2.167,layer = 'below',
            line = list(dash = 'dash', color = 'grey', width = 3)
          ),
          list(
            type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.833,y1 = 2.833,layer = 'below',
            line = list(dash = 'dash', color = 'grey', width = 3)
          ),
          list(
            type = 'line',x0 = -0.277,x1 = -0.277,y0 = 1.5,y1 = 3.5,layer = 'below',
            line = list(dash = 'dash', color = 'grey', width = 3)
          ),
          list(
            type = 'line',
            x0 = 0.277,x1 = 0.277,y0 = 1.5,y1 = 3.5,layer = 'below',
            line = list(dash = 'dash', color = 'grey', width = 3)
          )
        )
      )
  })
  
#>> Release Point Plots ----
  
  output$ReleasePoint1 = renderPlotly({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    df = pitcher_final_filtered()
    plot_ly(df, color = ~Pitch, colors = color_map) %>% 
      add_trace(x = ~RelSide, y = ~RelHeight, type = 'scatter', mode = 'markers', 
                marker = list(size = 8, line = list(color = 'black', width = 1))) %>% 
      config(displayModeBar = F) %>% 
      layout(
        xaxis = list(range = c(-5,5)),
        yaxis = list(range = c(4, 7)),
        title = "Pitch Release Points",
        showlegend = F,
        shapes = list(
          list(
            type = 'line',x0 = -5,x1 = 5,y0 = 5,y1 = 5,layer = 'below'
          )
        ))
  })
  
#> Hitting Outputs ----

#>> Hitting Metrics -----

output$HitterMetricsTable = DT::renderDataTable({
  validate(
    need(!is.null(input$hitter_select) && input$hitter_select != "", "")
  )
  df = hitter_final_filtered()
  table = df %>% 
    group_by(PitcherThrows) %>%
    summarise(
      "#" = n(),
      "%" = percent(n() / length(.$Pitch)),
      "H" = length(which(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))),
      'FC' = length(which(PlayResult == 'FieldersChoice')), #10
      'K' = length(which(PAOutcome == 'Strikeout')),
      'E' = length(which(PlayResult == 'Error')),
      'O' = length(which(PlayResult == 'Out')),
      'PA' = length(which(Count == '0-0')),
      'AB' = FC + H + E + O + K,
      "Avg EV" = mean(ExitSpeed, na.rm = TRUE),
      "Max EV" = max(ExitSpeed, na.rm = TRUE),
      "LA" = mean(Angle, na.rm = TRUE),
      "Hit Spin" = mean(HitSpinRate, na.rm = TRUE),
      "Hard-Hit %" = (length(which(ExitSpeed>95))/n()) %>% percent(2),
      "Barrel %" = (length(which(ExitSpeed >= 95 & Angle >= 10 & Angle <= 35))/n()) %>% percent(2)
    ) %>% column_to_rownames(var = "PitcherThrows") %>% 
    select(-FC,-K,-E,-O) %>% mutate(across(where(is.numeric), round, 2))
  
  DT::datatable(table, 
                rownames = TRUE,
                options = list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE,
                  columnDefs = list(
                    list(className = 'dt-center', targets = "_all")
                  )
                )
  )
  
})
  

#>> Hitter Strike Zone  ---------------------------------------------------------------
  
output$HitterStrikeZone1 = renderPlotly({
  validate(
    need(!is.null(input$hitter_select) && input$hitter_select != "", "Select a batter to see stats")
  )
  df = hitter_final_filtered()
  plot_ly(df, color = ~Pitch, colors = color_map) %>% 
    add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
              marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1))) %>% 
    config(displayModeBar = F) %>% 
    layout(
      title = list(text = 'Strike Zone', x = .55),
      xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = c(-3, 3)),
      yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = c(-0.5, 4.5)),
      showlegend = F,
      shapes = hitter_all_shapes
    )
  
})

  

#>> Hitter Contact Chart --------------------------------------------------------------

output$HitterContactChart1 = renderPlotly({
  validate(
    need(!is.null(input$hitter_select) && input$hitter_select != "", "Select a batter to see stats")
  )
  df = hitter_final_filtered()
  df = df %>% filter(PitchCall == "InPlay")
  
  plot_ly(
    data = df,
    x = ~ContactPositionZ,
    y = ~ContactPositionX,
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, opacity = 1, color = ~ExitSpeed, colorscale = 'RdBu',
                  line = list(width = 0.5, color = "#222222"), showscale = FALSE),
    text = ~paste("EV:", df$ExitSpeed %>% round(2), "<br>", 
                  df$HitType),
    customdata = ~PitchNo,
    hovertemplate = "%{text}<extra>%{customdata}</extra>"
  ) %>%
    config(displayModeBar = F) %>% 
    layout(
      title = list(text = "Contact Chart", x = .55),
      xaxis = list(title = "Contact Position X (ft)", zeroline = FALSE, 
                   showgrid = FALSE, range = c(-2, 2)),
      yaxis = list(title = "Contact Position Y (ft)", zeroline = FALSE, 
                   showgrid = FALSE, range = c(-1, 5)),
      shapes = contact_shapes_list
    )
})

#>> Launch Exit Scatter --------------------------------------------------------------
  
  output$HitterLaunchExit1 = renderPlotly({
    validate(
      need(!is.null(input$hitter_select) && input$hitter_select != "", "Select a batter to see stats")
    )
    df = hitter_final_filtered()
    
    plot_ly(
      data = df, type = 'scatterpolar', mode = 'markers', r = ~ExitSpeed %>% round(2), theta = ~theta,
      marker = list(size = 8, opacity = 0.8, color = ~ExitSpeed, colorscale = 'RdBu', showscale = TRUE),
      text = ~Angle %>% round(2),
      hovertemplate = paste(
        "<b>Exit Velo:</b> %{r} mph<br>",
        "<b>Launch Angle: </b>%{text}°<br>",
        "<extra></extra>"
      )
    ) %>%
      config(displayModeBar = F) %>% 
      layout(
        title = list(text = "Launch Exit Scatter Chart", x = .6, font = list(size = 15)),
        polar = list(
          sector = c(-90, 90),
          radialaxis = list(
            #title = "Exit Velocity (mph)",
            range = c(20, 120),       # Adjust to your data
            angle = 90,              # Put labels on top
            tickfont = list(size = 12, color = 'black'),
            tickangle = 90
          ),
          angularaxis = list(
            direction = "clockwise",
            rotation = 90,           # 90° = CF
            tickvals = c(0, 30, 60, 90, 120, 150, 180),
            ticktext = c("90", "60", "30", "0", "-30", '-60', '-90'),
            tickfont = list(size = 10)
          )
        ),
        showlegend = F
      )
  })
  
  
}