# Server
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(readxl)


#data = read.csv('/Users/garrettkemp/Documents/Mercer Baseball/Data/2025-2026_Data.xlsx')

data = read_xlsx('2025-2026_Data copy.xlsx', sheet = 1)

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
  
  # --- updated reactive/observer section: preserves selections both ways ---

  # Pitching -----  

#> Filtering and Display Logic ----
  # Reactive that filters by season (if your data has Season)
  season_filtered <- reactive({
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
    df_season <- season_filtered()
    
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
    df <- season_filtered()
    df <- df %>% filter(Pitcher == input$pitcher_select)
    df
  })
  
  #> Date and other filters reactive ----
  date_and_other_filtered <- reactive({
    df <- pitcher_filtered()  # req in pitcher_filtered forces pitcher selection
    if (!is.null(input$pitcher_date_select) && input$pitcher_date_select != "" && input$pitcher_date_select != "All") {
      df <- df %>% filter(Date == input$pitcher_date_select)
    }
    # Pitch type (multiple selection)
    if (!is.null(input$pitcher_pitch_type_select) && length(input$pitcher_pitch_type_select) > 0) {
      df <- df %>% filter(Pitch %in% input$pitcher_pitch_type_select)
    }
    # Batter Hand
    if (!is.null(input$pitcher_batter_hand_select) && length(input$pitcher_batter_hand_select) > 0) {
      df <- df %>% filter(BatterSide %in% input$pitcher_batter_hand_select)
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
  
  pitcher = reactive({
    req(input$pitcher_select)
    df = date_and_other_filtered()
    df
  })
  
#  vals <- reactiveValues(updating = FALSE)
  
#  observeEvent(list(input$pitch_call_select, input$hit_type_select), {
#    req(!vals$updating)
#    df = pitcher()
#    
#    vals$updating <- TRUE
#    
#    updateSelectizeInput(session, 'hit_type_select',
#                         choices = hit_types[hit_types %in% unique(df$HitType)],
#                         selected = input$hit_type_select)
#    updateSelectizeInput(session, 'pitch_call_select',
#                         choices = pitch_calls[pitch_calls %in% unique(df$PitchCall)],
#                         selected = input$pitch_call_select)
#    updateSelectizeInput(session, 'PA_result_select',
#                         choices = play_results[play_results %in% unique(df$PlayResult)],
#                         selected = NULL)
#    
#    vals$updating <- FALSE
#  })
  
  
  
  
  
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
  
  # Pitch Movement Plot 1 ----
  output$PitchMovementPlot1 = renderPlotly({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    
    df = date_and_other_filtered()
    
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
  
# Strike Zone Plots ----
  output$StrikeZone1 = renderPlotly({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    df = date_and_other_filtered()
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
  
# Release Point Plots ----
  
  output$ReleasePoint1 = renderPlotly({
    validate(
      need(!is.null(input$pitcher_select) && input$pitcher_select != "", "Select a pitcher to see stats")
    )
    df = date_and_other_filtered()
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