library(tidyverse)
library(scales)
library(kableExtra)
library(styler)
library(usethis)
options(knitr.kable.NA = "")
library(DT)
library(plotly)
library(readxl)
library(htmltools)


getAdminLTEColors()

data.frame(colnames(data), 1:ncol(data))

data = read_xlsx('2025-2026_Data copy.xlsx', sheet = 1)
data = data %>% select(-4,-8,-12,-15,-29, -52:-54,-56, -61:-69,-72:-76,-81:-83,
                       -87:-96, -98, -101:-110, -112:-138, -146:-168)

game <- data %>%
  mutate(
    Date = format(as.Date(data$Date), "%m/%d/%y"),
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = recode(TaggedPitchType,
      Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = "SI",
      Cutter = "CT", Splitter = "SP", ChangeUp = "CH", Slider = "SL",
      Curveball = "CB", KnuckleBall = "KC"
    ),
    PlayResult = ifelse(KorBB != "Undefined", KorBB, PlayResult),
    PitchCall = recode(PitchCall,
      BallCalled = "Ball", BallinDirt = "Ball",
      FoulBallNotFieldable = "Foul", FoulBallFieldable = "Foul"
    ),
    KorBB = recode(KorBB, Strikeout = "Strikeout", Walk = "Walk", Undefined = ""),
    `Top/Bottom` = recode(`Top/Bottom`, Top = "T", Bottom = "B"),
    Inn = paste(`Top/Bottom`, Inning, sep = " "),
    ABlabel = paste(Inn, PAofInning, sep = "-"),
    Zone = ifelse(between(PlateLocHeight, 1.379, 3.6208) &
      between(PlateLocSide, -0.8288, 0.8288), 1, 0)
  ) %>%
  rename(
    Velo = RelSpeed,
    Spin = SpinRate,
    IVB = InducedVertBreak,
    HB = HorzBreak,
    VAA = VertApprAngle,
    HAA = HorzApprAngle,
    Ext = Extension,
    PAOutcome = KorBB
  )

game <-
  game %>%
  group_by(Date, Batter) %>%
  mutate(AB = dense_rank(ABlabel), .after = Date) %>%
  ungroup() %>%
  group_by(Pitcher) %>%
  mutate(FBV = mean(Velo[Pitch %in% c("FB", "2SFB")], na.rm = T)) %>%
  ungroup() %>%
  mutate(VDiff = FBV - Velo)


# Pitcher Filter ----
pitcher = filter(game, Pitcher == "Ackerman, Jess")


pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB", "KC")

# Pitcher Metrics Table ----
unique(data$Pitcher)

## > Player filter ----
player <- game %>% filter(Pitcher == "Thomas, Zach")

## > Metrics Table ----
pitch.metrics <-
  player %>%
  group_by(Pitch) %>%
  summarise(
    Usage = n(),
    Pct = percent(n() / length(.$Pitch)),
    Usage = paste(Usage, Pct, sep = " | "),
    Max = floor(max(Velo, na.rm = T)),
    Avg = floor(mean(Velo, na.rm = T)),
    Spin = floor(mean(Spin, na.rm = T)),
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
    VAA = mean(VAA, na.rm = T) %>% round(2),
    HAA = mean(HAA, na.rm = T) %>% round(2),
    Ext = mean(Ext, na.rm = T) %>% round(2)
  ) %>%
  #select(-Pct, -Axis, -Sa, -HHa, -MMa, -HH, -MMb, -MM) %>%
  mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
  arrange(Pitch)
pitch.metrics

## > Pitch Color Function ----
get_pitch_colors <- function(pitch_vector) {
  case_when(
    pitch_vector == "FB" ~ "#d22d49",
    pitch_vector == "2SFB" ~ "#93afd4",
    pitch_vector == "SI" ~ "#de6a04",
    pitch_vector == "SP" ~ "#ddb33a",
    pitch_vector == "CT" ~ "#933f2c",
    pitch_vector == "CH" ~ "#1dbe3a",
    pitch_vector == "SL" ~ "#c3bd0e",
    pitch_vector == "CB" ~ "#00d1ed",
    pitch_vector == "KC" ~ "#854cb5",
    TRUE ~ "black" # fallback color
  )
}

## > Get pitch colors ----
pitch_colors_metrics <- get_pitch_colors(pitch.metrics$Pitch)

## > Styled Metrics Table Output ----
styled.metrics <-
  pitch.metrics %>%
  kable(format = "html", align = "c") %>%
  kable_styling(
    font_size = 15,
    bootstrap_options = "bordered"
  ) %>%
  row_spec(0, color = "white", background = "#f76800") %>%
  column_spec(1, border_left = T, bold = T, color = "white", background = pitch_colors_metrics)
styled.metrics


styled.metrics

# Pitch Stats Table ----
pitch.stats <-
  player %>%
  group_by(Pitch) %>%
  summarise(
    "#" = n(),
    CStrk = length(which(PitchCall == "StrikeCalled")),
    Swing = length(which(!PitchCall %in% c("StrikeSwinging", "HitByPitch", "Ball"))),
    Whiff = length(which(PitchCall == "StrikeSwinging")),
    "Zone%" = percent(length(which(Zone == 1)) / n()),
    "Strk%" = percent(length(which(!PitchCall %in% c("Ball", "HitByPitch"))) / n()),
    "Chase%" = percent(length(which(PitchCall == "StrikeSwinging" & Zone == 0)) / n()),
    "Whiff%" = percent(Whiff / Swing),
    "CSW%" = percent((CStrk + Whiff) / n()),
    AvgEV = mean(ExitSpeed, na.rm = TRUE) %>% round(),
    "Hard%" = percent(length(which(ExitSpeed > 90)) / n()),
    FP = length(which(Count == "0-0")),
    "FPStrk%" = percent(length(which(Count == "0-0" & !PitchCall %in% c("Ball", "HitByPitch", "InPlay"))) / FP)
  ) %>%
  mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
  arrange(Pitch)
pitch.stats

## > Get pitch colors ----
pitch_colors_stats <- get_pitch_colors(pitch.stats$Pitch)

styled.stats <-
  pitch.stats %>%
  kable(format = "html", align = "c") %>%
  kable_styling(font_size = 15) %>%
  kable_styling(bootstrap_options = "bordered") %>%
  column_spec(1, border_left = TRUE, bold = T, color = "white", background = pitch_colors_stats) %>%
  row_spec(row = 0, color = "white", background = "#f76800")
styled.stats


# Plot SZ ----
ggplot(player, aes(x = PlateLocSide, y = PlateLocHeight, color = Pitch)) +
  xlim(-2, 2) +
  ylim(0, 4.5) +
  geom_rect(aes(xmin = -0.708, xmax = 0.708, ymin = 1.5, ymax = 3.5), alpha = 0, size = .75, color = "black") +
  geom_rect(aes(xmin = -0.8288, xmax = 0.8288, ymin = 1.379, ymax = 3.6208), alpha = 0, size = .75, color = "red") +
  # Home Plate Outline Below
  geom_segment(aes(x = -0.708, y = 0.3, xend = 0.708, yend = 0.3), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.3, xend = -0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = -0.708, y = 0.15, xend = 0, yend = 0), size = 1, color = "black") +
  geom_segment(aes(x = 0, y = 0.0, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  geom_segment(aes(x = 0.708, y = 0.3, xend = 0.708, yend = 0.15), size = 1, color = "black") +
  # annotate("segment",x=-1,xend=1,y = 1, yend = 3, color = 'black') +
  geom_point(size = 4) +
  geom_text(aes(PlateLocSide, PlateLocHeight, label = Zone), size = 5, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), axis.title = element_blank()) +
  theme(aspect.ratio = 1)


# Game Line Table ----

player %>%
  # group_by(Date) %>%
  summarise(
    "IPb" = ((sum(OutsOnPlay) + length(which(PAOutcome == "Strikeout"))) / 3),
    "IP" = ifelse(IPb %% 1 == 0, IPb + 0.0,
      ifelse(between(IPb %% 1, .0, .34), IPb - .2333333, IPb - .4666666)
    ) %>%
      formatC(width = 2),
    "P" = n(),
    "BF" = length(which(Count == "0-0")),
    "K" = length(which(PAOutcome == "Strikeout")),
    "BB" = length(which(PAOutcome == "Walk")),
    "HBP" = length(which(PitchCall == "HitByPitch")),
    "BIP" = length(which(PitchCall == "InPlay")),
    "H" = length(which(PlayResult %in% c("Single", "Double", "Triple", "HomeRun"))),
    "XBH" = length(which(PlayResult %in% c("Double", "Triple", "HomeRun"))),
    "R" = sum(RunsScored),
    "BAA" = sprintf((H / (BF - BB - HBP - sum(PlayResult == "Sacrifice"))), fmt = "%#.3f")
  ) %>% select(-IPb)

# Pitch Characteristics Table ----
pitch.char <-
  player %>%
  group_by(Pitch) %>%
  summarise(
    VAvg = floor(mean(Velo, na.rm = T)),
    VMax = floor(max(Velo, na.rm = T)),
    VDiff = mean(VDiff, na.rm = T) %>% round(1),
    SAvg = floor(mean(Spin, na.rm = T)),
    SMax = floor(max(Spin, na.rm = T)),
    SMin = floor(min(Spin, na.rm = T)),
    HB = round(mean(HB, na.rm = T), 1),
    IVB = round(mean(IVB, na.rm = T), 1),
    HAA = round(mean(HAA, na.rm = T), 1),
    VAA = round(mean(VAA, na.rm = T), 1),
    RelH = round(mean(RelHeight, na.rm = T), 1),
    RelS = round(mean(RelSide, na.rm = T), 1),
    VRA = round(mean(VertRelAngle, na.rm = T), 1),
    HRA = round(mean(HorzRelAngle, na.rm = T), 1),
    Ext = round(mean(Ext, na.rm = T), 1),
  ) %>%
  mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
  arrange(Pitch)
pitch.char

pitch.char %>%
  kbl(format = "html", align = "c") %>%
  kable_styling(bootstrap_options = "bordered", font_size = 18) %>%
  add_header_above(c("", "Velocity" = 3, "Spin" = 3, "Movement" = 4, "Release" = 5),
    background = "#f76800", color = "white"
  ) %>%
  column_spec(1, border_left = TRUE, bold = T, color = "black") %>%
  row_spec(row = 0, color = "white", background = "#f76800") %>%
  column_spec(ncol(pitch.char), border_right = TRUE, color = "black")


# Pitch Movement Plot -----

pcolors <- c("#d22d49", "#93afd4", "#1dbe3a", "#c3bd0e", "#00d1ed", "#933f2c", "#de6a04", "#ddb33a", "#854cb5")

pcolors <- setNames(pcolors, c("FB", "2SFB", "CH", "SL", "CB", "CT", "SI", "SP", "KC"))

ggplot(player, aes(x = HB, y = IVB, color = Pitch)) +
  labs(title = "Pitch Movement" ,color = "",x = "HB (in.)", y = "IVB (in.)" )  +
  xlim(-25, 25) +
  ylim(-25, 25) +
  annotate("segment", x = 0, y = -25, xend = 0, yend = 25, size = 1, color = "grey55") +
  annotate("segment", x = -25, y = 0, xend = 25, yend = 0, size = 1, color = "grey55") +
  coord_fixed() +
  geom_point(aes(fill = Pitch), size = 3, alpha = .85, color = "black", pch = 21) +
  scale_fill_manual(values = pcolors) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none", legend.text = element_text(size = 8))


##> Alternative with elipses ----

ggplot(pitcher, aes(x = HB, y = IVB, color = Pitch)) +
  labs(title = "Pitch Movement" ,color = "",x = "HB (in.)", y = "IVB (in.)" )  +
  xlim(-25, 25) +
  ylim(-25, 25) +
  annotate("segment", x = 0, y = -25, xend = 0, yend = 25, size = 1, color = "grey55") +
  annotate("segment", x = -25, y = 0, xend = 25, yend = 0, size = 1, color = "grey55") +
  coord_fixed() +
  geom_point(aes(fill = Pitch), size = 3, alpha = .85, color = "black", pch = 21) +
  stat_ellipse(aes(fill = Pitch), type = "norm", geom = "polygon", alpha = 0.2, color = NA) +
  scale_fill_manual(values = pcolors) +
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
  theme(legend.position = "none", legend.text = element_text(size = 8))


# Pitch Metrics Table Testing -----

pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")

pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))

#!!! Color Map ----
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

pitcher %>% 
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
  ) %>% 
  mutate(Pitch = factor(Pitch, levels = pitch_order)) %>%
  arrange(Pitch)

datatable(tableA, 
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


# Pitcher Standings Table Testing -----

TopGunTable = 
game %>% 
  filter(PitcherTeam == "MER_BEA") %>% 
  group_by(Pitcher) %>% 
  summarise(
    Velo = max(Velo, na.rm = T),
  ) %>%
  arrange(desc(Velo))

datatable(TopGunTable, 
          rownames = FALSE,
          options = list(
            dom = 't',
            paging = FALSE,
            ordering = FALSE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            )
          )
) %>% formatRound('Velo', 2)

# PitchMovement Graph ----

plot_ly(pitcher, color = ~Pitch, colors = color_map) %>% 
  add_trace(x = ~HB, y = ~IVB, type = 'scatter', mode = 'markers',
            marker = list(size = 8, line = list(color = 'black',width = 1)),
            text = ~paste(
                          'HB:', round(HB, 1),'in',
                          '<br>VB:', round(IVB, 1),'in',
                          '<br>Spin:',round(pitcher$Spin),'RPM',
                          '<br>Ext:', round(pitcher$Ext,2), 'ft'
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

# Strike Zone ----

plot_ly(pitcher, color = ~Pitch, colors = color_map) %>% 
  add_trace(x = ~PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none',
            #text = ~paste(
            #  PitchingDF()$PitchCall,
            #  "<br>",PitchingDF()$HitType,
            #  "<br>",PitchingDF()$PlayResult
            #              ), 
            #hoverinfo = 'text'
            text = ~PitchCall,
            customdata = paste0(pitcher$TaggedHitType, "\n", pitcher$PlayResult),
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

# Release Point ----
plot_ly(pitcher, color = ~Pitch, colors = color_map) %>% 
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

unique(game$PitchCall)

# Hitting -----

#__Strike Zone Hitter ----

hitter = filter(game, Batter == "Decker, Kai")

plot_ly(hitter, color = ~Pitch, colors = color_map) %>% 
  add_trace(x = ~-PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none',
            #text = ~paste(
            #  PitchingDF()$PitchCall,
            #  "<br>",PitchingDF()$HitType,
            #  "<br>",PitchingDF()$PlayResult
            #              ), 
            #hoverinfo = 'text'
            text = ~PitchCall,
            customdata = paste0(hitter$TaggedHitType, "\n", hitter$PlayResult),
            hovertemplate = "%{text}<extra>%{customdata}</extra>"
  ) %>% 
  config(displayModeBar = F) %>% 
  layout(
    dragmode = FALSE,
    xaxis = list(range = c(-3,3), showgrid = T, zeroline = F, title = NA),
    yaxis = list(range = c(-0.5,5), showgrid = T, zeroline = F, title = NA),
    title = "Strike Zone",
    showlegend = F,
    shapes = list(
      list(type = "rect",x0 = -0.708,x1 = 0.708,y0 = 1.5,y1 = 3.5, layer = 'above'),
      #Draw Plate
      # Front of Plate
      list(type = "line",x0 = -0.708,x1 = 0.708,y0 = 0.5,y1 = 0.5, layer = 'above'),
      #Left side of plate
      list(type = "line",x0 = -0.708,x1 = -0.708,y0 = 0.3,y1 = 0.5, layer = 'above'),
      #Right side of plate
      list(type = "line",x0 = 0.708,x1 = 0.708,y0 = 0.3,y1 = 0.5, layer = 'above'),
      #Right angle side of plate
      list(type = "line",x0 = 0.708,x1 = 0,y0 = 0.3,y1 = 0.15, layer = 'above'),
      #Left angle side of plate
      list(type = "line",x0 = -0.708,x1 = 0,y0 = 0.3,y1 = 0.15, layer = 'above'),
      #End Draw Plate
      list(
        type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.167,y1 = 2.167,layer = 'above',
        line = list(dash = 'dash', color = 'grey', width = 3)
      ),
      list(
        type = 'line',x0 = -0.708,x1 = 0.708,y0 = 2.833,y1 = 2.833,layer = 'above',
        line = list(dash = 'dash', color = 'grey', width = 3)
      ),
      list(
        type = 'line',x0 = -0.277,x1 = -0.277,y0 = 1.5,y1 = 3.5,layer = 'above',
        line = list(dash = 'dash', color = 'grey', width = 3)
      ),
      list(
        type = 'line',
        x0 = 0.277,x1 = 0.277,y0 = 1.5,y1 = 3.5,layer = 'above',
        line = list(dash = 'dash', color = 'grey', width = 3)
      )
    )
  )

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
Plate_list <- pmap(hitterPlate, function(x0, y0, x1, y1) {
  list(type = "line",
       x0 = x0, y0 = y0, x1 = x1, y1 = y1,
       line = list(color = "black", width = 3),
       layer = 'below'
  )
})
# Combine the shapes into a single list
all_shapes <- append(Plate_list, c(list(Zone_list), ZoneMark_list))

plot_ly(hitter, color = ~Pitch, colors = color_map) %>%
  add_trace(x = ~-PlateLocSide, y = ~PlateLocHeight, type = 'scatter', mode = 'markers', 
            marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none',
            text = ~paste0(PitchCall,ifelse(TaggedHitType == 'Undefined', "", paste0("<br>",TaggedHitType))),
            customdata = ~Pitch,
            hovertemplate = "%{text}<extra>%{customdata}</extra>"
            ) %>% 
  layout(
    xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = c(-3, 3)),
    yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = c(-1, 5)),
    showlegend = F,
    shapes = all_shapes
  )

ifelse(TaggedHitType == 'Undefined', "", paste0("<br>",TaggedHitType))

# Hitter Metrics Table -----

hitter = filter(game, Batter == "Baughcum, Brant")

hitter %>% filter(Pitch == "SL") %>% select(Date, Pitch, PitchCall, TaggedHitType, HitSpinRate)

HitterMetrics <-
  hitter %>%
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
HitterMetrics

datatable(HitterMetrics, 
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


# Spray chart testing ----

game %>% 
  filter(PitchCall == "InPlay") %>% 
  select(Pitch, ExitSpeed, Angle, Direction, Distance, Bearing)

spray <- game %>%
  #filter(Batter == "Thompson, Zack") %>% 
  filter(PitchCall == 'InPlay') %>% 
  filter(!is.na(Distance), !is.na(Bearing)) %>%
  mutate(
    x = Distance * sin(Bearing * pi/180),  # convert degrees → radians
    y = Distance * cos(Bearing * pi/180)
  )


game %>% filter(PitchCall == 'InPlay') %>% 
  filter(!is.na(Distance), !is.na(Bearing)) %>%
  mutate(
    x = Distance * sin(Bearing * pi/180),  # convert degrees → radians
    y = Distance * cos(Bearing * pi/180),
    theta_rad = (Bearing) * pi/180,
    xt = sin(theta_rad) * Distance,
    yt = cos(theta_rad) * Distance
  ) %>% select(x,y,theta_rad,xt, yt)

ggplot(spray, aes(x = x, y = y, color = PlayResult)) +
  coord_fixed() +
  xlim(-270,270) +
  ylim(-50,400) +
  theme_minimal() +
  geom_segment(aes(x=0,y=0,xend=226,yend=226), linewidth = 1, color = 'black') +
  geom_segment(aes(x=0,y=0,xend=-234,yend=234), linewidth = 1, color = 'black') +
  geom_segment(aes(x=60,y=60,xend=0,yend=120), linewidth = 1, color = 'black') +
  geom_segment(aes(x=-60,y=60,xend=0,yend=120), linewidth = 1, color = 'black') +
  geom_curve(aes(x=-234,y=234,xend=226,yend=226), curvature = -.64, ncp = 4, linewidth = 1, color = 'black') +
  geom_curve(aes(x=-90,y=90,xend=90,yend=90), curvature = -.75, linewidth = 1, ncp = 7, color = 'black') +
  theme_void() + 
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Spray Chart",
       x = "Left ←   Field   → Right",
       y = "Distance from Home Plate (ft)")


plot_ly(spray, color = ~PlayResult) %>% 
  add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 1, line = list(color = 'black',width = 1)), fill = 'none',
            #text = ~paste(
            #  PitchingDF()$PitchCall,
            #  "<br>",PitchingDF()$HitType,
            #  "<br>",PitchingDF()$PlayResult
            #              ), 
            #hoverinfo = 'text'
            text = ~TaggedHitType,
            customdata = paste0(spray$TaggedHitType, "\n", spray$PlayResult),
            hovertemplate = "%{text}<extra>%{customdata}</extra>"
  ) %>% 
  config(displayModeBar = F) %>% 
  layout(
    dragmode = FALSE,
    xaxis = list(range = c(-270,270), showgrid = F, zeroline = F, title = NA),
    yaxis = list(range = c(-50,450), showgrid = F, zeroline = F, title = NA),
    #title = "Strike Zone",
    showlegend = F,
    shapes = list(
    )
  )

# Create smooth fence arc (from LF to RF)
angles <- seq(-90, 90, length.out = 200)
theta_rad <- (90 - angles) * pi / 180
arc_x <- cos(theta_rad) * fence_radius
arc_y <- sin(theta_rad) * fence_radius


# Simple curved outfield wall (semi-circle)
fence_radius <- 380   # You can change this
angles <- seq(-90, 90, length.out = 200)  # Left to right field
theta_rad <- (90 - angles) * pi / 180
arc_x <- cos(theta_rad) * fence_radius
arc_y <- sin(theta_rad) * fence_radius




fence_points = data.frame(
  angle = c(-45, -22.5, 0, 22.5, 45),
  dist = c(330, 365, 380, 345, 320)
)
# Create smooth fence arc
all_angles <- seq(-45, 45, length.out = 180)
smooth_dists <- approx(fence_points$angle, fence_points$dist, xout = all_angles)$y
arc_x <- cos((90 - all_angles) * pi / 180) * smooth_dists
arc_y <- sin((90 - all_angles) * pi / 180) * smooth_dists
# Define foul lines (home to LF & RF)
foul_line_LF <- data.frame(x = c(0, arc_x[1]), y = c(0, arc_y[1]))
foul_line_RF <- data.frame(x = c(0, arc_x[length(arc_x)]), y = c(0, arc_y[length(arc_y)]))
# Infield diamond (90 ft bases). We'll draw a rotated square (diamond) centered along home->infield.
base_dist <- 90
# Diamond coordinates relative to home: using classic orientation (home at origin, second base up the y axis)
home <- c(0,0)
first <- c(base_dist, base_dist) * (1/sqrt(2))
second <- c(0, base_dist * sqrt(2))
third <- c(-base_dist, base_dist) * (1/sqrt(2))
diamond_x <- c(home[1], first[1], second[1], third[1], home[1])
diamond_y <- c(home[2], first[2], second[2], third[2], home[2])
p <- plot_ly(
  data = spray,
  x = ~x,
  y = ~y,
  color = ~PlayResult,
  type = "scatter",
  mode = "markers",
  marker = list(size = 8, opacity = 1, line = list(width = 0.5, color = "#222222")),
  text = ~Distance
  #hoverinfo = "text"
)
p %>% 
layout(
  #title = list(text = title, x = 0.02),
  xaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = c(-270, 270)),
  yaxis = list(title = "", zeroline = FALSE, showgrid = FALSE, range = c(-20, 450)),
  showlegend = F,
  shapes = list(
    # Outfield arc (drawn as path)
    list( # Curved Fence
      type = "path",
      path = paste0("M ", paste(sprintf("%.2f %.2f", arc_x, arc_y), collapse = " L ")),
      layer = 'below',
      line = list(color = "black", width = 3)
    ),
    list( # Left Foul Line
      type = "line",
      x0 = 0, y0 = 0,
      x1 = -233, y1 = 233,
      layer = 'below',
      line = list(color = "black", width = 3)
    ),
    list( # Right Foul Line
      type = "line",
      x0 = 0, y0 = 0,
      x1 = 226, y1 = 226,
      layer = 'below',
      line = list(color = "black", width = 3)
    ),
    # Infield diamond
    list(type = "path",
         path = paste0("M ", paste0(sprintf("%.2f %.2f", diamond_x, diamond_y), collapse = " L "), " Z"),
         line = list(color = "rgba(139,69,19,0.9)", width = 1.5),
         layer = 'below',
         fillcolor = "rgba(210,180,140,0.05)"
    )
  )
) %>%
  config(displayModeBar = TRUE)

# Launch Exit Scatter ----
library("RColorBrewer")
brewer.pal.info

df = game %>% filter(Date == "11/01/25")

# Set thresholds & grouping for hit quality zones (common in MLB visuals)
df <- df %>%
  mutate(
    quality = case_when(
      ExitSpeed >= 95 & Angle >= 10 & Angle <= 35 ~ "Barrel",
      ExitSpeed >= 90 & Angle >= 0 & Angle <= 40 ~ "Solid Contact",
      ExitSpeed >= 80 & Angle >= -10 & Angle <= 50 ~ "Flare/Burner",
      TRUE ~ "Poor Contact"
    )
  )

df <- df %>%
  mutate(
    contact_type = case_when(
      Angle > 50 ~ "Pop Up",
      Angle > 30 & Angle <= 50 ~ "Fly Ball",
      Angle > 25 & Angle <= 30 ~ "Gap Split/Wall Bang",
      Angle > 20 & Angle <= 25 ~ "OF Line Drive",
      Angle > 15 & Angle <= 20 ~ "Looping Line",
      Angle > 5 & Angle <= 15 ~ "Line Drive",
      Angle > 0 & Angle <= 5 ~ "Power GB",
      Angle > -5 & Angle <= 0 ~ "Hard GB",
      Angle > -10 & Angle <= -5 ~ "Weak GB",
      TRUE ~ "Weak Contact"
    )
  )

df %>% filter(PitchCall == "InPlay") %>% 
select(Angle, ExitSpeed, TaggedHitType, contact_type, PlayResult)


df = df %>% mutate(
  theta = 90 - Angle   # Savant style: 90° = straight CF
) #%>% filter(!is.na(ExitSpeed))

plot_ly(
  data = df, type = 'scatterpolar', mode = 'markers', r = ~ExitSpeed, theta = ~theta,
  marker = list(size = 8, opacity = 0.8, color = ~ExitSpeed, colorscale = 'RdBu', showscale = TRUE),
  text = ~Angle,
  hovertemplate = paste(
    "<b>Exit Velo:</b> %{r} mph<br>",
    "<b>Launch Angle:</b> %{text}°<br>",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(text = "Exit Launch Scatter Chart", x=.5, font = list(size = 15)),
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

#tickvals = c(0, 45, 90, 135, 180),
#ticktext = c("90°", "45°", "0°", "-45°", "-90°"),
#
#tickvals = c(0, 30, 60, 90, 120, 150, 180),
#ticktext = c("90", "60", "30", "0", "-30", '-60', '-90'),
#
#tickvals = c(0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180),
#ticktext = c("90",'75','60',"45","30",'15',"0","-15",'-30','-45','-60','-75','-90'),

# Contact Chart -------

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

contactdf = data.frame(
  x = c(0.85, 0.20),
  y = c(2.12, 2.46)
)

data = read_xlsx('/Users/garrettkemp/Documents/R-Files/Baseball-Dashboard-New/Data/2025 Spring Data.xlsx', sheet = 'Season')

df = game %>% filter(Batter == "Decker, Kai" & PitchCall == "InPlay")

df = data %>% filter(Date == as.Date('2025-02-26') & BatterTeam == "MER_BEA")

plot_ly(
  data = df,
  x = ~ContactPositionZ,
  y = ~ContactPositionX,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 10,
    opacity = 1,
    color = ~ExitSpeed,
    colorscale = "RdBu",
    line = list(width = 0.5, color = "#222222")
  ),
  text = ~paste("EV:", ExitSpeed, "<br>", TaggedHitType),
  customdata = ~PitchNo,
  hovertemplate = "%{text}<extra>%{customdata}</extra>"
) %>%
  layout(
    title = "Contact Chart",
    xaxis = list(title = "Horizontal Distance from Center of Plate (ft)", zeroline = FALSE, 
                 showgrid = FALSE, range = c(-2, 2)),
    yaxis = list(title = "Vertical Distance from Plate (ft)", zeroline = FALSE, 
                 showgrid = FALSE, range = c(-1, 5)),
    shapes = contact_shapes_list,
    showlegend = FALSE
  )

plot_ly(df, color = ~ExitSpeed) %>%
  add_trace(x = ~ContactPositionZ, y = ~ContactPositionX, type = 'scatter', 
            mode = 'markers', 
            marker = list(size = 10, colorscale = "RdBu")
            ) %>% 
  layout(
    title = "Contact Chart",
    xaxis = list(title = "Horizontal Distance from Center of Plate (ft)", zeroline = FALSE, 
                 showgrid = FALSE, range = c(-2, 2)),
    yaxis = list(title = "Vertical Distance from Plate (ft)", zeroline = FALSE, 
                 showgrid = FALSE, range = c(-1, 5)),
    shapes = contact_shapes_list
  )

fig <- plot_ly(
  type = 'scatter',
  mode='markers',
  data = df,
  x = ~ContactPositionZ,
  y = ~ContactPositionX,
  color=~ExitSpeed,
  marker=list(
    size=8,
    colorbar=list(
      title='Colorbar'
    ),
    colorscale='RdBu',
    reversescale =F,
    showscale = F
  ),
  text = ~ExitSpeed
)
fig <- fig %>% layout(
  xaxis = list(
    showgrid = F,
    zeroline = F
  ),
  yaxis = list(
    showgrid = F,
    zeroline = F
  )
)
fig




