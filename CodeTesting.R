library(tidyverse)
library(scales)
library(kableExtra)
library(styler)
library(usethis)
options(knitr.kable.NA = "")
library(DT)
library(plotly)


getAdminLTEColors()


style_file("CodeTesting.R")

data <- read.csv("/Users/garrettkemp/Documents/Python/Data/20241030-MercerUniversity-Private-1_unverified.csv")

data = read.csv("20250308-MercerUniversity-2_unverified.csv")

game <- data %>%
  mutate(
    Date = format(as.Date(data$Date), "%m/%d/%y"),
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = recode(TaggedPitchType,
      Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = "SI",
      Cutter = "CT", Splitter = "SP", ChangeUp = "CH", Slider = "SL",
      Curveball = "CB", KnuckleBall = "KC"
    ),
    PitchCall = recode(PitchCall,
      BallCalled = "Ball", BallinDirt = "Ball",
      FoulBallNotFieldable = "Foul", FoulBallFieldable = "Foul"
    ),
    KorBB = recode(KorBB, Strikeout = "Strikeout", Walk = "Walk", Undefined = ""),
    `Top.Bottom` = recode(`Top.Bottom`, Top = "T", Bottom = "B"),
    Inn = paste(`Top.Bottom`, Inning, sep = " "),
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
  mutate(AB = dense_rank(ABlabel), .after = Time) %>%
  ungroup() %>%
  group_by(Pitcher) %>%
  mutate(FBV = mean(Velo[Pitch %in% c("FB", "2SFB")], na.rm = T)) %>%
  ungroup() %>%
  mutate(VDiff = FBV - Velo)



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

ggplot(player, aes(x = HB, y = IVB, color = Pitch)) +
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

pitcher = filter(game, Pitcher == "Kersey, Braydon")

pitch_order <- c("FB", "2SFB", "SI", "CT", "SP", "CH", "SL", "CB","KC")

pcolors = c('#d22d49','#93afd4', '#1dbe3a', '#c3bd0e', '#00d1ed', '#933f2c', '#de6a04', '#ddb33a', '#854cb5') 
pcolors = setNames(pcolors, c('FB', '2SFB', 'CH', 'SL', 'CB', 'CT', 'SI', 'SP', 'KC'))

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


tableA = 
  pitcher %>% 
  group_by(Pitch) %>% 
  summarise(
    "#" = n(),
    Usage = percent(n()/length(.$Pitch)),#3
    Max = floor(max(Velo, na.rm = TRUE)) %>% as.integer(),
    Avg = floor(mean(Velo, na.rm = TRUE)) %>% as.integer(),
    Spin = mean(Spin, na.rm = T) %>% as.integer(),
    Tilt = Tilt %>% as.POSIXct(format = '%H:%M', tz = 'UTC') %>%
      as.numeric() %>% mean(na.rm = T) %>%
      as.POSIXct(origin = '1970-01-01', tz = 'UTC') %>%
      format(format = "%k:%M", tz = 'UTC'),
    HB = mean(HB, na.rm = T) %>% round(2),
    IVB = mean(IVB, na.rm = T) %>% round(2),
    VAA = mean(VAA, na.rm = T) %>% round(2),
    HAA = mean(HAA, na.rm = T) %>% round(2),
    Ext = mean(Ext, na.rm = T) %>% round(2)
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

plot_ly(pitcher, color = ~Pitch, colors = pcolors, source = 'PMB') %>% 
  add_trace(x = ~HB, y = ~IVB, type = 'scatter', mode = 'markers',
            marker = list(size = 8, line = list(color = 'black',width = 1)), # ADD COMMA BACK HERE
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

