# Server
library(tidyverse)
library(data.table)

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

  
}