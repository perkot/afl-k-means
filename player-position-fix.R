interchange <- players %>% 
  filter(PositionType == "Interchange")

print(interchange$Player_Name)

interchange$PositionType

players$PositionType[which(players$Player_Name == "Liam Baker")] <- "Defender"
players$PositionType[which(players$Player_Name == "Shai Bolton")] <- "Forward"
players$PositionType[which(players$Player_Name == "Gryan Miers")] <- "Forward"
players$PositionType[which(players$Player_Name == "Sam Menegola")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jack Graham")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Callum Ah Chee")] <- "Forward"
players$PositionType[which(players$Player_Name == "Zac Bailey")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Cameron Ellis-Yolmen")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Sam Powell-Pepper")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Hunter Clark")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Ben Paton")] <- "Defender"
players$PositionType[which(players$Player_Name == "Jake Waterman")] <- "Forward"
players$PositionType[which(players$Player_Name == "Dom Sheed")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jack Petruccelle")] <- "Forward"
players$PositionType[which(players$Player_Name == "Tom Cole")] <- "Defender"
players$PositionType[which(players$Player_Name == "Bailey Smith")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Patrick Lipinski")] <- "Forward"
players$PositionType[which(players$Player_Name == "Adam Cerra")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Reece Conca")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Andrew Brayshaw")] <- "Midfield"
players$PositionType[which(players$Player_Name == "James Aish")] <- "Defender"
players$PositionType[which(players$Player_Name == "Blake Acres")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jordan Dawson")] <- "Defender"
players$PositionType[which(players$Player_Name == "Ryan Clarke")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jackson Thurlow")] <- "Defender"
players$PositionType[which(players$Player_Name == "Robbie Fox")] <- "Defender"
players$PositionType[which(players$Player_Name == "James Rowbottom")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Harry Morrison")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Hugh Greenwood")] <- "Midfield"
players$PositionType[which(players$Player_Name == "James Cousins")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jack Scrimshaw")] <- "Defender"
players$PositionType[which(players$Player_Name == "David Cuningham")] <- "Forward"
players$PositionType[which(players$Player_Name == "Tom Williamson")] <- "Defender"
players$PositionType[which(players$Player_Name == "Matthew Kennedy")] <- "Midfield"
players$PositionType[which(players$Player_Name == "David Mackay")] <- "Forward"
players$PositionType[which(players$Player_Name == "Jake Kelly")] <- "Defender"
players$PositionType[which(players$Player_Name == "Chayce Jones")] <- "Forward"
players$PositionType[which(players$Player_Name == "Matt Guelfi")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Kyle Langford")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Harry Perryman")] <- "Forward"
players$PositionType[which(players$Player_Name == "Sam J. Reid")] <- "Defender"
players$PositionType[which(players$Player_Name == "Daniel Lloyd")] <- "Forward"
players$PositionType[which(players$Player_Name == "Luke Davies-Uniacke")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Callum L. Brown")] <- "Forward"
players$PositionType[which(players$Player_Name == "Adam Kennedy")] <- "Defender"
players$PositionType[which(players$Player_Name == "Kane Farrell")] <- "Forward"
players$PositionType[which(players$Player_Name == "Cam Sutcliffe")] <- "Forward"
players$PositionType[which(players$Player_Name == "Paddy Dow")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Josh Wagner")] <- "Defender"
players$PositionType[which(players$Player_Name == "Angus Brayshaw")] <- "Midfield"
players$PositionType[which(players$Player_Name == "James Harmes")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jack Higgins")] <- "Forward"
players$PositionType[which(players$Player_Name == "Paul Ahern")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Connor Blakely")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Alex Witherden")] <- "Defender"
players$PositionType[which(players$Player_Name == "Ben Crocker")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Colin O'Riordan")] <- "Defender"
players$PositionType[which(players$Player_Name == "Sydney Stack")] <- "Forward"
players$PositionType[which(players$Player_Name == "Jay Lockhart")] <- "Forward"
players$PositionType[which(players$Player_Name == "Brayden Fiorini")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Rhys Mathieson")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Riley Knight")] <- "Forward"
players$PositionType[which(players$Player_Name == "Tommy Sheridan")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Lin Jong")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Nic Newman")] <- "Defender"
players$PositionType[which(players$Player_Name == "Luke Dunstan")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Allen Christensen")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Jordan Murdoch")] <- "Defender"
players$PositionType[which(players$Player_Name == "Michael Rischitelli")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Nathan Hrovat")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Andy Otten")] <- "Defender"
players$PositionType[which(players$Player_Name == "David Myers")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Anthony Miles")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Dom Tyson")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Bernie Vince")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Dean Towers")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Will Langford")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Liam Baker")] <- "Defender"
players$PositionType[which(players$Player_Name == "Billy Hartung")] <- "Midfield"
players$PositionType[which(players$Player_Name == "Josh Green")] <- "Forward"
players$PositionType[which(players$Player_Name == "Josh Daicos")] <- "Defender"

# Standard .csv export 
write.csv(players, file = "players_pf.csv",
          na = "", 
          row.names = FALSE)

df.players$PositionType[which(df.players$Player_Name == "Liam Baker")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Shai Bolton")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Gryan Miers")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Sam Menegola")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jack Graham")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Callum Ah Chee")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Zac Bailey")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Cameron Ellis-Yolmen")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Sam Powell-Pepper")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Hunter Clark")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Ben Paton")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Jake Waterman")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Dom Sheed")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jack Petruccelle")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Tom Cole")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Bailey Smith")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Patrick Lipinski")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Adam Cerra")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Reece Conca")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Andrew Brayshaw")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "James Aish")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Blake Acres")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jordan Dawson")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Ryan Clarke")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jackson Thurlow")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Robbie Fox")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "James Rowbottom")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Harry Morrison")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Hugh Greenwood")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "James Cousins")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jack Scrimshaw")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "David Cuningham")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Tom Williamson")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Matthew Kennedy")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "David Mackay")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Jake Kelly")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Chayce Jones")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Matt Guelfi")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Kyle Langford")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Harry Perryman")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Sam J. Reid")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Daniel Lloyd")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Luke Davies-Uniacke")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Callum L. Brown")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Adam Kennedy")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Kane Farrell")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Cam Sutcliffe")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Paddy Dow")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Josh Wagner")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Angus Brayshaw")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "James Harmes")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jack Higgins")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Paul Ahern")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Connor Blakely")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Alex Witherden")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Ben Crocker")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Colin O'Riordan")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Sydney Stack")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Jay Lockhart")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Brayden Fiorini")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Rhys Mathieson")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Riley Knight")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Tommy Sheridan")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Lin Jong")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Nic Newman")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Luke Dunstan")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Allen Christensen")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Jordan Murdoch")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Michael Rischitelli")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Nathan Hrovat")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Andy Otten")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "David Myers")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Anthony Miles")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Dom Tyson")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Bernie Vince")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Dean Towers")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Will Langford")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Liam Baker")] <- "Defender"
df.players$PositionType[which(df.players$Player_Name == "Billy Hartung")] <- "Midfield"
df.players$PositionType[which(df.players$Player_Name == "Josh Green")] <- "Forward"
df.players$PositionType[which(df.players$Player_Name == "Josh Daicos")] <- "Defender"


# Standard .csv export 
write.csv(df.players, file = "afl_player_statistics_pf.csv",
          na = "", 
          row.names = FALSE)