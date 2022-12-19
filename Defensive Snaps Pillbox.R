library(tidyverse)
library(nflverse)
library(RColorBrewer)
library(shades)
# Data--------------------------------------------------------------------------

YEAR <- 2022
TEAM <- "SF"

# use "get_current_week" for most recent week, or uncomment the other WEEK if 
# you want to specify a week
WEEK <- get_current_week(use_date = TRUE)
#WEEK <- 11

def_data_raw <- load_participation(seasons = YEAR, include_pbp = TRUE)

defense_1 <- def_data_raw |> 
  filter(defteam == TEAM, week == WEEK, pass == 1 | rush == 1, play_type != "no_play") |> 
  mutate(snap_num = row_number()) |> 
  select(week, snap_num, posteam, defteam, defense_players, drive, qtr, down, pass, rush, drive_end_transition)

OPP <- unique(defense_1$posteam)

defense_2 <- defense_1 |> 
  separate_rows(defense_players, sep = ";")

# Get player/roster info
def_positions <- load_players() |> 
  select(short_name, gsis_id, position_group, position) |> 
  mutate(short_name = ifelse(gsis_id == "00-0035678", "Dr.Jones", short_name)) |> 
  mutate(position = ifelse(short_name == "C.Dunlap", "DE", position)) |> 
  mutate(position_group = ifelse(short_name == "C.Dunlap", "DL", position_group))

defense_3 <-  
  left_join(defense_2, def_positions, by = c("defense_players" = "gsis_id")) |> 
  filter(position_group %in% c("DL", "LB", "DB")) |> 
  relocate(short_name, position_group, position, .after = defense_players) |> 
  group_by(defense_players) |> 
  mutate(player_snaps = n()) |> 
  ungroup() |> 
  mutate(position_group = factor(position_group, c("DL", "LB", "DB"))) |> 
  mutate(down = if_else(is.na(down), "2PT", as.character(down))) |> 
  mutate(down = factor(down, c("1", "2", "3", "4", "2PT"))) |> 
  arrange(position_group, player_snaps, desc(short_name))

def_player_level_df <- defense_3 |> 
  select(defense_players) |> 
  distinct() |> 
  mutate(y_pos = row_number())

defense_4 <- left_join(defense_3, def_player_level_df, by = "defense_players") |> 
  relocate(y_pos, .after = snap_num)

defense_bar_1 <- defense_4 |> 
  select(drive, snap_num) |> 
  arrange(drive) |> 
  distinct() |> 
  group_by(drive) |> 
  mutate(drive_snaps = n()) |> 
  ungroup() |> 
  select(-snap_num) |> 
  distinct() |> 
  mutate(def_drive_num = row_number()) |> 
  mutate(dummy = 1) |> 
  mutate(bar_color = case_when(
    def_drive_num %% 2 == 0 ~ "gray80", 
    TRUE ~ "gray95")) |> 
  arrange(-drive)

def_players <- defense_4 |> 
  select(position_group, short_name, defense_players) |> 
  distinct() |> 
  mutate(dummy = 1)

defense_bar_2 <- left_join(defense_bar_1, def_players, by = "dummy")

defense_bar <- left_join(defense_bar_2, select(defense_4, defense_players, y_pos), by = "defense_players") |> 
  distinct() |> 
  mutate(drive_snaps_calc = drive_snaps + 1) |> 
  arrange(drive)

defense <- left_join(defense_4, select(defense_bar, drive, def_drive_num), by = "drive") |> 
  mutate(x_pos = snap_num + def_drive_num - 1) |> 
  relocate(x_pos, def_drive_num, .after = snap_num) 

def_drive_outcomes_1 <- defense_bar |> 
  select(drive, def_drive_num, drive_snaps, drive_snaps_calc) |> 
  distinct()

def_drive_outcomes <- left_join(def_drive_outcomes_1, select(defense_1, drive, drive_end_transition), by = "drive") |> 
  distinct() |> 
  mutate(y_pos = ifelse(def_drive_num %% 2 == 0, -0.45, 0.05)) |> 
  mutate(x_end = cumsum(drive_snaps_calc)) |> 
  mutate(x_start = ifelse(def_drive_num == 1, 1, lag(x_end, 1L))) |> 
  mutate(x_mid = x_end - ((x_end - x_start) / 2)) |> 
  mutate(drive_end_transition = case_when(
    drive_end_transition == "BLOCKED_FG" ~ "BLCK FG", 
    drive_end_transition == "BLOCKED_FG_DOWNS" ~ "BLCK FG", 
    drive_end_transition == "BLOCKED_PUNT" ~ "BLCK PUNT", 
    drive_end_transition == "BLOCKED_PUNT_DOWNS" ~ "BLCK PUNT", 
    drive_end_transition == "END_GAME" ~ "GAME", 
    drive_end_transition == "END_HALF" ~ "HALF", 
    drive_end_transition == "FIELD_GOAL" ~ "FG", 
    drive_end_transition == "FUMBLE" ~ "FUM", 
    drive_end_transition == "FUMBLE_SAFETY" ~ "FUM SFTY", 
    drive_end_transition == "INTERCEPTION" ~ "INT", 
    drive_end_transition == "MISSED_FG" ~ "MISS FG", 
    drive_end_transition == "SAFETY" ~ "SFTY", 
    drive_end_transition == "TOUCHDOWN" ~ "TD", 
    TRUE ~ drive_end_transition
  ))

def_quarters <- defense |> 
  select(qtr, snap_num, x_pos) |> 
  distinct() |> 
  arrange(qtr) |> 
  group_by(qtr) |> 
  mutate(x_qtr_start = min(x_pos)) |> 
  mutate(x_qtr_end = max(x_pos)) |>
  mutate(x_qtr_mid = mean(x_pos)) |> 
  mutate(x_qtr_line = case_when(
    qtr != max(defense$qtr) ~ x_qtr_end
  )) |> 
  ungroup() |> 
  select(qtr, x_qtr_start, x_qtr_end, x_qtr_mid, x_qtr_line) |> 
  distinct() |> 
  mutate(x_qtr_end_line = case_when(
    lead(x_qtr_start) == x_qtr_end + 1 ~ x_qtr_end + 0.5,
    lead(x_qtr_start) == x_qtr_end + 2 ~ x_qtr_end + 1
  ))

# Generate a color scale based on Team Color

team_colors <- load_teams() |> 
  filter(team_abbr == TEAM) |> 
  select(team_abbr, team_color, team_color2)

# Plot--------------------------------------------------------------------------

def_max_player_num <- max(defense$y_pos)
def_max_player_name_length <- (max(nchar(defense_bar$short_name)) + 2) / 100  

bar_colors <- c("grey95", "grey85", "grey95", "grey85", "grey95", "grey85", 
                "grey95", "grey85", "grey95", "grey85", "grey95", "grey85", 
                "grey95", "grey85", "grey95", "grey85", "grey95", "grey85", 
                "grey95", "grey85")

tick_colors <- c("#337eff", "darkorange1", "red3", "black", "purple")

(def_pill <- 
  ggplot() + 
    geom_col(defense_bar,
             mapping = aes(x = drive_snaps_calc,
                           y = reorder(paste(short_name, " (", position_group, ")", sep = ""), y_pos),
                           fill = reorder(def_drive_num, -def_drive_num)),
             color = "grey70",
             show.legend = F) +
    scale_fill_manual(values = bar_colors) +
    geom_segment(defense,
                 mapping = aes(x = x_pos,
                               xend = x_pos,
                               y = ifelse(pass == 1, y_pos + 0.3, y_pos - 0.3),
                               yend = ifelse(pass == 1, y_pos - 0.1, y_pos + 0.1)),
                 color = "grey30",
                 linewidth = 1,
                 lineend = "round") +
    geom_segment(defense,
                 mapping = aes(x = x_pos + 0.02,
                               xend = x_pos + 0.02,
                               y = ifelse(pass == 1, y_pos + 0.29, y_pos - 0.29),
                               yend = ifelse(pass == 1, y_pos - 0.09, y_pos + 0.09),
                               color = factor(down)),
                 #color = down),
                 linewidth = 0.6,
                 lineend = "round") +
    scale_color_manual(values = tick_colors, name = "Downs:", labels = c("1st", "2nd", "3rd", "4th", "2PT")) +
    geom_text(def_drive_outcomes, 
              mapping = aes(x = x_mid, 
                            y = y_pos, 
                            label = drive_end_transition), 
              size = 2.5) +
    scale_y_discrete(expand = expansion(mult = c(0.1, 0.07))) + 
    scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
    geom_linerange(def_quarters, mapping = aes(x = x_qtr_end_line, 
                                               ymin = 0.5, 
                                               ymax = def_max_player_num + 1 # 0.75 
    ), 
    color = "orange", 
    alpha = 0.7) +
    geom_text(def_quarters, 
              mapping = aes(x = x_qtr_mid, 
                            y = def_max_player_num + 1, 
                            label = ifelse(qtr == 5, "OT", paste("Q", qtr, sep = ""))), 
              size = 2.5) + 
    labs(
      title = paste(TEAM, " Defensive Snaps: Week ", WEEK, " (", OPP, ")", sep = ""),
      subtitle = "Upticks = pass, Downticks = run", 
      caption = "Data: @nflverse   |   Chart: @josephjefe",
      tag = TEAM
    ) + 
    theme_minimal() +
    theme(
      axis.title.y = element_blank(), 
      axis.title.x = element_blank(), 
      axis.text.x = element_blank(), 
      axis.ticks.y = element_line(), 
      plot.background = element_rect(fill = "white"), 
      legend.position = "bottom", 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(), 
      panel.grid.major.y = element_blank(), 
      plot.title = element_text(hjust = 0.5), 
      plot.subtitle = element_text(hjust = 0.5), 
      plot.tag.position = c(def_max_player_name_length, 0.95), 
      plot.tag = element_nfl_logo(size = 1.5),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-10,-10, 0, 0), 
      legend.key.width = unit(0.25, "cm"), 
    )
)

ggsave(def_pill, path = "./images", filename = paste("Week ", WEEK, " ", TEAM, " Defensive Snaps ", "(", OPP, ").png", sep = "")) 

