library(tidyverse)
library(nflverse)
library(RColorBrewer)
library(shades)
# Data--------------------------------------------------------------------------

YEAR <- 2022
TEAM <- "SF"

df <- load_players()
dic <- dictionary_pbp
# use "get_current_week" for most recent week, or uncomment the other WEEK if 
# you want to specify a week
WEEK <- get_current_week(use_date = TRUE)
#WEEK <- 11

dic <- dictionary_pbp

off_data_raw <- load_participation(seasons = YEAR, include_pbp = TRUE) 

offense_1 <- off_data_raw |> 
  filter(posteam == TEAM, week == WEEK, pass == 1 | rush == 1, !is.na(epa), play_type != "no_play") |> 
  mutate(snap_num = row_number()) |> 
  mutate(rusher_receiver_id = case_when(
    rush == 1 ~ coalesce(rusher_player_id, receiver_player_id), 
    pass == 1 ~ coalesce(receiver_player_id, rusher_player_id)
  )) |> 
  select(week, snap_num, posteam, defteam, offense_players, drive, qtr, down, play_type, pass, rush, drive_end_transition, rusher_player_id, receiver_player_id, rusher_receiver_id, desc)

OPP <- unique(offense_1$defteam)

offense_2 <- offense_1 |> 
  separate_rows(offense_players, sep = ";")

# Get player/roster info
off_positions <- load_players() |> 
  select(short_name, gsis_id, position_group, position)

offense_3 <-  
  left_join(offense_2, off_positions, by = c("offense_players" = "gsis_id")) |> 
  filter(position_group %in% c("TE", "WR", "RB", "OL", "QB")) |> 
  relocate(short_name, position_group, position, .after = offense_players) |> 
  group_by(offense_players) |> 
  mutate(player_snaps = n()) |> 
  ungroup() |> 
  mutate(position_group = factor(position_group, c("TE", "WR", "RB", "OL", "QB"))) |> 
  mutate(down = if_else(is.na(down), "2PT", as.character(down))) |> 
  mutate(down = factor(down, c("1", "2", "3", "4", "2PT"))) |> 
  mutate(ball_player = case_when(
    rusher_receiver_id == offense_players ~ "*"
  )) |> 
  arrange(position_group, player_snaps, desc(short_name))

off_player_level_df <- offense_3 |> 
  select(offense_players) |> 
  distinct() |> 
  mutate(y_pos = row_number())

offense_4 <- left_join(offense_3, off_player_level_df, by = "offense_players") |> 
  relocate(y_pos, .after = snap_num)

offense_bar_1 <- offense_4 |> 
  select(drive, snap_num) |> 
  arrange(drive) |> 
  distinct() |> 
  group_by(drive) |> 
  mutate(drive_snaps = n()) |> 
  ungroup() |> 
  select(-snap_num) |> 
  distinct() |> 
  mutate(off_drive_num = row_number()) |> 
  mutate(dummy = 1) |> 
  mutate(bar_color = case_when(
    off_drive_num %% 2 == 0 ~ "gray80", 
    TRUE ~ "gray95")) |> 
  arrange(-drive)

off_players <- offense_4 |> 
  select(position_group, short_name, offense_players) |> 
  distinct() |> 
  mutate(dummy = 1)

offense_bar_2 <- left_join(offense_bar_1, off_players, by = "dummy")

offense_bar <- left_join(offense_bar_2, select(offense_4, offense_players, y_pos), by = "offense_players") |> 
  distinct() |> 
  mutate(drive_snaps_calc = drive_snaps + 1) |> 
  arrange(drive)

offense <- left_join(offense_4, select(offense_bar, drive, off_drive_num), by = "drive") |> 
  mutate(x_pos = snap_num + off_drive_num - 1) |> 
  relocate(x_pos, off_drive_num, .after = snap_num) 

off_drive_outcomes_1 <- offense_bar |> 
  select(drive, off_drive_num, drive_snaps, drive_snaps_calc) |> 
  distinct()

off_drive_outcomes <- left_join(off_drive_outcomes_1, select(offense_1, drive, drive_end_transition), by = "drive") |> 
  distinct() |> 
  mutate(y_pos = ifelse(off_drive_num %% 2 == 0, -0.45, 0.05)) |> 
  mutate(x_end = cumsum(drive_snaps_calc)) |> 
  mutate(x_start = ifelse(off_drive_num == 1, 1, lag(x_end, 1L))) |> 
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

off_quarters <- offense |> 
  select(qtr, snap_num, x_pos) |> 
  distinct() |> 
  arrange(qtr) |> 
  group_by(qtr) |> 
  mutate(x_qtr_start = min(x_pos)) |> 
  mutate(x_qtr_end = max(x_pos)) |>
  mutate(x_qtr_mid = mean(x_pos)) |> 
  mutate(x_qtr_line = case_when(
    qtr != max(offense$qtr) ~ x_qtr_end
  )) |> 
  ungroup() |> 
  select(qtr, x_qtr_start, x_qtr_end, x_qtr_mid, x_qtr_line) |> 
  distinct() |> 
  mutate(x_qtr_end_line = case_when(
    lead(x_qtr_start) == x_qtr_end + 1 ~ x_qtr_end + 0.5,
    lead(x_qtr_start) == x_qtr_end + 2 ~ x_qtr_end + 1
  ))


# Plot--------------------------------------------------------------------------

off_max_player_num <- max(offense$y_pos)
off_max_player_name_length <- (max(nchar(offense_bar$short_name)) + 2) / 100  


bar_colors <- c("grey95", "grey85", "grey95", "grey85", "grey95", "grey85", 
                "grey95", "grey85", "grey95", "grey85", "grey95", "grey85", 
                "grey95", "grey85", "grey95", "grey85", "grey95", "grey85", 
                "grey95", "grey85")

tick_colors <- c("#337eff", "darkorange1", "red3", "black", "purple")

(off_pill <- 
    ggplot() + 
    geom_col(offense_bar,
             mapping = aes(x = drive_snaps_calc,
                           y = reorder(paste(short_name, " (", position_group, ")", sep = ""), y_pos),
                           fill = reorder(off_drive_num, -off_drive_num)),
             color = "grey70",
             show.legend = F) +
    scale_fill_manual(values = bar_colors) +
    geom_segment(offense,
                 mapping = aes(x = x_pos,
                               xend = x_pos,
                               y = ifelse(pass == 1, y_pos + 0.3, y_pos - 0.3),
                               yend = ifelse(pass == 1, y_pos - 0.1, y_pos + 0.1)),
                 color = "grey30",
                 linewidth = 1,
                 lineend = "round") +
    geom_segment(offense,
                 mapping = aes(x = x_pos + 0.02,
                               xend = x_pos + 0.02,
                               y = ifelse(pass == 1, y_pos + 0.29, y_pos - 0.29),
                               yend = ifelse(pass == 1, y_pos - 0.09, y_pos + 0.09),
                               color = factor(down)),
                 #color = down),
                 linewidth = 0.6,
                 lineend = "round") +
    scale_color_manual(values = tick_colors, name = "Downs:", labels = c("1st", "2nd", "3rd", "4th", "2PT")) +
    geom_text(off_drive_outcomes, 
              mapping = aes(x = x_mid, 
                            y = y_pos, 
                            label = drive_end_transition), 
              size = 2.5) +
    geom_text(offense, 
              mapping = aes(x = x_pos, 
                            y = ifelse(pass == 1, y_pos - 0.38, y_pos + 0.22),
                            label = ball_player), 
              size = 3) +
    scale_y_discrete(expand = expansion(mult = c(0.1, 0.07))) + 
    scale_x_continuous(expand = expansion(mult = c(0, 0))) + 
    geom_linerange(off_quarters, mapping = aes(x = x_qtr_end_line, 
                                               ymin = 0.5, 
                                               ymax = off_max_player_num + 0.75 
    ), 
    color = "orange", 
    alpha = 0.7) +
    geom_text(off_quarters, 
              mapping = aes(x = x_qtr_mid, 
                            y = off_max_player_num + 1, 
                            label = ifelse(qtr == 5, "OT", paste("Q", qtr, sep = ""))), 
              size = 2.5) + 
    #geom_text(aes(x = 1.05, y = 1.05, label = "BUF")) +
    labs(
      title = paste(TEAM, " Offensive Snaps: Week ", WEEK, " (", OPP, ")", sep = ""),
      subtitle = "Upticks = pass, Downticks = run, * = Carry/Target", 
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
      plot.subtitle = element_text(size = 9, hjust = 0.5), 
      plot.tag.position = c(off_max_player_name_length, 0.95), 
      plot.tag = element_nfl_logo(size = 1.5),
      legend.justification="left",
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-10,-10, 0, 0), 
      legend.key.width = unit(0.25, "cm"), 
      
    )
)

ggsave(off_pill, path = "./images", filename = paste("Week ", WEEK, " ", TEAM, " Offensive Snaps ", "(", OPP, ").png", sep = "")) 

