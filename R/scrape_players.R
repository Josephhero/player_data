library(dplyr)
library(tidyr)
library(rvest)
library(nflreadr)
library(janitor)
library(stringr)

# dataset -----------------------------------------------------------------

load_spotrac_free_agents <- function(){
  url <- "https://www.spotrac.com/nfl/free-agents/"
  
  spotrack_raw <- httr::GET(
    'https://www.spotrac.com/nfl/free-agents/_/year/2024/status/available/sort/contract_value',
    httr::add_headers(`user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0")
  )
  
  players <- read_html(spotrack_raw) |> 
    html_elements(".player") |> 
    html_text()
  
  position <- read_html(spotrack_raw) |> 
    html_elements(".player+ .text-center") |> 
    html_text()
  
  free_agents1<- tibble::tibble(
    player = str_trim(str_replace(players,',','')), 
    position = str_trim(position), 
  ) |> 
    mutate(rank = row_number(), .before = player)
    # separate_wider_delim(player, 
    #                      delim = " ", 
    #                      names = c("old", "new"), 
    #                      too_many = "merge") |> 
    # mutate(new = str_trim(str_replace(new,',',''))) |> 
    # # Andrew Van Ginkel's name shows up weird, thus we need to fix it. 
    # mutate(new = case_when(
    #   str_detect(new, "Andrew Van Ginkel") ~ "Andrew Van Ginkel", 
    #   TRUE ~ new
    # ))
  
  free_agents_good <- free_agents1 |> 
    filter(str_starts(player, "Jr.", negate = TRUE),  
           str_starts(player, "II", negate = TRUE),   
           str_starts(player, "III", negate = TRUE),  
           str_starts(player, "IV", negate = TRUE))
  
  free_agents_problem <- free_agents1 |> 
    filter(str_starts(player, "Jr.") | 
             str_starts(player, "II") |  
             str_starts(player, "III") | 
             str_starts(player, "IV")) |> 
    separate_wider_delim(player, 
                         delim = " ", 
                         names = c("old2", "player"), 
                         too_many = "merge") |> 
    mutate(player = str_trim(str_replace(player,',',''))) |> 
    select(-old2)
  
  
  
  free_agents <- bind_rows(free_agents_good, free_agents_problem) |> 
    arrange(rank) |> 
    mutate(name = nflreadr::clean_player_names(player), .after = "player") |> 
    select(-player) |> 
    mutate(position_roster = case_when(
      position %in% c("LT", "RT", "T") ~ "OT", 
      position %in% c("LG", "RG", "G") ~ "OG", 
      position %in% c("FB") ~ "RB", 
      position %in% c("DE", "OLB") ~ "EDGE", 
      position %in% c("ILB") ~ "LB", 
      position %in% c("LDT", "RDT", "NT", "DT") ~ "DL", 
      position %in% c("FS", "SS") ~ "S", 
      TRUE ~ position
    ), .after = position) |> 
    select(player = name, position = position_roster)

}

free_agents <- load_spotrac_free_agents()

saveRDS(free_agents, paste0("Data/", "new_players.rds"))







