library(dplyr)
library(tidyr)
library(rvest)
library(nflreadr)
library(janitor)
library(stringr)

# dataset -----------------------------------------------------------------

load_spotrac_free_agents <- function(){
  url <- "https://www.spotrac.com/nfl/free-agents/"
  
  spotrac_raw <- httr::GET(
    'https://www.spotrac.com/nfl/free-agents/_/year/2024/status/available/sort/contract_value',
    httr::add_headers(`user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0")
  )
  
  players <- read_html(spotrac_raw) |> 
    html_elements(".player") |> 
    html_text()
  
  position <- read_html(spotrac_raw) |> 
    html_elements(".player+ .text-center") |> 
    html_text()
  
  free_agents1<- tibble::tibble(
    player = str_trim(players), 
    position = str_trim(position), 
  ) |> 
    mutate(rank = row_number(), .before = player) |> 
    separate_wider_delim(player, 
                         delim = " ", 
                         names = c("old", "new"), 
                         too_many = "merge") |> 
    mutate(new = str_trim(str_replace(new,',',''))) |> 
    # Andrew Van Ginkel's name shows up weird, thus we need to fix it. 
    mutate(new = case_when(
      str_detect(new, "Andrew Van Ginkel") ~ "Andrew Van Ginkel", 
      TRUE ~ new
    ))
  
  free_agents_good <- free_agents1 |> 
    filter(str_starts(new, "Jr.", negate = TRUE),  
           str_starts(new, "II", negate = TRUE),   
           str_starts(new, "III", negate = TRUE),  
           str_starts(new, "IV", negate = TRUE))
  
  free_agents_problem <- free_agents1 |> 
    filter(str_starts(new, "Jr.") | 
             str_starts(new, "II") |  
             str_starts(new, "III") | 
             str_starts(new, "IV")) |> 
    separate_wider_delim(new, 
                         delim = " ", 
                         names = c("old2", "new"), 
                         too_many = "merge") |> 
    mutate(new = str_trim(str_replace(new,',',''))) |> 
    select(-old2)
  
  
  
  free_agents <- bind_rows(free_agents_good, free_agents_problem) |> 
    arrange(rank) |> 
    mutate(name = nflreadr::clean_player_names(new), .after = "new") |> 
    select(-old, -new) |> 
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







