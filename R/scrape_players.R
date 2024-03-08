library(dplyr)
library(tidyr)
library(rvest)
library(nflreadr)
library(janitor)
library(stringr)

# dataset -----------------------------------------------------------------

load_spotrac_free_agents <- function(){
  cli::cli_progress_step("Scraping Spotrac Free Agents. Please be patient, the parser takes a while.")
  
  url <- "https://www.spotrac.com/nfl/free-agents/"
  
  players <- read_html(url) |> 
    html_elements(".player") |> 
    html_text()
  
  position <- read_html(url) |> 
    html_elements(".center:nth-child(2)") |> 
    html_text()
  
  old_team <- read_html(url) |> 
    html_elements(".center:nth-child(4)") |> 
    html_text()
  
  new_team <- read_html(url) |> 
    html_elements(".center:nth-child(5)") |> 
    html_text()
  
  free_agents<- tibble::tibble(
    player = str_trim(players), 
    position = str_trim(position), 
    old_team = str_trim(old_team), 
    new_team = str_trim(new_team)
  ) |> 
    filter(old_team != "From") |> 
    mutate(rank = row_number(), .before = player) |> 
    mutate(player = str_replace_all(player,',','')) |> 
    mutate(player = str_replace_all(player,' Jr.','')) |> 
    mutate(player = str_replace_all(player,' II','')) |> 
    mutate(player = str_replace_all(player,' III','')) |> 
    mutate(player = str_replace_all(player,' IV','')) |> 
    tidyr::separate(player,c("old","new"), "(?<=[a-z])(?=[A-Z])", 
                    extra = "merge", remove = FALSE) |> 
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
    mutate(new_team = case_when(
      new_team == "TBD" ~ NA, 
      new_team == "-" ~ NA, 
      TRUE ~ new_team
    )) |> 
    filter(is.na(new_team)) |> 
    select(player = name, position = position_roster)
  
}


# Gist provided by mrcaseb
# Gist URL https://gist.github.com/mrcaseb/143a25e975736e1c82faddfc08dbe755

# Scrape nflmockdraftdatabase consensus board from
# https://www.nflmockdraftdatabase.com
load_nflmockdraftdatabase_consensus_board <- function(year){
  cli::cli_progress_step("Loading {.val {year}}. Please be patient, the parser takes a while.")
  raw <- glue::glue("https://www.nflmockdraftdatabase.com/big-boards/{year}/consensus-big-board-{year}") |>
    rvest::read_html()
  
  mock_list <- raw |>
    rvest::html_elements(xpath = "//*[@class='mock-list-item']")
  
  pick_no <- mock_list |>
    rvest::html_elements(xpath = "//div[@class='left-container']//div[contains(concat(' ',normalize-space(@class),' '),' pick-number ')]") |>
    rvest::html_text()
  
  peak <- mock_list |>
    rvest::html_elements(xpath = "//div[@class='peak']//span") |>
    rvest::html_text()
  
  player <- mock_list |>
    rvest::html_elements(xpath = "//div[contains(concat(' ',normalize-space(@class),' '),' player-name ')]") |>
    rvest::html_text()
  
  college_details <- mock_list |>
    rvest::html_elements(xpath = "//div[@class='player-details college-details']")|>
    rvest::html_text()
  
  position <- college_details |>
    stringr::str_split_i("\\|", 1) |>
    stringr::str_trim()
  
  college <- college_details |>
    stringr::str_split_i("\\|", 2) |>
    stringr::str_trim() |>
    stringr::str_split_i("#|[:digit:]|--", 1)
  
  projection <- college_details |>
    stringr::str_split_i("\\|", 2) |>
    stringr::str_trim() |>
    stringr::str_split_i("#", 2)
  
  consensus_board <- tibble::tibble(
    current_rank = as.integer(pick_no),
    player = nflreadr::clean_player_names(player),
    position = position,
    college = college,
    projected_pick = as.integer(projection),
    highest_rank = as.integer(peak)
  )
  
  cfb_teams <- cfbfastR::load_cfb_teams()
  
  draft_players <- consensus_board |> 
    left_join(select(cfb_teams, school, abbreviation), 
              by = c("college" = "school")) |>  
    left_join(select(cfb_teams, alt_name1, abbreviation), 
              by = c("college" = "alt_name1")) |>  
    left_join(select(cfb_teams, alt_name2, abbreviation), 
              by = c("college" = "alt_name2")) |>  
    left_join(select(cfb_teams, alt_name3, abbreviation), 
              by = c("college" = "alt_name3")) |> 
    mutate(abbr = coalesce(abbreviation.x, abbreviation.x.x, abbreviation.y, abbreviation.y.y)) |> 
    mutate(abbr = case_when(
      college == "Baldwin Wallace" ~ "BAL", 
      college == "Louisiana-Lafayette" ~ "ULL", 
      college == "Minnesota Duluth" ~ "MNDU", 
      college == "Mississippi" ~ "MISS", 
      college == "West Florida" ~ "UWF", 
      college == "Toronto" ~ "TOR", 
      college == "Saint John's (MN)" ~ "SJU", 
      TRUE ~ abbr
    )) |> 
    mutate(player = paste0(player, " (", abbr, ")")) |> 
    mutate(position = if_else(position == "FB", "RB", position)) |> 
    select(player, position)
  
  return(draft_players)
}

YEAR <- as.integer(format(Sys.Date(), "%Y"))

free_agents <- load_spotrac_free_agents()

draft_board <- load_nflmockdraftdatabase_consensus_board(YEAR)

new_players <- bind_rows(free_agents, draft_board) |> 
  arrange(position, player)

saveRDS(new_players, paste0("Data/", "new_players.rds"))







