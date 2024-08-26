# Fix team names function

fix_team_names <- function(team_name_vector) {
  map_chr(team_name_vector, ~ case_when(
    str_detect(., "(^Adelaide)|(Kuwarna)") ~ "Adelaide Crows",
    str_detect(., "^Brisbane") ~ "Brisbane Lions",
    str_detect(., "^Carlton") ~ "Carlton",
    str_detect(., "^Collingwood") ~ "Collingwood Magpies",
    str_detect(., "^Essendon") ~ "Essendon Bombers",
    str_detect(., "(^Fremantle)|(Walyalup)") ~ "Fremantle Dockers",
    str_detect(., "^Geelong") ~ "Geelong Cats",
    str_detect(., "^Gold Coast") ~ "Gold Coast Suns",
    str_detect(., "(^GWS)|(^Greater)") ~ "GWS Giants",
    str_detect(., "^Hawthorn") ~ "Hawthorn Hawks",
    str_detect(., "(^Melbourne)|(Narrm)") ~ "Melbourne Demons",
    str_detect(., "^North Melb") ~ "North Melbourne Kangaroos",
    str_detect(., "(^Port Adel)|(Yartapuulti)") ~ "Port Adelaide Power",
    str_detect(., "^Richmond") ~ "Richmond Tigers",
    str_detect(., "(^St Kilda)|(Euro-Yroke)") ~ "St Kilda Saints",
    str_detect(., "^Sydney") ~ "Sydney Swans",
    str_detect(., "(^West Coast)|(Waalitj Marawar)") ~ "West Coast Eagles",
    str_detect(., "(^Western Bulldogs)|(Bulldogs)") ~ "Western Bulldogs",
    TRUE ~ . # Default case to return the original team name
  ))
}