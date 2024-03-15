municipal_name_fix.func <- function(df, name_variable){

  df <- df |> 
    mutate(
      name_variable = case_when(
        #Manchester
        name_variable %in% c("Manchester-by-the-Sea","Manchestr By Sea") ~ "Manchester",
        #Foxborough
        name_variable %in% c("Foxboro") ~ "Foxborough",
        #North Attleborough
        name_variable %in% c("North Attleboro") ~ "North Attleborough",
        #Boston
        name_variable %in% c("Roxbury", "Roxbury Crossing", "Mission Hill",
                             "Dorchester", "Grove Hall", "Dorchester Ctr",
                             "Uphams's Corner", "Mattapan", "South Boston",
                             "East Boston", "Charlestown", "Jamaica Plain",
                             "Roslindale", "West Roxbury", "Allston",
                             "Brighton", "Hyde Park") ~ "Boston",
        #Arlington
        name_variable %in% c("East Arlington", "Arlington Hts") ~ "Arlington",
        #Cambridge
        name_variable %in% c("North Cambridge", "East Cambridge") ~ "Cambridge",
        #Newton
        name_variable %in% c("Newtonville", "Newton Center", "Newton Highlands",
                             "Newton Lower Fls", "Newton Lower Falls", 
                             "Newton Upper Fls", "Newton Upper Falls",
                             "West Newton", "Auburndale", "Chestnut Hill",
                             "Waban", "Nonatum") ~ "Newton",
        #Easton
        name_variable %in% c("North Easton", "South Easton") ~ "Easton",
        #Weymouth
        name_variable %in% c("East Weymouth", "South Weymouth", "North Weymouth") ~ "Weymouth",
        #Falmouth
        name_variable %in% c("Teaticket", "North Falmouth", "East Falmouth",
                             "West Falmouth", "Woods Hole", "Waquoit") ~ "Falmouth",
        #Quincy
        name_variable %in% c("North Quincy", "Wollaston") ~ "Quincy",
        name_variable %in% c()
        
      )
    )
  
}