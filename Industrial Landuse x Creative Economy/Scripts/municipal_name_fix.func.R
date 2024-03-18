municipal_name_fix.func <- function(df, name_variable){

  df <- df |> 
    mutate(
      df$name_variable = case_when(
        #Manchester
        df$name_variable %in% c("Manchester-by-the-Sea","Manchestr By Sea") ~ "Manchester",
        #Foxborough
        df$name_variable %in% c("Foxboro") ~ "Foxborough",
        #North Attleborough
        df$name_variable %in% c("North Attleboro") ~ "North Attleborough",
        #Boston
        df$name_variable %in% c("Roxbury", "Roxbury Crossing", "Mission Hill",
                             "Dorchester", "Grove Hall", "Dorchester Ctr",
                             "Uphams's Corner", "Mattapan", "South Boston",
                             "East Boston", "Charlestown", "Jamaica Plain",
                             "Roslindale", "West Roxbury", "Allston",
                             "Brighton", "Hyde Park") ~ "Boston",
        #Arlington
        df$name_variable %in% c("East Arlington", "Arlington Hts") ~ "Arlington",
        #Cambridge
        df$name_variable %in% c("North Cambridge", "East Cambridge") ~ "Cambridge",
        #Newton
        df$name_variable %in% c("Newtonville", "Newton Center", "Newton Highlands",
                             "Newton Lower Fls", "Newton Lower Falls", 
                             "Newton Upper Fls", "Newton Upper Falls",
                             "West Newton", "Auburndale", "Chestnut Hill",
                             "Waban", "Nonatum") ~ "Newton",
        #Easton
        df$name_variable %in% c("North Easton", "South Easton") ~ "Easton",
        #Weymouth
        df$name_variable %in% c("East Weymouth", "South Weymouth", "North Weymouth") ~ "Weymouth",
        #Falmouth
        df$name_variable %in% c("Teaticket", "North Falmouth", "East Falmouth",
                             "West Falmouth", "Woods Hole", "Waquoit") ~ "Falmouth",
        #Quincy
        df$name_variable %in% c("North Quincy", "Wollaston") ~ "Quincy",
        #Barnstable
        df$name_variable %in% c("Hyannis", "Centerville", "Cotuit", "Cummaquid", 
                             "Hyannis Port", "Marstons Mills", "Osterville", "West Hyannisport",
                             "West Barnstable") ~ "Barnstable",
        #Somerville
        df$name_variable %in% c("West Somerville") ~ "Somerville",
        #Plymouth
        df$name_variable %in% c("White Horse Bch", "Manomet") ~ "Plymouth",
        #Sandwich
        df$name_variable %in% c("East Sandwich", "Forestdale") ~ "Sandwich",
        #Wellfleet
        df$name_variable %in% c("South Wellfleet") ~ "Wellfleet",
        #Amherst
        df$name_variable %in% c("North Amherst") ~ "Amherst",
        #Uxbridge
        df$name_variable %in% c("Noth Uxbridge") ~ "Uxbridge",
        #Great Barrington
        df$name_variable %in% c("Housatonic") ~ "Great Barrington",
        #Tisbury
        df$name_variable %in% c("Vineyard Haven") ~ "Tisbury",
        #Scituate
        df$name_variable %in% c("Greenbush", "Humarock", "North Scituate") ~ "Scituate",
        #Newbury
        df$name_variable %in% c("Byfield") ~ "Newbury",
        #Walpole
        df$name_variable %in% c("East Walpole", "South Walpole") ~ "Walpole",
        #Grafton
        df$name_variable %in% c("North Grafton", "South Grafton") ~ "Grafton",
        #Colrain
        df$name_variable %in% c("Shattuckville") ~ "Colrain",
        #Russell
        df$name_variable %in% c("Woronoco") ~ "Russell",
        #Milton
        df$name_variable %in% c("Milton Village") ~ "Milton",
        #Sheffield
        df$name_variable %in% c("Ashley Falls") ~ "Sheffield",
        #North Attleborough
        df$name_variable %in% c("Attleboro Falls") ~ "North Attleborough",
        #Sutton
        df$name_variable %in% c("Manchaug", "Wilkinsonville") ~ "Sutton",
        #Hatfield
        df$name_variable %in% c("West Hatfield", "North Hatfield") ~ "Hatfield",
        #Douglas
        df$name_variable %in% c("East Douglas") ~ "Douglas",
        #Orleans
        df$name_variable %in% c("East Orleans") ~ "Orleans",
        #Attleboro
        df$name_variable %in% c("South Attleboro") ~ "Attleboro",
        #Templeton
        df$name_variable %in% c("Baldwinville", "East Templeton") ~ "Templeton",
        #Charlton
        df$name_variable %in% c("Charlton Depot", "Charlton City") ~ "Charlton",
        #Freetown
        df$name_variable %in% c("Assonet", "East Freetown") ~ "Freetown",
        #Millbury
        df$name_variable %in% c("West Millbury") ~ "Millbury",
        #Harwich
        df$name_variable %in% c("West Harwich", "Harwich Port", "East Harwich") ~ "Harwich",
        #Dennis
        df$name_variable %in% c("South Dennis", "West Dennis", "East Dennis",
                             "Dennis Port") ~ "Dennis",
        #Montague
        df$name_variable %in% c("Lake Pleasant", "Millers Falls", "Turners Falls") ~ "Montague",
        #Yarmouth
        df$name_variable %in% c("Bass River", "West Yarmouth", "Yarmouth Port",
                             "South Yarmouth") ~ "Yarmouth",
        #Marshfield
        df$name_variable %in% c("Marshfield Hills", "North Marshfield", "Ocean Bluff",
                             "Brant Rock", "Green Harbor") ~ "Marshfield",
        #Norwell
        df$name_variable %in% c("Accord") ~ "Norwell",
        #Brookline
        df$name_variable %in% c("Brookline Vlg") ~ "Brookline",
        #Gosnold
        df$name_variable %in% c("Cuttyhunk") ~ "Gosnold",
        #Williamsburg
        df$name_variable %in% c("Haydenville") ~ "Williamsburg",
        #Medford
        df$name_variable %in% c("West Medford") ~ "Medford",
        #Whately
        df$name_variable %in% c("West Whately") ~ "Whately",
        #Otis
        df$name_variable %in% c("East Otis") ~ "Otis",
        #Wellesley
        df$name_variable %in% c("Wellesley Hills", "Babson Park") ~ "Wellesley",
        #Agawam
        df$name_variable %in% c("Feeding Hills") ~ "Agawam",
        #Hardwick
        df$name_variable %in% c("Gilbertville", "South Harwich") ~ "Hardwick",
        #Bourne
        df$name_variable %in% c("Buzzards Bay", "Cataumet", "Monument Beach",
                             "Pocasset", "Sagamore", "Sagamore Beach") ~ "Bourne",
        #Westport
        df$name_variable %in% c("Westport Point") ~ "Westport",
        #Eastham
        df$name_variable %in% c("North Eastham") ~ "Eastham",
        #Wareham
        df$name_variable %in% c("East Wareham", "West Wareham", "Onset") ~ "Wareham",
        #Lanesborough
        df$name_variable %in% c("Berkshire") ~ "Lanesborough",
        #Palmer
        df$name_variable %in% c("Bondsville", "Thorndike", "Three Rivers") ~ "Palmer",
        #Haverhill
        df$name_variable %in% c("Bradford") ~ "Haverhill",
        #Pembroke
        df$name_variable %in% c("Bryantville") ~ "Pembroke",
        #Norton
        df$name_variable %in% c("Chartley") ~ "Norton",
        #Leicester
        df$name_variable %in% c("Cherry Valley", "Rochdale") ~ "Leicester",
        #Florida
        df$name_variable %in% c("Drury") ~ "Florida",
        #Taunton
        df$name_variable %in% c("East Taunton") ~ "Taunton",
        #East Bridgewater
        df$name_variable %in% c("Elmwood") ~ "East Bridgewater",
        #Sturbridge
        df$name_variable %in% c("Fiskdale") ~ "Sturbridge",
        #Northampton
        df$name_variable %in% c("Florence", "Leeds") ~ "Northampton",
        #Danvers
        df$name_variable %in% c("Hathorne") ~ "Danvers",
        #Springfield
        df$name_variable %in% c("Indian Orchard") ~ "Springfield",
        #Holden
        df$name_variable %in% c("Jefferson") ~ "Holden",
        #Lenox
        df$name_variable %in% c("Lenox Dale") ~ "Lenox",
        #Stockbrodge
        df$name_variable %in% c("Glendale") ~ "Stockbridge",
        #Chilmark
        df$name_variable %in% c("Menemsha") ~ "Chilmark",
        #New Marlborough
        df$name_variable %in% c("Mill River") ~ "New Marlborough",
        #Hanson
        df$name_variable %in% c("Monponsett") ~ "Hanson",
        #Gill
        df$name_variable %in% c("Mt Hermon") ~ "Gill",
        #Needham
        df$name_variable %in% c("Needham Heights") ~ "Needham",
        #Billerica
        df$name_variable %in% c("North Billerica") ~ "Billerica",
        #Carver
        df$name_variable %in% c("North Carver") ~ "Carver",
        #Chatham
        df$name_variable %in% c("North Chatham") ~ "Chatham"
      )
    )
  
}