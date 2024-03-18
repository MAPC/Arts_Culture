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
        #Barnstable
        name_variable %in% c("Hyannis", "Centerville", "Cotuit", "Cummaquid", 
                             "Hyannis Port", "Marstons Mills", "Osterville", "West Hyannisport",
                             "West Barnstable") ~ "Barnstable",
        #Somerville
        name_variable %in% c("West Somerville") ~ "Somerville",
        #Plymouth
        name_variable %in% c("White Horse Bch", "Manomet") ~ "Plymouth",
        #Sandwich
        name_variable %in% c("East Sandwich", "Forestdale") ~ "Sandwich",
        #Wellfleet
        name_variable %in% c("South Wellfleet") ~ "Wellfleet",
        #Amherst
        name_variable %in% c("North Amherst") ~ "Amherst",
        #Uxbridge
        name_variable %in% c("Noth Uxbridge") ~ "Uxbridge",
        #Great Barrington
        name_variable %in% c("Housatonic") ~ "Great Barrington",
        #Tisbury
        name_variable %in% c("Vineyard Haven") ~ "Tisbury",
        #Scituate
        name_variable %in% c("Greenbush", "Humarock", "North Scituate") ~ "Scituate",
        #Newbury
        name_variable %in% c("Byfield") ~ "Newbury",
        #Walpole
        name_variable %in% c("East Walpole", "South Walpole") ~ "Walpole",
        #Grafton
        name_variable %in% c("North Grafton", "South Grafton") ~ "Grafton",
        #Colrain
        name_variable %in% c("Shattuckville") ~ "Colrain",
        #Russell
        name_variable %in% c("Woronoco") ~ "Russell",
        #Milton
        name_variable %in% c("Milton Village") ~ "Milton",
        #Sheffield
        name_variable %in% c("Ashley Falls") ~ "Sheffield",
        #North Attleborough
        name_variable %in% c("Attleboro Falls") ~ "North Attleborough",
        #Sutton
        name_variable %in% c("Manchaug", "Wilkinsonville") ~ "Sutton",
        #Hatfield
        name_variable %in% c("West Hatfield", "North Hatfield") ~ "Hatfield",
        #Douglas
        name_variable %in% c("East Douglas") ~ "Douglas",
        #Orleans
        name_variable %in% c("East Orleans") ~ "Orleans",
        #Attleboro
        name_variable %in% c("South Attleboro") ~ "Attleboro",
        #Templeton
        name_variable %in% c("Baldwinville", "East Templeton") ~ "Templeton",
        #Charlton
        name_variable %in% c("Charlton Depot", "Charlton City") ~ "Charlton",
        #Freetown
        name_variable %in% c("Assonet", "East Freetown") ~ "Freetown",
        #Millbury
        name_variable %in% c("West Millbury") ~ "Millbury",
        #Harwich
        name_variable %in% c("West Harwich", "Harwich Port", "East Harwich") ~ "Harwich",
        #Dennis
        name_variable %in% c("South Dennis", "West Dennis", "East Dennis",
                             "Dennis Port") ~ "Dennis",
        #Montague
        name_variable %in% c("Lake Pleasant", "Millers Falls", "Turners Falls") ~ "Montague",
        #Yarmouth
        name_variable %in% c("Bass River", "West Yarmouth", "Yarmouth Port",
                             "South Yarmouth") ~ "Yarmouth",
        #Marshfield
        name_variable %in% c("Marshfield Hills", "North Marshfield", "Ocean Bluff",
                             "Brant Rock", "Green Harbor") ~ "Marshfield",
        #Norwell
        name_variable %in% c("Accord") ~ "Norwell",
        #Brookline
        name_variable %in% c("Brookline Vlg") ~ "Brookline",
        #Gosnold
        name_variable %in% c("Cuttyhunk") ~ "Gosnold",
        #Williamsburg
        name_variable %in% c("Haydenville") ~ "Williamsburg",
        #Medford
        name_variable %in% c("West Medford") ~ "Medford",
        #Whately
        name_variable %in% c("West Whately") ~ "Whately",
        #Otis
        name_variable %in% c("East Otis") ~ "Otis",
        #Wellesley
        name_variable %in% c("Wellesley Hills", "Babson Park") ~ "Wellesley",
        #Agawam
        name_variable %in% c("Feeding Hills") ~ "Agawam",
        #Hardwick
        name_variable %in% c("Gilbertville", "South Harwich") ~ "Hardwick",
        #Bourne
        name_variable %in% c("Buzzards Bay", "Cataumet", "Monument Beach",
                             "Pocasset", "Sagamore", "Sagamore Beach") ~ "Bourne",
        #Westport
        name_variable %in% c("Westport Point") ~ "Westport",
        #Eastham
        name_variable %in% c("North Eastham") ~ "Eastham",
        #Wareham
        name_variable %in% c("East Wareham", "West Wareham", "Onset") ~ "Wareham",
        #Lanesborough
        name_variable %in% c("Berkshire") ~ "Lanesborough",
        #Palmer
        name_variable %in% c("Bondsville", "Thorndike", "Three Rivers") ~ "Palmer",
        #Haverhill
        name_variable %in% c("Bradford") ~ "Haverhill",
        #Pembroke
        name_variable %in% c("Bryantville") ~ "Pembroke",
        #Norton
        name_variable %in% c("Chartley") ~ "Norton",
        #Leicester
        name_variable %in% c("Cherry Valley", "Rochdale") ~ "Leicester",
        #Florida
        name_variable %in% c("Drury") ~ "Florida",
        #Taunton
        name_variable %in% c("East Taunton") ~ "Taunton",
        #East Bridgewater
        name_variable %in% c("Elmwood") ~ "East Bridgewater",
        #Sturbridge
        name_variable %in% c("Fiskdale") ~ "Sturbridge",
        #Northampton
        name_variable %in% c("Florence", "Leeds") ~ "Northampton",
        #Danvers
        name_variable %in% c("Hathorne") ~ "Danvers",
        #Springfield
        name_variable %in% c("Indian Orchard") ~ "Springfield",
        #Holden
        name_variable %in% c("Jefferson") ~ "Holden",
        #Lenox
        name_variable %in% c("Lenox Dale") ~ "Lenox",
        #Stockbrodge
        name_variable %in% c("Glendale") ~ "Stockbridge",
        #Chilmark
        name_variable %in% c("Menemsha") ~ "Chilmark",
        #New Marlborough
        name_variable %in% c("Mill River") ~ "New Marlborough",
        #Hanson
        name_variable %in% c("Monponsett") ~ "Hanson",
        #Gill
        name_variable %in% c("Mt Hermon") ~ "Gill",
        #Needham
        name_variable %in% c("Needham Heights") ~ "Needham",
        #Billerica
        name_variable %in% c("North Billerica") ~ "Billerica",
        #Carver
        name_variable %in% c("North Carver") ~ "Carver",
        #Chatham
        name_variable %in% c("North Chatham") ~ "Chatham"
      )
    )
  
}