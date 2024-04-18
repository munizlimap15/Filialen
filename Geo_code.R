# Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(purrr)  # Added for the map2 function
library(lwgeom)

# Set working directory
#setwd("C:/Users/pedro/Desktop/JO_bonus")

# Load data
filial_data <- read_excel("Filialdaten.xlsx")
states_sf <- st_read("states.shp") %>%
  st_transform(4326) %>%
  mutate(state = recode(state, `Burgenland` = "Burgenland"),
         State = state) 

# Assuming filial_data is your data frame
columns_to_round <- names(filial_data)[6:14]
# Use lapply to loop over the selected columns and round each
filial_data[columns_to_round] <- lapply(filial_data[columns_to_round], round, 4)

states_sf$state <- NULL



# Define Austrian states data with Purchasing Power Index and ranks
# https://www.wohnkultur.co.at/gfk-veroeffentlicht-kaufkraft-2021-fuer-oesterreich-und-die-schweiz/
austrian_states <- tibble(
  State = c( "Vienna", "Upper Austria", "Lower Austria","Salzburg", "Tyrol","Vorarlberg", "Styria", "Carinthia", "Burgenland"),
  PPI = c(1, 2, 3, 4, 5, 6, 7, 8,9 )
) %>%
  arrange(desc(PPI)) 

filial_data <- filial_data %>%
  group_by(B_LAND) %>%
  mutate(median_index = median(Kaufkraft_KOPF)) %>% # Calculate median per state
  ungroup() %>%
  mutate(Rank = dense_rank(desc(median_index))) %>%
  select(-median_index)

quart_thresh <- quantile(filial_data$Umsatz, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
filial_data <- filial_data %>%
  mutate(Cluster = cut(Umsatz,
                          breaks = c(-Inf,quart_thresh, Inf),
                          labels = c("Basic", "Bronze", "Silver", "Gold"),
                          include.lowest = TRUE
    )
  )


filial_data <- merge(filial_data, austrian_states, by.x = "Rank", by.y = "PPI", all.x = TRUE)
filial_data <- filial_data %>%
  mutate(Normalized_Revenue = Umsatz / `Kund. Anz.`)


n_points_df = filial_data %>%
  group_by(B_LAND, State) %>%
  summarise(npoints = n_distinct(FILIALE)) 


states_sf <- merge(states_sf , n_points_df, by= "State", all.x = TRUE)%>%
  mutate(npoints = if_else(State == "Tyrol", npoints / 2, npoints)) # I have done this because Tyrol has 2 domains.


sampled_points_sf <- lapply(1:nrow(states_sf), function(i) {
  if (states_sf$npoints[i] > 0) {
    pts <- st_sample(states_sf[i, ], size = states_sf$npoints[i], type = "random", by_polygon = FALSE)
    if (length(pts) > 0) {  # Check if any points were generated
      return(st_sf(geometry = pts, State = rep(states_sf$State[i], length(pts)), B_LAND = rep(states_sf$B_LAND[i], length(pts))))
    }
  }
}) %>% do.call(rbind, .)  # Combine all results into one sf object

# Add a row number within each state group to both datasets before joining
filial_data <- filial_data %>%
  group_by(State, B_LAND) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

sampled_points_sf <- sampled_points_sf %>%
  group_by(State, B_LAND) %>%
  mutate(row_id = row_number()) %>%
  ungroup()

# Perform the join using the additional row_id to align rows more tightly
merged_data <- filial_data %>%
  left_join(sampled_points_sf, by = c("State", "B_LAND", "row_id")) %>%
  select(-row_id) %>%
  st_as_sf()#%>%  mutate(across(.cols = 7:17, .fns = ~round(., digits = 2)))



rm(austrian_states)
rm(sampled_points_sf)
rm(filial_data )
rm(n_points_df)


abbreviated_names <- c(
  "Rank" = "Rnk",
  "FILIALE" = "FILIALE",
  "B_LAND" = "B_LAND",
  "VERKAUFS_M2" = "SalesAreaM2",
  "Umsatz" = "Revenue",
  "Kund. Anz." = "CustomerCount",
  "Aktionsanteil" = "AShare",
  "Anteil Clever" = "BShare",
  "Anteil Ja!Natürlich" = "CShare",
  "Anteil Feinkost" = "DShare",
  "Anteil Obst&Gemüse" = "EShare",
  "Anteil BILLA Marke" = "FShare",
  "Anteil BILLA Corso" = "GShare",
  "Anteil Getränke ohne Alkohol" = "NonAlcDrinkShare",
  "Anteil Alkohol" = "AlcoholShare",
  "ANZAHL_Verbrauchermärkte_im Umkreis von 20min" = "Mkts20MinCount",
  "ANZAHL_Diskonter_im Umkreis von 20min" = "Discount20MinCount",
  "KaufKraft_Drogeriefachhandel_KOPF" = "DrugPPCap",
  "KaufKraft_Lebensmittelhandel_KOPF" = "GroceryPPCap",
  "Kaufkraft_KOPF" = "OverallPPCap",
  "median_index" = "MedIndex",
  "State" = "State",
  "PPI" = "PPI",
  "Cluster" = "Cluster",
  "Normalized_Revenue" = "NormRevenue",
  "geometry" = "geometry"
)


names(merged_data) <- abbreviated_names[names(merged_data)]


