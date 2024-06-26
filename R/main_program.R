################################################################################
#######                           Main program                           #######
################################################################################


# Utilities --------------------------------------------------------------------

## General utilities -----------------------------------------------------------
source("./R/utilities.R")
## API-utilities ---------------------------------------------------------------
source("./R/API-utilities.R")


# Exercice 1 : Se familiariser avec l’API de TravelTime ------------------------

## API parameters --------------------------------------------------------------
source("./R/API_parameters.R")


# Exercice 2 : Travailler avec des données géographiques -----------------------

## Gare GPS --------------------------------------------------------------------

if (!dir.exists(paths = "data")) {
    dir.create("data")
}

if (file.exists("data/gares_fr.csv")) {
    csv_stations <- get_data(path = "data/gares_fr.csv")
} else {
    STATIONS_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/d22ba593-90a4-4725-977c-095d1f654d28"
    csv_stations <- read.csv2(STATIONS_DATA_URL)
    write_data(data = csv_stations, path = "data/gares_fr.csv")
}


# Exercice 3 : Calcul du temps de transport ------------------------------------

## Temps entre 2 gares ---------------------------------------------------------

get_travel_time_between_stations("Lyon-Perrache", "Toulouse-Matabiau")
get_travel_time_between_stations("Toulouse-Matabiau", "Lyon-Perrache")
get_travel_time_between_stations("Ste-Léocadie", "St-Dié-des-Vosges")

## Calcul de la matrix des distances -------------------------------------------

list_stations <- c(
    "Paris-Nord", "Lyon-Perrache", "Marseille-St-Charles",
    "Toulouse-Matabiau", "Lille-Flandres", "Bordeaux-St-Jean",
    "Nice-Ville", "Nantes", "Strasbourg-Ville", "Montpellier-St-Roch"
)
if (file.exists("data/time_matrix.csv")) {
    time_matrix <- get_data(
        path = "data/time_matrix.csv",
        check.names = FALSE
    ) |> as.matrix()
} else {
    time_matrix <- matrix(
        data = NA_real_,
        ncol = length(list_stations),
        nrow = length(list_stations),
        dimnames = list(list_stations, list_stations)
    )
    for (index_1 in seq_along(list_stations)) {
        station_1 <- list_stations[index_1]
        for (index_2 in seq_len(index_1 - 1)) {
            station_2 <- list_stations[index_2]
            time_value <- get_travel_time_between_stations(station_1, station_2)
            time_matrix[index_1, index_2] <- time_value
            time_matrix[index_2, index_1] <- time_value
        }
    }
    write_data(
        data = time_matrix,
        path = "data/time_matrix.csv",
        row.names = TRUE,
        col.names = TRUE
    )
}

## Gares en dessous de 4h30 ----------------------------------------------------

stations_under_four <- omnibus::isTRUENA(time_matrix <= 4.5)


# Exercice 4 : Analyse du trafic aérien ----------------------------------------

## Traffic aérien --------------------------------------------------------------

if (!dir.exists(paths = "data")) {
    dir.create("data")
}

if (file.exists("data/air_traffic.csv")) {
    air_traffic_df <- get_data(path = "data/air_traffic.csv")
} else {
    # On définit l'URL des données
    AIR_TRAFFIC_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/0c0a451e-983b-4f06-9627-b5ff1bccd2fc"
    air_traffic_df <- read.csv2(AIR_TRAFFIC_DATA_URL)
    write_data(data = air_traffic_df, path = "data/air_traffic.csv")
}

## Compute PKT -----------------------------------------------------------------

compute_PKT(city_1 = "PRISTINA", city_2 = "BALE")
compute_PKT(city_1 = "PARIS", city_2 = "TOULOUSE")

total_PKT <- 0

for (index_1 in seq_along(list_stations)) {
    station_1 <- list_stations[index_1]
    for (index_2 in seq_len(index_1 - 1)) {
        station_2 <- list_stations[index_2]
        if (stations_under_four[station_1, station_2]) {
            city_1 <- extract_city_name(station_1)
            city_2 <- extract_city_name(station_2)
            total_PKT <- total_PKT + compute_PKT(city_1, city_2)
        }
    }
}

GCO2_PER_PKT <- 80

# On estime les émissions de CO2 en tCO2éq
cat(sprintf("En 2019, environ %.2f tCO2éq aurait pu être évités",
            total_PKT * GCO2_PER_PKT / 1000000))


# Exercice 5 : Visualiser des données géographiques  ---------------------------

table_emission <- data.frame(
    city1 = character(0L),
    city2 = character(0L),
    lat1 = numeric(0L),
    lng1 = numeric(0L),
    lat2 = numeric(0L),
    lat2 = numeric(0L),
    emissions = numeric(0L)
)

for (index_1 in seq_along(list_stations)) {
    station_1 <- list_stations[index_1]
    city_1 <- extract_city_name(station_1)
    coord1 <- get_station_coordinates(station_1)
    for (index_2 in seq_len(index_1 - 1)) {
        station_2 <- list_stations[index_2]
        city_2 <- extract_city_name(station_2)
        coord2 <- get_station_coordinates(station_2)
        if (stations_under_four[station_1, station_2]) {
            value_PKT <- compute_PKT(city_1, city_2)
            table_emission <- rbind(
                table_emission,
                data.frame(
                    city1 = city_1,
                    city2 = city_2,
                    lat1 = coord1[1L],
                    lng1 = coord1[2L],
                    lat2 = coord2[1L],
                    lng2 = coord2[2L],
                    emissions = value_PKT * GCO2_PER_PKT / 1000000
                )
            )
        }
    }
}

# Affichage carte
map <- leaflet::leaflet() |>
    leaflet::addTiles(urlTemplate = TILES_URL)


# Affichage des lignes
emission_by_route_df <- table_emission

for (i in 1:nrow(emission_by_route_df)) {
    # Définir les options par défaut pour les lignes
    lat_vector <- c(emission_by_route_df$lat1[i], emission_by_route_df$lat2[i])
    lng_vector <- c(emission_by_route_df$lng1[i], emission_by_route_df$lng2[i])
    color <- "black" # couleur par défaut
    opacity <- 0.5
    weight <- 1 # poids par défaut

    # Si les émissions sont supérieures à zéro, ajuster la couleur et le poids
    if (emission_by_route_df$emissions[i] > 0) {
        color <- "red"
        weight <- emission_by_route_df$emissions[i] / 10000
    }

    # Ajouter des lignes à la carte
    map <- map |>
        leaflet::addPolylines(lat = lat_vector, lng = lng_vector, color = color, opacity = opacity, weight = weight)
}

# Affichage des cercles

# Définir les options de label personnalisées
custom_label_options <- leaflet::labelOptions(noHide = TRUE, style = list("background" = "rgba(255, 255, 255, 0.5)"))

# Fonction pour ajouter des marqueurs circulaires
add_circle_marker <- function(map, lat, lng, city, label_options) {
    map |>
        leaflet::addCircleMarkers(
            lat = lat,
            lng = lng,
            radius = 5,
            color = "#4444AA",
            label = as.character(city),
            labelOptions = label_options
        )
}

# Boucle pour ajouter des marqueurs pour chaque ligne du dataframe
for (i in 1:nrow(emission_by_route_df)) {
    map <- add_circle_marker(map, emission_by_route_df$lat1[i], emission_by_route_df$lng1[i], emission_by_route_df$city1[i], custom_label_options)
    map <- add_circle_marker(map, emission_by_route_df$lat2[i], emission_by_route_df$lng2[i], emission_by_route_df$city2[i], custom_label_options)
}

