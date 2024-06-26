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
    csv_gare <- get_data(path = "data/gares_fr.csv")
} else {
    STATIONS_DATA_URL <- "https://www.data.gouv.fr/fr/datasets/r/d22ba593-90a4-4725-977c-095d1f654d28"
    csv_gare <- read.csv2(STATIONS_DATA_URL)
    write_data(data = csv_gare, path = "data/gares_fr.csv")
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
    time_matrix <- get_data(path = "data/time_matrix.csv", check.names = FALSE) |> as.matrix()
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
cat(sprintf("En 2019, environ %.2f tCO2éq aurait pu être évités", total_PKT * GCO2_PER_PKT / 1000000))


