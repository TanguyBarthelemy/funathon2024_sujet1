################################################################################
#######                   Some utilities around the API                  #######
################################################################################


## Récupérer la position GPS ---------------------------------------------------

get_station_coordinates <- function(station_name, data_stations = csv_stations, verbose = TRUE) {
    if (station_name == "Strasbourg-Ville") {
        coords <- c(lat = 48.584488, long = 7.735626)
    } else {
        coords <- data_stations |>
            subset(libelle == station_name) |>
            base::`[`(1L, ) |>
            subset(select = "c_geo", drop = TRUE) |>
            strsplit(",") |>
            unlist() |>
            as.numeric() |>
            setNames(c("lat", "lng"))
    }

    # Si verbose est TRUE, on affiche les coordonnées
    if (verbose) {
        cat(sprintf("%s -> (%f, %f)\n", station_name, coords[1], coords[2]))
    }

    return(coords)
}

## Fonction autour de l'API ----------------------------------------------------

get_travel_time_api_response <- function(endpoint = ROUTES_API_URL, request, headers = secrets_headers) {

    response <- httr::POST(
        url = endpoint,
        body = request,
        encode = "json",
        headers
    )

    if (!httr::http_error(response)) {
        message("La requête a bien été traitée")
        return(list(
            "Content" = httr::content(response, as = "parsed"),
            "Status_code" = httr::status_code(response)
        ))
    } else {
        # On affiche une message d'avertissement lorsque la requête n'a rien renvoyé
        warning("Failed to retrieve data: ", httr::http_status(response)$message)
        return(list(
            "Content" = NA,
            "Status_code" = httr::status_code(response)
        ))
    }
}

#' Construit une requête à envoyer à l'API
#'
#' @param coord_from des coordonnées GPS sous la forme d'un vecteur numeric de taille 2 (lattitude et longitude)
#' @param coord_to  des coordonnées GPS sous la forme d'un vecteur numeric de taille 2 (lattitude et longitude)
#'
#' @return une chaine de caractère avec la requête à envoyer à l'API
#'
construct_request <- function(coord_from, coord_to) {
    request_body <- paste0(
        '{
  "locations": [
    {
      "id": "point-from",
      "coords": {
        "lat": ', coord_from[1L], ',
        "lng": ', coord_from[2L], '
      }
    },
    {
      "id": "point-to-1",
      "coords": {
        "lat": ', coord_to[1L], ',
        "lng": ', coord_to[2L], '
      }
    }
  ],
  "departure_searches": [
    {
      "id": "departure-search",
      "transportation": {
        "type": "public_transport"
      },
      "departure_location_id": "point-from",
      "arrival_location_ids": [
        "point-to-1"
      ],
      "departure_time": "2024-06-25T07:00:00.000Z",
      "properties": [
        "travel_time",
        "route"
      ],
      "range": {
        "enabled": true,
        "max_results": 5,
        "width": 900
      }
    }
  ]
}'
    )

    return(request_body)
}

get_min_travel_time <- function(response) {
    if (length(response$Content$results[[1L]]$locations) == 0L) {
        warning("Aucune route ne permet de rejoindre les 2 villes")
        return(Inf)
    }
    return(
        sapply(
            X = response$Content$results[[1L]]$locations[[1L]]$properties,
            FUN = base::`[[`,
            "travel_time"
        ) |>
            min() |>
            base::`/`(3600)
    )
}

get_travel_time_from_coords <- function(coord_from, coord_to,
                                        headers = secrets_headers,
                                        endpoint = ROUTES_API_URL) {
    return(
        construct_request(coord_from, coord_to) |>
            get_travel_time_api_response(endpoint = endpoint,
                                         headers = headers) |>
            get_min_travel_time()
    )
}

get_travel_time_between_stations <- function(station_from, station_to,
                                             data_stations = csv_stations,
                                             verbose = TRUE) {

    # Si les stations sont identiques aucun trajet nécessaire
    if (station_from == station_to) {
        return(NA)
    }

    # Récupérer les coordonnées pour les deux stations
    coordinates <- lapply(
        X = c(station_from, station_to),
        FUN = get_station_coordinates,
        data_stations = data_stations,
        verbose = FALSE
    )

    # Générer le JSON pour l'API de routage
    request_body <- construct_request(coord_from = coordinates[[1]],
                                      coord_to = coordinates[[2]])

    # Interroger l'API de routage
    response <- get_travel_time_api_response(ROUTES_API_URL, request_body)

    # Gérer la limitation du taux d'API
    if (response[[2]] == 429) {
        if (verbose) cat("Trop de requêtes, attente d'une minute...\n")
        Sys.sleep(60)
        return(get_travel_time_between_stations(station_from, station_to,
                                                data_stations, verbose))
    }

    # Vérifier l'existence d'un itinéraire valide
    if (length(response[[1]]$results[[1]]$locations) == 0) {
        travel_time <- Inf
    } else {
        # Extraire les données de temps de trajet et trouver le temps de trajet minimum en heures
        travel_times <- sapply(
            X = response[[1]]$results[[1]]$locations[[1]]$properties,
            FUN = function(item) item$travel_time
        )
        travel_time <- min(travel_times) / 3600
    }

    # Afficher le temps de trajet si verbose
    if (verbose) {
        message_text <- sprintf(
            "%s -> %s : %s\n",
            station_from, station_to,
            ifelse(test = is.infinite(travel_time),
                   yes = "Aucun itinéraire trouvé",
                   no = paste0(round(travel_time, 2), " heures"))
        )
        cat(message_text)
    }

    return(travel_time)
}

# Renvoie le trafic total d’une liaison en 2019 en PKT
# (produit du nombre de passagers et de la distance parcourue)
# à partir des deux noms de villes (et non d’aéroport) en paramètre.

compute_PKT <- function(city_1, city_2,
                        data_airports = air_traffic_df,
                        verbose = TRUE) {
    PKT <- data_airports |>
        subset((grepl(pattern = city_1, x = LSN_DEP_NOM, ignore.case = TRUE)
                & grepl(pattern = city_2, x = LSN_ARR_NOM, ignore.case = TRUE))
               | (grepl(pattern = city_2, x = LSN_DEP_NOM, ignore.case = TRUE)
                  & grepl(pattern = city_1, x = LSN_ARR_NOM, ignore.case = TRUE))) |>
        transform(PKT = LSN_PAX_loc * LSN_DIST) |>
        base::`[[`("PKT") |>
        sum()

    # Afficher le temps de trajet si verbose
    if (verbose) {
        message_text <- sprintf(
            "%s <-> %s : %s\n",
            city_1, city_2,
            ifelse(test = is.infinite(PKT),
                   yes = "Aucun itinéraire trouvé",
                   no = paste0(round(PKT, 2), " PKT"))
        )
        cat(message_text)
    }

    return(PKT)
}

# Fonction pour extraire les noms des villes à partir des noms des gares
extract_city_name <- function(station_name) {
    return(sapply(station_name, function(x) strsplit(x, "-")[[1]][1]))
}

get_tile <- function(x, y, zoom, url = TILES_URL) {

    url <- url |>
        gsub(pattern = "{x}", replacement = x, fixed = TRUE) |>
        gsub(pattern = "{y}", replacement = y, fixed = TRUE) |>
        gsub(pattern = "{z}", replacement = zoom, fixed = TRUE) |>
        gsub(pattern = "{r}", replacement = "@2x", fixed = TRUE)

    return(url)
}


httr::GET(
    url = get_tile(1, 1, zoom = 7)
)
