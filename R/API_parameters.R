################################################################################
#######                          API parameters                          #######
################################################################################


secrets <- yaml::read_yaml("./secrets.yaml")


## Paramètres de l'API TravelTime ----------------------------------------------

X_API_ID <- secrets$travelTime$X_API_ID
X_API_KEY <- secrets$travelTime$X_API_KEY

ROUTES_API_URL <- "https://api.traveltimeapp.com/v4/routes"

secrets_headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "X-Application-Id" = X_API_ID,
    "X-Api-Key" = X_API_KEY
)


## Paramètres de l'API Stadia ----------------------------------------------

STADIA_MAPS_API_KEY <- secrets$stadiaMaps$API_KEY

STYLE <- "outdoors"

TILES_URL <- sprintf(
    "https://tiles.stadiamaps.com/tiles/%s/{z}/{x}/{y}{r}.png?api_key=%s",
    STYLE, STADIA_MAPS_API_KEY
)
