################################################################################
#######                          API parameters                          #######
################################################################################

## Paramètres de l'API ---------------------------------------------------------

secrets <- yaml::read_yaml("./secrets.yaml")
X_API_ID <- secrets$travelTime$X_API_ID
X_API_KEY <- secrets$travelTime$X_API_KEY

ROUTES_API_URL <- "https://api.traveltimeapp.com/v4/routes"

secrets_headers <- httr::add_headers(
    "Content-Type" = "application/json",
    "X-Application-Id" = X_API_ID,
    "X-Api-Key" = X_API_KEY
)
