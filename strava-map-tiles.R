library(rStrava)
library(googlePolylines)

app_name <- '<App name>' # chosen by user
app_client_id  <- '<App client ID>' # an integer, assigned by Strava
app_secret <- '<App secret>' # an alphanumeric secret, assigned by Strava

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))

# access Strava API to retrieve activity list
act.list <- get_activity_list(stoken)
act.df <- compile_activities(act.list)

# Funktion 'plot.one()' plots one small map, 'pl' is a Polyline
plot.one <- function (pl, ...) {
  line.df <- decode(pl)[[1]]
  plot(line.df$lon, line.df$lat, type = "l",
       xaxt = "n", yaxt = "n", xlab = "", ylab = "",
       bty = "n", ...)
}

# Only use runs
act.df <- act.df[act.df$type == "Run",]

# Assign color according to distance
act.df$pace.col <- leaflet::colorNumeric(
  colorRampPalette(c("hotpink", "yellow", "green"))(100),
  domain = act.df$distance)(act.df$distance)

png("~/Desktop/maps.png", width = 3072, height = 1920)
# If you have more/fewer activities you want to plot, adapt the layout.
# Currently, it has 240 'slots'
layout(matrix(1:240, ncol = 240/10))
for (i in 1:nrow(act.df)) {
  par(mar = rep(0,4), bg = "black")
  col.i <- act.df[i, "pace.col"]
  plot.one(act.df[i,]$map.summary_polyline, col = col.i)
}
dev.off()

