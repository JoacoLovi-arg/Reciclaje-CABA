port <- as.numeric(Sys.getenv("PORT", 3838))

options(shiny.host = "0.0.0.0", shiny.port = port)

cat("Arrancando Shiny en 0.0.0.0:", port, "\n")

shiny::runApp(appDir = "/srv/shiny-server", host = "0.0.0.0", port = port)