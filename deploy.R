# Deploy the clinAnnotR Shiny app to shinyapps.io.
# Run this script from the package root directory.
#
# First-time setup:
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name="<account>", token="<token>", secret="<secret>")

app_files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  list.files("R",            full.names = TRUE, recursive = FALSE),
  list.files("inst/shiny",   full.names = TRUE, recursive = TRUE),
  list.files("inst/extdata", full.names = TRUE, recursive = FALSE)
)

rsconnect::deployApp(
  appDir        = ".",
  appFiles      = app_files,
  appPrimaryDoc = "inst/shiny/clinannotr/app.R",
  appName       = "clinannotr"
)
