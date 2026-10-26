On 10/26/2026 re-deployed (published) the SHINY app.  I did this by making a new conda environment on 
eddie called shiny_clean and then running the following code:

> rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "app.R",
  appName = "cvir_pmrs23",
  account = "jlmatt"
)

Note I exported a .yml and a .txt of this environment in the shiny directory.  