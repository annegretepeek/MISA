# ui.R

fluidPage(
  theme = "custom.css",
  tags$title("Ettevõtted: mina vs sarnased"),
  useShinyjs(),
  titlePanel("Ettevõtted: mina vs sarnased"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("company_id", "Vali ettevõte:", choices = NULL),
      uiOutput("similarity_controls")
    ),
    mainPanel(
      uiOutput("main_content")
    )
  )
)
