#ui.R

fluidPage(theme = "custom.css",
          tags$title("Ettevõtted: mina vs sarnased - Annegrete Molloka"),
          titlePanel = "Ettevõtted: mina vs sarnased",
          sidebarLayout(
            sidebarPanel(width = 3, 
                         useShinyjs(),
                         uiOutput("side")),
            mainPanel(uiOutput("page"))
          ))