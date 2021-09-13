library(glue)
library(maps)
library(DT)
library(plotly)
library(ggpubr)
library(tidyverse)
library(leaflet)
library(tibble)
library(shiny)
library(shiny.router)
library(shiny.semantic)
library(geosphere)

source("module_vessel.R")

# Info Page Loader
info_page <- div(div(class = "ui stackable grid container",
                     div(
                       class = "sixteen wide column",
                       div(class = "ui center aligned big header", "Shiny Vessel App"),
                       p(
                         "This app is created for demonstrating skills for the second step of Shiny Developer job application at ",
                         a("Appsilon. ", href = "https://appsilon.com/"),
                         "This dashboard is developed by ",
                         a("Burak Can Koc", href = "https://twitter.com/burakcankoc"),
                         "and data has been provideed by ",
                         a("Appsilon", href = "https://appsilon.com/"),
                         "."
                       ),
                       div(
                         class = "ui center aligned",
                         style = "text-align: center;",
                         action_button("go_modal", "Contact Burak Can Koc", class = "steelblue"),
                         br()
                       )
                     )))

# Router between tabs
router <- make_router(route("index", info_page),
                      route("vessel", uiVessel("p2")))


# Default Server
server <- function(input, output, session) {
  # router pages
  router$server(input, output, session) #router(input, output) #
  vesselServer("p2")
  
  # modal
  observeEvent(input$go_modal, {
    create_modal(
      modal(
        id = "simple-modal",
        title = "Info about Marine Vessel App shiny.semantic app",
        content = list(
          style = "background: orange",
          `data-custom` = "value",
          p(
            "The data for this app has been provided by Appsilon, courtesy of Paulina Kaczmarczyk.",
            "The design idea has been heavily inspired by ",
            a("Fifa '19 Demo App", href = "https://demo.appsilon.com/apps/fifa19/#!/index"),
            "."
          )
        ),
        p(
          tags$b(
            "This app was created by Burak Can Koc for Appsilon's Shiny Developer job application."
          )
        )
      )
    )
  })
}

# Default UI
ui <- semanticPage(
  title = "Marine Vessel App",
  tags$head(
    tags$link(rel = "stylesheet", href = "style.css", type = "text/css")
  ),
  horizontal_menu(list(
    list(
      name = "Info",
      link = route_link("index"),
      icon = "world"
    ),
    list(
      name = "Vessel App",
      link = route_link("vessel"),
      icon = "globe europe"
    )
  ), logo = "https://i0.wp.com/blog.rstudio.com/2020/07/21/4-tips-to-make-your-shiny-dashboard-faster/appsilon-logo.png"),
  router$ui,
  tags$footer(
    "Created by Burak Can Koc for Appsilon, September 2021",
    align = "center",
    style = "
                        position:fixed;
                        bottom:0;
                        right:0;
                        left:0;
                        background:black;
                        color: white;
                        padding:10px;
                        box-sizing:border-box;
                        z-index: 1000;
                        text-align: center"
  )
)
# Run App
shinyApp(ui, server)