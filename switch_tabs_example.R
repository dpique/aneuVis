#https://stackoverflow.com/questions/32971921/navigate-to-particular-sidebar-menu-item-in-shinydashboard/32972517#32972517
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard")),
                menuItem("Menu Item 1", tabName = "two", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "one",h2("Dashboard tab content"),actionButton('switchtab', 'Switch tab')),
      tabItem(tabName = "two",h2("Widgets tab content"))
    )
  )
)
  
#https://stackoverflow.com/questions/43552906/how-to-switch-between-navbar-tabs-with-a-button-r-shiny
server <- function(input, output, session) {
  observeEvent(input$switchtab, {
    newtab <- switch(input$tabs, "one" = "two","two" = "one")
    updateTabItems(session, "tabs", newtab)
  })
}

shinyApp(ui, server)