source("global.R")

# Define UI for application that draws a histogram
shinyUI(navbarPage("The Bike-Sharing World Map",
  
  
  tabPanel("Live City Map", 
           
           titlePanel("Live City Map"),
           
           hr(),
           
           fluidRow(
             
             column(3,
                    
                    selectInput("networkcontinent",
                                "Continent",
                                unique(network$continent) , 
                                selected = "North America" ),
                    
                    uiOutput("selectcountry"),
                    
                    
                    
                    uiOutput("selectcity"),
                    
                    selectInput("allfinish",
                                "Finish",
                                c("Yes","No"), 
                                selected = "No" ),
                    
                    
                    
                    hr(),
                    textOutput("totalstations"),
                    textOutput("totalbikes"),
                    
                    hr(),
                    htmlOutput("company"),
                    htmlOutput("pname"),
                    
                    hr(),
                    htmlOutput("population"),
                    htmlOutput("website")
                    

             ), 
             
             # Show a plot of the generated distribution
             column(9,
                    leafletOutput("cityplot"),

                    tags$div(class="header", checked=NA,
                             tags$a(href="https://www.iconfinder.com/", "Icon Source: IconFinder")
                    ),
                    tags$div(class="header", checked=NA,
                             tags$a(href="https://api.citybik.es/v2/",
                                    "Data Source: CityBikes API")
                    ),
                    tags$div(class="header", checked=NA,
                             tags$a(href="http://www.MinchunZhou.com", "By Minchun Zhou")
                    )
             )
           ),
           hr(),
           
           column(4,
                  uiOutput("showpop")
           ), 
           
           column(4,
                  uiOutput("showweb")
           ),
           column(4,
                  uiOutput("finish")
           ),
           
           column(12,                  
                  actionButton("updatepop", "Update"),
                  hr()
                  
                  ),
           
           
           
           column(3, 

                  selectInput("addtype",
                              "Edit type",
                              c("Add", "Delete") , 
                              selected = "Add" ),
                  

                  uiOutput("ui_addselect"),
                  
                  uiOutput("ui_adddel"),

                  uiOutput("ui_addvariable"),
                  uiOutput("ui_addtime"),
                  uiOutput("ui_addcurrency"),
                  uiOutput("ui_addprice"),
                  uiOutput("ui_addnote"),
                  uiOutput("ui_addnew")
                  
           ),
                  
           
           column(9, 
                  DT::dataTableOutput("citydetail")
           )
           
           
           
           
           
           
           
  )
))

# icon : https://www.iconfinder.com/

#https://www.google.com/maps/d/u/0/viewer?mid=1UxYw9YrwT_R3SGsktJU3D-2GpMU&hl=en&ll=-3.81666561775622e-14%2C164.20445059999997&z=1
