source("global.R")

shinyServer(function(input, output) {
   
  values <- reactiveValues(
    stations = 0,
    bikes = 0,
    company = NULL,
    name = NULL,
    pop = 0, 
    id = NULL,
    rownumber = 0,
    subsetrow = 0, 
    selectdata =  NULL,
    networkdata = NULL,
    maxorder = 0,
    idorder = NULL,
    delrow =0,
    website = NULL,
    finish = "No"
    
  )
  
  observe({
    
    if (is.null(values$networkdata)) {
      values$networkdata <-  network
    }
    
    if ( (values$subsetrow == 0 ) &  (!is.null(values$id)) ) {
      values$subsetrow <- detail_to_collect %>% subset(id == values$id ) %>% nrow()
    }
    
    if ( is.null(values$selectdata) &  (!is.null(values$id))  ) {
      values$selectdata <- detail_to_collect %>% subset(id == values$id )
    }
    
    if ( (values$maxorder == 0 ) & (!is.null(values$selectdata)) ) {
      values$maxorder <- nrow(values$selectdata)
    }
    
    if ( is.null(values$idorder)  & (!is.null(values$selectdata)) ) {
      temp <- detail_to_collect %>% subset(id == values$id )
      values$idorder <- temp$id_order
    }
    
    

  })
  
  defaultvalues <- reactiveValues(
    
    type = "ALL",
    continent = "ALL",
    worldcountry = "ALL",
    worldcity = "ALL",
    networkcontinent = "North America",
    selectcountry = "United States",
    selectcity = "Nashville, TN",
    allfinish = "No"
    
  )
  

  
  output$company <- renderUI({
    HTML(paste0("Company: ", values$company))
  })
  
  output$pname <- renderUI({
    HTML(paste(paste0("Program Name: ", values$name) , sep="<br/>"))
  })
  
  
  output$population <- renderUI({
    HTML(paste( paste0("Population: ", values$pop ) , sep="<br/>"))
  })
  
  output$website <- renderUI({
    HTML(paste(paste0("Website: ", ifelse(is.na(values$website),"None", values$website))
               , sep="<br/>"))
  })
  
  
  
  output$totalstations <- renderText({
    paste0("Total Stations: ", values$stations ) 
  })
  
  output$totalbikes <- renderText({
    paste0("Total Available Bicycles: ", values$bikes ) 
  })
  
  
  output$selectcity <- renderUI({
    
    tempcity <- sort(unique(values$networkdata$city[ (values$networkdata$fullname == input$selectcountry) & 
                                                       (values$networkdata$finish == input$allfinish)]))
    selectInput("selectcity","City",
                tempcity,
                selected = "Nashville, TN")
  })
  
  output$showpop <- renderUI({
    numericInput("showpop", 
                 label = h5("Population"),  
                 value = values$pop)
      })
  
  output$showweb <- renderUI({
    textInput("showweb", 
                 label = h5("Website"),  
                 value = values$website)
  })
  
  output$finish <- renderUI({
    selectInput("finish", 
              label = h5("Finish"),  
              c("Yes","No"),
              selected = values$finish)
  })
  
  
  

  output$ui_addselect <- renderUI({
    
    if (input$addtype == "Delete"){
      
      if (values$subsetrow == 0) {
        maxrow <- 0
      } else {
        maxrow <- values$idorder
      }
      
      selectInput("addselect", "id_order",
                  maxrow)
    }
    
  })
  
  output$ui_addupdate <- renderUI({
    if (input$addtype == "Delete"){
      actionButton("addupdate", "Update")
    }
  })
  
  output$ui_adddel <- renderUI({
    if (input$addtype == "Delete"){
      actionButton("adddel", "Delete")
    }
  })
  
  output$ui_addvariable <- renderUI({
    if (input$addtype == "Add"){
      
      selectInput("addvariable",
                  "Variable",
                  c("Membership", "Membership-Student"  ,"Membership Usage", 
                    "Out-of-Hub Fees", "Out-of-System Fee", "Out-of-Hub Return Credit",
                    "Non-membership Usage", "Panalty", "Deposit"  ,"Other") , 
                  selected = "Membership" )
          }
  })
  
  output$ui_addtime <- renderUI({
    if (input$addtype == "Add"){
      
      if ( input$addvariable %in% c("Membership","Membership-Student") ) {
        selectInput("addtime",
                    "Time",
                    c("1d", "2d", "3d", "4d", "5d", "6d", 
                      "7d/1w", "30d/1m", "3m", "6m", "1y", "none",
                      "Other") , 
                    selected = "1d" )
        
      } else if (input$addvariable %in% c("Membership Usage",  
                                          "Non-membership Usage", "Other" ) ) {
        selectInput("addtime",
                    "Time",
                    c("1st-30min", "1st-60min", "1st-90min", "1st-120min",
                      "Additional 30min","Additional 60min",
                      "Add 30min after 1st-30min", "Add 30min after 1st-60min", 
                      "Add 30min after 1st-90min", "Add 30min after 1st-120min",
                      "Every 60min", "Every 30min",
                      "Additional Minute", "Max per day",
                      "Other") , 
                    selected = "1st-hr" )
      } else if (input$addvariable == "Panalty") {
        selectInput("addtime",
                    "Time",
                    c("Bike Replacement", "Card replacement", "Key replacement", "Other") , 
                    selected = "Bike Replacement" )
      } else {
        selectInput("addtime",
                    "Time",
                    c("Other") , 
                    selected = "Other" )
      }
      
    }
    
  })
  
  
  output$ui_addcurrency <- renderUI({
    if (input$addtype == "Add"){
      selectInput("addcurrency",
                  "Currency",
                  unique(ISO_4217$Letter) , 
                  selected = "USD" )
    }
  })
  output$ui_addprice <- renderUI({
    if (input$addtype == "Add"){
      
      numericInput("addprice", 
                   label = h5("Price"), value=0)
    }
  })
  output$ui_addnote <- renderUI({
    if (input$addtype == "Add"){
      textInput("addnote", h5("Note"), value="None")
    }
  })
  output$ui_addnew <- renderUI({
    if (input$addtype == "Add"){
      actionButton("addnew", "Add")
    }
  })
  

  output$selectcountry <- renderUI({
    tempcountry <- sort(unique(values$networkdata$fullname[ values$networkdata$continent == input$networkcontinent]))
    selectInput("selectcountry",
                "Country",
                tempcountry,
                selected = "United States")
    
  })
  
  output$cityplot <- renderLeaflet({
  
    
    if ( !is.null(input$selectcity) ) {
     defaultvalues$selectcity <- input$selectcity
    } 
    
    #tempselectcity <- ifelse(is.null(input$selectcity), "Nashville, TN" ,input$selectcity)
    
    
    values$rownumber <- which( network$city == defaultvalues$selectcity)
    
    herf <- network$herf[ values$rownumber  ]
    values$company <- network$company[ values$rownumber ]
    values$pop <- network$population[values$rownumber  ]
    values$id <- network$id[ values$rownumber ]
    values$website <- network$website[values$rownumber]
    values$finish <- network$finish[values$rownumber]
    values$selectdata <- detail_to_collect %>% subset(id == values$id )
    
    url <- paste0("https://api.citybik.es",herf)
    dat <- content(GET(url))
    
    
    values$name <- dat$network$name
    
    stations <- dat$network$stations
    
    values$stations <- length(stations)
    
    sname <- unlist(  lapply(stations, "[[", "name")  )
    long <- unlist(  lapply(stations, "[[", "longitude")  )
    lat <- unlist(  lapply(stations, "[[", "latitude")  )
    freebike <- unlist(  lapply(stations, "[[", "free_bikes")  )
    empty_slots <- unlist(  lapply(stations, "[[", "empty_slots")  )
    
    values$bikes <- sum(freebike)
    
    df_city <- sp::SpatialPointsDataFrame(
      cbind(long,
            lat),
      data.frame(type="operation",
                 Description = paste0(sname, "<br>" ,
                                      "Bikes: ", freebike, "<br>",
                                      "Empty Slots: ", empty_slots, "<br>"))
    )
    
    leaflet(df_city) %>% addTiles() %>%
      addMarkers(icon = ~bikeIcons[type], popup = ~Description)
    
    
    })
  
  
  
   observeEvent(input$updatepop, {

     colpop <- which(colnames(network) == "population")
     colweb <- which(colnames(network) == "website")
     colfin <- which(colnames(network) == "finish")
     
     values$pop <- input$showpop
     values$website <- input$showweb
     values$finish <- input$finish
     
      ss <- gs_edit_cells(ss,  ws = "livemap", input = values$pop,  
                           anchor = paste0("R", values$rownumber + 1,"C", colpop))
      
      ss <- gs_edit_cells(ss,  ws = "livemap", input = values$website,  
                          anchor = paste0("R", values$rownumber + 1,"C", colweb))
      
      ss <- gs_edit_cells(ss,  ws = "livemap", input = values$finish,  
                          anchor = paste0("R", values$rownumber + 1,"C", colfin))
      
      values$networkdata <- gs_read(ss,  ws = "livemap")
      
    
  })
   
   
   observeEvent(input$addnew, {
     
     inputdata <- c(values$id, values$maxorder+1  , input$addvariable,
                    input$addtime, input$addcurrency,
                    input$addprice, input$addnote)
     
     ss <- gs_add_row(ss,  ws = "detail", input = inputdata)
     temp <- gs_read(ss,  ws = "detail")
     values$selectdata <- temp %>% subset(id == values$id )
     values$subsetrow <-  nrow(temp %>% subset(id == values$id ))
     values$maxorder <- values$maxorder+1
     values$idorder <- values$selectdata$id_order
     #ss <- gs_edit_cells(ss,  ws = "detail", input = 1:values$maxorder,  
    #                     anchor = paste0("B", values$rownumber + 1,"C", colpop))
     
     
    
     })
   

   
   observeEvent(input$adddel, {
     
     if (values$subsetrow > 0){
       
       temp <- gs_read(ss,  ws = "detail" )
       values$selectdata <- temp %>% subset(id == values$id )
       values$delrow <- which( (temp$id == values$id) & (temp$id_order == input$addselect)  )
       
       ss <- gs_edit_cells(ss,  ws = "detail", input = rep(NA,7),  
                           anchor = paste0( "A", values$delrow+1), byrow=TRUE)
       
       temp <- gs_read(ss,  ws = "detail")
       values$selectdata <- temp %>% subset(id == values$id )
       values$subsetrow <-  nrow(temp %>% subset(id == values$id ))
       values$idorder <- values$selectdata$id_order
       
     }
     
   })
   
   
   output$citydetail <- renderDataTable({
    
     datatable(values$selectdata)
     
     
   })
   
  
  
})


