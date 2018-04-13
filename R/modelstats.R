#' Analyze Model Statistics
#'
#' Shiny app to analyze statistics collected with \code{\link{runstatistics}}
#' and merged with \code{\link{mergestatistics}}
#'
#' @param files path to rds-files from which statistics should be read
#' @author Jan Philipp Dietrich
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel selectInput shinyApp renderPlot mainPanel plotOutput column actionButton reactive removeUI
#' reactiveValues observeEvent insertUI tags fluidRow sliderInput titlePanel radioButtons textOutput renderText updateSelectInput
#' @importFrom data.table uniqueN
#' @export


modelstats <- function(files=c("https://www.pik-potsdam.de/rd3mod/magpie.rds","https://www.pik-potsdam.de/rd3mod/remind.rds")) {
  
  names(files) <- basename(files)
  
  ui <- fluidPage(
    tags$div(id="title", titlePanel(paste0("Model run statistics from ",basename(files[1])))),
    tags$div(id="titleend"),
    sidebarLayout(
      sidebarPanel(radioButtons(inputId = "file",
                                label = "Choose input", selected = NULL,
                                inline = TRUE, choices = files),
                   tags$div(id="navigation",
                            selectInput(inputId = "xaxis",
                                        label = "Choose X-Axis",
                                        choices = "revision_date"),
                            selectInput(inputId = "yaxis",
                                        label = "Choose Y-Axis",
                                        choices = "runtime"),
                            selectInput(inputId = "color",
                                        label = "Choose Colorkey",
                                        choices = "revision"),
                            tags$p(),tags$hr(),tags$p(),
                            selectInput(inputId = "filter",
                                        label = "Choose a filter",
                                        choices = "user")),
                   tags$div(id="sidebarend"),
                   tags$hr(),
                   textOutput("observations")),
      mainPanel(plotOutput("stats"))
    )
  )
  
  server <- function(input, output, session) {
    readdata <- function(file) {
      if(grepl("https://",file)) {
        out <- readRDS(gzcon(url(file)))
      } else {
        out <- readRDS(file)
      }
      if(!is.null(out$date)) out$date <- as.POSIXct(out$date, origin="1970-01-01")
      if(!is.null(out$revision_date)) out$revision_date <- as.POSIXct(out$revision_date, origin="1970-01-01")
      return(out)
    }
    
    x <- reactiveValues(data=NULL, selection=NULL, filter=NULL, filterclass=NULL, activefilter=NULL)
    
    selectdata <- function(data,input,filter){
      choices <- list()
      fvec <- rep(TRUE,dim(data)[1])
      for(f in filter) {
        slf <- paste0("slider",f)
        if(!is.null(input[[slf]])) {
          #updateSliderInput(session,slf,   min = min(data[[f]][fvec], na.rm=TRUE),
          #                max = max(data[[f]][fvec], na.rm=TRUE),
          #                value = c(min(data[[f]][fvec], na.rm=TRUE),max(data[[f]][fvec], na.rm=TRUE)))
          
          fvec <- (fvec & (data[[f]] >= input[[slf]][1]) & (data[[f]] <= input[[slf]][2]))
        } else {
          sf <- paste0("select",f)
          if(!is.null(input[[sf]])) {
            #updateSelectInput(session, sf,  choices=data[[f]][fvec], selected=input[[sf]])
            fvec <- (fvec & (data[[f]] %in% input[[sf]]))
          }
        }
      }
      fvec[is.na(fvec)] <- FALSE
      return(data[fvec,])
    }
    
    selection <- reactive(selectdata(x$data,input,x$activefilter))
    
    observeEvent(input$file, {
      x$data <- readdata(input$file)
      nelem <- apply(x$data,2,uniqueN)
      x$filter <- grep(".id",names(x$data)[nelem>1], fixed=TRUE, value=TRUE, invert=TRUE)
      x$filterclass <- sapply(x$data,function(x)return(class(x)[1]))
      for(f in x$activefilter) {
        removeUI(
          selector = escapeRegex(paste0("#div",f))
        )
      }
      x$activefilter <- NULL
      removeUI(selector = "#title")
      insertUI(
        selector = "#titleend",
        where = "beforeBegin",
        ui = tags$div(id="title", titlePanel(paste0("Model run statistics from ",basename(input$file))))
      )
      updateSelectInput(session, "xaxis",  choices=x$filter, selected = "date")
      updateSelectInput(session, "yaxis",  choices=x$filter, selected = "user")
      updateSelectInput(session, "color",  choices=x$filter, selected = "user")
      updateSelectInput(session, "filter", choices=x$filter, selected = "user")
    })
    
    output$stats <- renderPlot({
      selection <- selection()
      output$observations <- renderText(paste0(dim(selection)[1]," observations"))
      cset <- function(i,check) {
        if(i %in% check) return(i)
        return(check[1])
      }
      # fvec is the filter vector to be applied on
      # out to select the chosen entries
      if (!requireNamespace("mip", quietly = TRUE)) {
        theme <- NULL
      } else {
        theme <- mip::theme_mip(size=14)
      }
      
      ggplot2::ggplot(selection) + ggplot2::theme(legend.direction="vertical") +
        ggplot2::geom_point(ggplot2::aes_string(y=cset(input$yaxis,x$filter),
                                                x=cset(input$xaxis,x$filter),
                                                color=cset(input$color,x$filter)),size=5, na.rm=TRUE) +
        theme
    }, height=700)
    
    selectUI <- function(filter, data, class) {
      if(class=="POSIXct") {
        return(tags$div(id=paste0("div",filter),
                        sliderInput(inputId = paste0("slider", filter),
                                    label = filter,
                                    min = min(data[[filter]], na.rm=TRUE),
                                    max = max(data[[filter]], na.rm=TRUE),
                                    value = c(min(data[[filter]], na.rm=TRUE),max(data[[filter]], na.rm=TRUE)),
                                    ticks = FALSE,
                                    step=60,
                                    timezone="CET",
                                    timeFormat = "%F %H:%M")))
      } else if(class=="numeric") {
        return(tags$div(id=paste0("div",filter),
                        sliderInput(inputId = paste0("slider", filter),
                                    label = filter,
                                    min = floor(min(data[[filter]], na.rm=TRUE)),
                                    max = ceiling(max(data[[filter]], na.rm=TRUE)),
                                    value = c(min(data[[filter]], na.rm=TRUE),max(data[[filter]], na.rm=TRUE)),
                                    ticks = FALSE)))        
      } else {
        return(tags$div(id=paste0("div",filter),
                        selectInput(inputId = paste0("select", filter),
                                    label = filter,
                                    choices = unique(data[[filter]]),
                                    multiple = TRUE, "display:inline")))
      }
    }
    
    observeEvent(c(input$filter,input$file), {
      if(!(input$filter %in% x$activefilter)){
        insertUI(
          selector = "#sidebarend",
          where = "beforeBegin",
          ui = selectUI(input$filter, selection(), x$filterclass[input$filter])
        )
        x$activefilter <- c(x$activefilter,input$filter)
      }
      for(f in setdiff(x$activefilter, input$filter)) {
        if(!is.null(input[[paste0("slider",f)]])) {
          removeUI <- ((input[[paste0("slider",f)]][1] <= min(x$data[[f]], na.rm=TRUE)) &
                         (input[[paste0("slider",f)]][2] >= max(x$data[[f]], na.rm=TRUE)))
        } else {
          removeUI <- ifelse(is.null(input[[paste0("select",f)]]), TRUE, FALSE)
        }
        if(removeUI) {
          removeUI(
            selector = escapeRegex(paste0("#div",f))
          )
          x$activefilter <- setdiff(x$activefilter,f)
        }
      }
    })
  }
  
  shinyApp(ui=ui, server=server)
}