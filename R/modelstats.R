#' Analyze Model Statistics
#' 
#' Shiny app to analyze statistics collected with \code{\link{runstatistics}}
#' and merged with \code{\link{mergestatistics}}
#' 
#' @param files path to rds-files from which statistics should be read
#' @author Jan Philipp Dietrich
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel selectInput shinyApp renderPlot mainPanel plotOutput column actionButton reactive removeUI
#' reactiveValues observeEvent insertUI tags fluidRow sliderInput titlePanel radioButtons textOutput renderText
#' @importFrom data.table uniqueN
#' @importFrom mip theme_mip
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
                                        choices = "revision_date",
                                        selected = "revision_date"),
                            selectInput(inputId = "yaxis", 
                                        label = "Choose Y-Axis",
                                        choices = "runtime",
                                        selected = "runtime"),
                            selectInput(inputId = "color", 
                                        label = "Choose Colorkey",
                                        choices = "revision",
                                        selected = "revision")),
                   tags$div(id="sidebarend"),
                   tags$hr(),
                   textOutput("observations")),
      mainPanel(plotOutput("stats"))
    )
  )
  
  server <- function(input, output) {

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
    
    i <- reactiveValues(out=NULL, filter=NULL, filterclass=NULL)
    filter <- reactiveValues(active=NULL)
    
    observeEvent(input$file, {
      i$out <- readdata(input$file)
      nelem <- apply(i$out,2,uniqueN)
      i$filter <- grep(".id",names(i$out)[nelem>1], fixed=TRUE, value=TRUE, invert=TRUE)
      i$filterclass <- sapply(i$out,function(x)return(class(x)[1]))
      removeUI(
        selector = "#navigation"
      )
      for(f in filter$active) {
        removeUI(
          selector = escapeRegex(paste0("#div",f))
        ) 
      }
      filter$active <- NULL
      removeUI(selector = "#title") 
      insertUI(
        selector = "#titleend",
        where = "beforeBegin",
        ui = tags$div(id="title", titlePanel(paste0("Model run statistics from ",basename(input$file))))
      )
      insertUI(
        selector = "#sidebarend",
        where = "beforeBegin",
        ui = tags$div(id="navigation",
                selectInput(inputId = "xaxis", 
                         label = "Choose X-Axis",
                         choices = i$filter,
                         selected = "revision_date"),
                selectInput(inputId = "yaxis", 
                    label = "Choose Y-Axis",
                    choices = i$filter,
                    selected = "runtime"),
                selectInput(inputId = "color", 
                    label = "Choose Colorkey",
                    choices = i$filter,
                    selected = "revision"),
                tags$p(),tags$hr(),tags$p(),
                selectInput(inputId = "filter", 
                    label = "Choose a filter",
                    choices = i$filter))
      )
      
    })
    
    output$stats <- renderPlot({
      fvec <- rep(TRUE,dim(i$out)[1])
      for(f in filter$active) {
        slf <- paste0("slider",f)
        if(!is.null(input[[slf]])) {
          fvec <- (fvec & (i$out[[f]] >= input[[slf]][1]) & (i$out[[f]] <= input[[slf]][2]))
        } else {
          sf <- paste0("select",f)
          if(!is.null(input[[sf]])) {
            fvec <- (fvec & (i$out[[f]] %in% input[[sf]]))
          }
        }
      }
      fvec[is.na(fvec)] <- FALSE
      output$observations <- renderText(paste0(sum(fvec)," observations"))
      cset <- function(i,check) {
        if(i %in% check) return(i)
        return(check[1])
      }
      # fvec is the filter vector to be applied on
      # out to select the chosen entries
      ggplot2::ggplot(i$out[fvec,]) + ggplot2::theme(legend.direction="vertical") +
        ggplot2::geom_point(ggplot2::aes_string(y=cset(input$yaxis,i$filter),
                                                x=cset(input$xaxis,i$filter),
                                                color=cset(input$color,i$filter)),size=5) +
        theme_mip(size=14)
    }, height=700)

    observeEvent(input$filter, {
      if(!(input$filter %in% filter$active)){
        selectUI <- function(filter, out, class) {
          if(class=="POSIXct") {
            return(tags$div(id=paste0("div",filter),
                            sliderInput(inputId = paste0("slider", filter), 
                                        label = filter, 
                                        min = min(out[[filter]], na.rm=TRUE), 
                                        max = max(out[[filter]], na.rm=TRUE), 
                                        value = c(min(out[[filter]], na.rm=TRUE),max(out[[filter]], na.rm=TRUE)), 
                                        ticks = FALSE, 
                                        step=60, 
                                        timezone="CET",
                                        timeFormat = "%F %H:%M")))
          } else if(class=="numeric") {
            return(tags$div(id=paste0("div",filter),
                            sliderInput(inputId = paste0("slider", filter), 
                                        label = filter, 
                                        min = floor(min(out[[filter]], na.rm=TRUE)), 
                                        max = ceiling(max(out[[filter]], na.rm=TRUE)), 
                                        value = c(min(out[[filter]], na.rm=TRUE),max(out[[filter]], na.rm=TRUE)), 
                                        ticks = FALSE)))          
          } else {
            return(tags$div(id=paste0("div",filter),
                            selectInput(inputId = paste0("select", filter),
                                        label = filter,
                                        choices = unique(out[[filter]]),
                                        multiple = TRUE, "display:inline")))
          }
        }
        
        insertUI(
          selector = "#sidebarend",
          where = "beforeBegin",
          ui = selectUI(input$filter, i$out, i$filterclass[input$filter])
        )
        filter$active <- c(filter$active,input$filter)
      }
      for(f in setdiff(filter$active, input$filter)) {
        if(!is.null(input[[paste0("slider",f)]])) {
          removeUI <- ((input[[paste0("slider",f)]][1] <= min(i$out[[f]], na.rm=TRUE)) & 
                         (input[[paste0("slider",f)]][2] >= max(i$out[[f]], na.rm=TRUE)))
        } else {
          removeUI <- ifelse(is.null(input[[paste0("select",f)]]), TRUE, FALSE) 
        }
        if(removeUI) {
          removeUI(
            selector = escapeRegex(paste0("#div",f))
          )
          filter$active <- setdiff(filter$active,f)
        }
      }
    })
  }
  
  shinyApp(ui=ui, server=server)
}