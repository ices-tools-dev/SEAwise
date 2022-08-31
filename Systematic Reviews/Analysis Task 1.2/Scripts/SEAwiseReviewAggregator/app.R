#
##
### Shiny App to explore, filter and download records and data from all SEAwise Reviews
##
#

#===
# Libraries ----
#===
library(shiny)
library(DT)
library(bslib)
library(ggplot2)
library(plotly)
library(ggthemes)
library(tidyquant)
library(lubridate)

#====

#===
# Data ----
#===
# wp2 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_2_20220829.csv", header = T) # Path when run manually
wp2 <- read.csv(file = "../../Databases/Database_2_20220829.csv", header = T)
wp2$Person <- NULL
wp2$Exclusion.Criteria <- NULL
wp2$CS...non.CS <- NULL
wp2$Region.CS.and.non.CS <- NULL
wp2 <- wp2[wp2$SW.ID != "", ]

# wp3 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_3_20220829.csv", header = T) # Path when run manually
wp3 <- read.csv(file = "../../Databases/Database_3_20220829.csv", header = T)
wp3$X <- NULL
wp3$Exclusion.Criteria <- NULL

# wp4 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_4_20220829.csv", header = T) # Path when run manually
wp4 <- read.csv(file = "../../Databases/Database_4_20220829.csv", header = T)
wp4$X <- NULL
wp4$Exclusion.Criteria <- NULL
wp4o <- wp4
wp4$Sampling.Method.used.for.data.collection <- ifelse(wp4$Sampling.Method.used.for.data.collection == "other", wp4$Description.Other.Sampling.Method, wp4$Sampling.Method.used.for.data.collection)
wp4$Description.Other.Sampling.Method <- NULL

# wp5 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_5_20220829.csv", header = T) # Path when run manually
wp5 <- read.csv(file = "../../Databases/Database_5_20220829.csv", header = T)
wp5$Name <- NULL
wp5 <- wp5[wp5$Exclusion.Criteria == "K",]
wp5$Exclusion.Criteria <- NULL

allRev <- list(
    "Socio-economic Interactions with Fishing" = wp2,
    "Ecological Effects on Fishing" = wp3,
    "Ecological Effects of Fishing" = wp4,
    "Spatial Management" = wp5
)

wp2$WP <- "Socio-economic"
wp3$WP <- "Ecosystem on Fisheries"
wp4$WP <- "Fisheries on Ecosystem"
wp5$WP <- "Spatial Management"

keepcols <- colnames(wp3[, c(1:29, length(colnames(wp3)))])
wp2$Concluding.statement.or.quotable.quote <- NA
wp2$Comments <- NA
colnames(wp5)[1:29] <- colnames(wp3)[1:29]


combRev <- rbind(wp2[!duplicated(wp2$SW.ID), keepcols],
                 wp3[!duplicated(wp3$SW.ID), keepcols],
                 wp4[!duplicated(wp4$SW.ID), keepcols],
                 wp5[!duplicated(wp5$SW.ID), keepcols])

regionlist <- c("Baltic Sea" = "baltic",
                "North Sea" = "north Sea",
                "Western Waters" = "western Waters",
                "Mediterranean Sea" = "mediterranean")

wp2ImpactList <- c("Economic", "Environmental", "Governance", "Health", "Social", "Unspecified")



#====

#===
# Look and Feel ----
#===
swCols <- c("#210384", "#037184", "#00B292", "#00B262", "#86C64E", "#C6E83E")
#====


# Define UI ----
ui <- navbarPage(
    
    # Application title
    title = "SEAwise Review Aggregator",
    # First Tab ----
    tabPanel(
        title = "Reviews Combined",
        # Sidebar with filtering options
        sidebarLayout(
            sidebarPanel(
                # Slider for date ranges
                sliderInput(inputId = "years",
                            label = "Publication Year Range",
                            min = min(combRev$Year, na.rm = T),
                            max = max(combRev$Year, na.rm = T),
                            value = c(min(combRev$Year, na.rm = T),max(combRev$Year, na.rm = T)),
                            step = 1,
                            sep = ""),
                # Checkboxes for selecting by regions
                checkboxGroupInput(inputId = "region",
                                   label = "Regions",
                                   choices = regionlist,),
                # Create data download button
                downloadButton(outputId = "dfDownload",
                               label = "Download CSV")
            ),
            
            # Introduce the tab and show a plot of the records being retained by the filters in the main panel
            mainPanel(
                h1("SEAwise Systematic Reviews"),
                p(paste0("This app. provides access to the results from the range of SEAwise systematic reviews, with the themes ", paste(labels(allRev), collapse = ", "), ".")),
                h2("Combined Results"),
                p("This front page provides an indication of where and when the total records from all reviews are from, allows you to apply time and space filters and download the resulting filtered data."),
                p("The downloaded file from this front page contains bibliographic information for each record, as well as the common fields that were extracted by each of the different reviews.  These fields cover temporal and spatial scale, sampling methods/data sources, broad analytical methods and some quality assessments.  These quality assessments are based on the appropriateness of the different scales of observations and the analyses employed to meet the studies' own stated aims and objectives."),
                h2("Theme Specific Results"),
                p("More specific review data can be investigated, filtered and downloaded using the different tabs at the top of the page"),
                p("The filters and displays in these pages are works in progress and your feedback will be used to ensure they developed to be useful and appropriate."),
                plotOutput("combTS")
            )
        )
    ),
    # Second tab ----
    tabPanel(
        title = "Socio-economic Interactions",
        # Sidebar with filtering options
        sidebarLayout(
            sidebarPanel(
                # Slider for date ranges
                sliderInput(inputId = "wp2years",
                            label = "Publication Year Range",
                            min = min(wp2$Year, na.rm = T),
                            max = max(wp2$Year, na.rm = T),
                            value = c(min(wp2$Year, na.rm = T),max(wp2$Year, na.rm = T)),
                            step = 1,
                            sep = ""),
                # Checkboxes for selecting by regions
                checkboxGroupInput(inputId = "wp2region",
                                   label = "Regions",
                                   choices = regionlist,),
                # Dropdown for selecting multiple MANAGEMENT POLICIES
                selectInput(inputId = "wp2MP",
                            label = "Relevant Managment Policy",
                            choices = unique(wp2$Management.Policy.Clean),
                            multiple = TRUE),
                # Dropdown for selecting multiple MANAGEMENT POLICY OBJECTIVES
                selectInput(inputId = "wp2MPO",
                            label = "Managment Policy Objectives",
                            choices = unique(wp2$Objective.of.Management.Policy.Clean),
                            multiple = TRUE),
                # Dropdown for selecting multiple TYPES OF IMPACT
                selectInput(inputId = "wp2ToI",
                            label = "Types of Impact",
                            choices = wp2ImpactList,
                            multiple = TRUE),
                # Create data download button
                downloadButton(outputId = "wp2Download",
                               label = "Download CSV")
            ),
            # Introduce the tab and show a plot of the records being retained by the filters in the main panel
            mainPanel(
                h1("SEAwise Systematic Reviews"),
                p(paste0("This app. provides access to the results from the range of SEAwise systematic reviews, with the themes ", paste(labels(allRev), collapse = ", "), ".")),
                h2("Socio-economic Interactions with Fishing"),
                p("This tab ......"),
                p("The downloaded file from this tab contains bibliographic information for each record, the common fields that were extracted by each of the different reviews, as well as a series of binary information fields specific to WP2, i.e. the socio-economic interactions with fisheries."),
                p("The filters and displays on this page include the spatio-temporal ones seen on the front page as well as a few example filters selected from all of the possible variables extracted from these records.  This list of radio buttons is a work in progress and your feedback will be used to ensure they developed to be useful and appropriate."),
                plotOutput("wp2TS")
            )
        )
    ),
    # third tab ----
    tabPanel(
        title = "Ecological Effects on Fishing",
        # Sidebar with filtering options
        sidebarLayout(
            sidebarPanel(
                # Slider for date ranges
                sliderInput(inputId = "wp3years",
                            label = "Publication Year Range",
                            min = min(wp3$Year, na.rm = T),
                            max = max(wp3$Year, na.rm = T),
                            value = c(min(wp3$Year, na.rm = T),max(wp3$Year, na.rm = T)),
                            step = 1,
                            sep = ""),
                # Checkboxes for selecting by regions
                checkboxGroupInput(inputId = "wp3region",
                                   label = "Regions",
                                   choices = regionlist,),
                # Dropdown for selecting multiple MANAGEMENT POLICIES
                selectInput(inputId = "wp3MP",
                            label = "Relevant Managment Policy",
                            choices = unique(wp2$Management.Policy.Clean),
                            multiple = TRUE),
                # Dropdown for selecting multiple MANAGEMENT POLICY OBJECTIVES
                selectInput(inputId = "wp2MPO",
                            label = "Managment Policy Objectives",
                            choices = unique(wp2$Objective.of.Management.Policy.Clean),
                            multiple = TRUE),
                # Dropdown for selecting multiple TYPES OF IMPACT
                selectInput(inputId = "wp2ToI",
                            label = "Types of Impact",
                            choices = wp2ImpactList,
                            multiple = TRUE),
                # Create data download button
                downloadButton(outputId = "wp2Download",
                               label = "Download CSV")
            ),
            # Introduce the tab and show a plot of the records being retained by the filters in the main panel
            mainPanel(
                h1("SEAwise Systematic Reviews"),
                p(paste0("This app. provides access to the results from the range of SEAwise systematic reviews, with the themes ", paste(labels(allRev), collapse = ", "), ".")),
                h2("Socio-economic Interactions with Fishing"),
                p("This tab ......"),
                p("The downloaded file from this tab contains bibliographic information for each record, the common fields that were extracted by each of the different reviews, as well as a series of binary information fields specific to WP2, i.e. the socio-economic interactions with fisheries."),
                p("The filters and displays on this page include the spatio-temporal ones seen on the front page as well as a few example filters selected from all of the possible variables extracted from these records.  This list of radio buttons is a work in progress and your feedback will be used to ensure they developed to be useful and appropriate."),
                plotOutput("wp2TS")
            )
        )
    )
)

# Define server ----
server <- function(input, output) {
    
    #===
    # Output for Reviews Combined tab ----
    #===
    # create datasets based on user input filters to be used for plotting and downloads
    combData <- reactive({
        combRev[combRev$Year >= input$years[1] &
                    combRev$Year <= input$years[2] &
                    grepl(pattern = paste(input$region, collapse = "|"),
                          x = combRev$Region,
                          ignore.case = TRUE)  &
                    rowSums(is.na(combRev)) != ncol(combRev), ]
    })
    tsComb <- reactive({
        merge(data.frame(PublicationYear=as.numeric(rownames(table(combData()[!duplicated(combData()$SW.ID), "Year"]))),
                         TotalRecords=as.vector(table(combData()[!duplicated(combData()$SW.ID), "Year"]))),
              data.frame(PublicationYear=c(min(as.numeric(rownames(table(combData()[!duplicated(combData()$SW.ID), "Year"])))):max(as.numeric(rownames(table(combData()[!duplicated(combData()$SW.ID), "Year"])))))),
              by="PublicationYear",
              all = TRUE)
    })
    # draw bar plot of records by area with a moving average of totals
    output$combTS <- renderPlot({
        req(combData(), tsComb())
        ggplot() +
            geom_bar(data = combData(),
                     mapping = aes(x = Year,
                                   fill = WP),
                     position = position_dodge(preserve = "single")) +
            geom_ma(data = tsComb(),
                    mapping = aes(x = `PublicationYear`, y = TotalRecords),
                    ma_fun = SMA, n = 5, color = swCols[1], linetype = 1, size = 1) +
            scale_x_continuous(n.breaks = ((max(combData()$Year, na.rm = T)-min(combData()$Year, na.rm = T))/5)) +
            scale_fill_manual(values = swCols[1:length(unique(combData()$WP))]) +
            theme_few()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  legend.position = "bottom")
    })
    # Create downloadable csv product
    output$dfDownload <- downloadHandler(filename = function(){
        paste0("SW_combinedReviews_", input$years[1], "-", input$years[2], "_", paste(input$region,collapse = "-"), "_", Sys.Date(), ".csv")
    },
    content = function(file){
        write.csv(combData(), file, row.names = FALSE)
    })
    #====
    
    #===
    # Output for the Socio-economic Interactions tab ----
    #===
    # create reactive dataframe to be used in plotting and file-download creation
    wp2Data <- reactive({
        wp2[wp2$Year >= input$wp2years[1] &
                wp2$Year <= input$wp2years[2] &
                grepl(pattern = paste(input$wp2region, collapse = "|"),
                      x = wp2$Region,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp2MP, collapse = "|"),
                      x = wp2$Management.Policy.Clean)  &
                grepl(pattern = paste(input$wp2MPO, collapse = "|"),
                      x = wp2$Objective.of.Management.Policy.Clean)  &
                grepl(pattern = paste(input$wp2ToI, collapse = "|"),
                      x = wp2$Type.of.impact.Clean), ]
    })
    
    tsWp2 <- reactive({
        merge(data.frame(PublicationYear=as.numeric(rownames(table(wp2Data()[!duplicated(wp2Data()$SW.ID), "Year"]))),
                         TotalRecords=as.vector(table(wp2Data()[!duplicated(wp2Data()$SW.ID), "Year"]))),
              data.frame(PublicationYear=c(min(as.numeric(rownames(table(wp2Data()[!duplicated(wp2Data()$SW.ID), "Year"])))):max(as.numeric(rownames(table(wp2Data()[!duplicated(wp2Data()$SW.ID), "Year"])))))),
              by="PublicationYear",
              all = TRUE)
    })
    # draw bar plot of records by area with a moving average of totals
    output$wp2TS <- renderPlot({
        req(wp2Data(), tsWp2())
        if(nrow(wp2Data()) <= 6){ 
            wp2TSplot <- ggplot() +
                geom_bar(data = wp2Data(),
                         mapping = aes(x = Year,
                                       fill = WP),
                         position = position_dodge(preserve = "single")) +
                scale_x_continuous(n.breaks = ((max(wp2Data()$Year, na.rm = T)-min(wp2Data()$Year, na.rm = T))/5)) +
                scale_fill_manual(values = swCols[1:length(unique(wp2Data()$WP))]) +
                theme_few()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                      legend.position = "bottom") +
                guides(fill = "none")
        } else {wp2TSplot <- ggplot() +
            geom_bar(data = wp2Data(),
                     mapping = aes(x = Year,
                                   fill = WP),
                     position = position_dodge(preserve = "single")) +
            geom_ma(data = tsWp2(),
                    mapping = aes(x = `PublicationYear`, y = TotalRecords),
                    ma_fun = SMA, n = 5, color = swCols[2], linetype = 1, size = 1) +
            scale_x_continuous(n.breaks = ((max(wp2Data()$Year, na.rm = T)-min(wp2Data()$Year, na.rm = T))/5)) +
            scale_fill_manual(values = swCols[1:length(unique(wp2Data()$WP))]) +
            theme_few()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  legend.position = "bottom") +
            guides(fill = "none")
        }
        wp2TSplot
    })
    # Create downloadable csv product
    output$wp2Download <- downloadHandler(filename = function(){
        paste0("SW_SocioEconomicReviews_", input$wp2years[1], "-", input$wp2years[2], "_", paste(input$wp2region,collapse = "-"), "_", Sys.Date(), ".csv")
    },
    content = function(file){
        write.csv(wp2Data(), file, row.names = FALSE)
    })
    #====
    
    #===
    # Output for the Ecological Effects tab ----
    #===
    # create reactive dataframe to be used in plotting and file-download creation
    wp3Data <- reactive({
        wp3[wp3$Year >= input$wp3years[1] &
                wp3$Year <= input$wp3years[2] &
                grepl(pattern = paste(input$wp3region, collapse = "|"),
                      x = wp3$Region,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp3MP, collapse = "|"),
                      x = wp3$Management.Policy.Clean)  &
                grepl(pattern = paste(input$wp3MPO, collapse = "|"),
                      x = wp3$Objective.of.Management.Policy.Clean)  &
                grepl(pattern = paste(input$wp3ToI, collapse = "|"),
                      x = wp3$Type.of.impact.Clean), ]
    })
    
    tsWp3 <- reactive({
        merge(data.frame(PublicationYear=as.numeric(rownames(table(wp3Data()[!duplicated(wp3Data()$SW.ID), "Year"]))),
                         TotalRecords=as.vector(table(wp3Data()[!duplicated(wp3Data()$SW.ID), "Year"]))),
              data.frame(PublicationYear=c(min(as.numeric(rownames(table(wp3Data()[!duplicated(wp3Data()$SW.ID), "Year"])))):max(as.numeric(rownames(table(wp3Data()[!duplicated(wp3Data()$SW.ID), "Year"])))))),
              by="PublicationYear",
              all = TRUE)
    })
    # draw bar plot of records by area with a moving average of totals
    output$wp3TS <- renderPlot({
        req(wp3Data(), tsWp3())
        if(nrow(wp3Data()) <= 6){ 
            wp3TSplot <- ggplot() +
                geom_bar(data = wp3Data(),
                         mapping = aes(x = Year,
                                       fill = WP),
                         position = position_dodge(preserve = "single")) +
                scale_x_continuous(n.breaks = ((max(wp3Data()$Year, na.rm = T)-min(wp3Data()$Year, na.rm = T))/5)) +
                scale_fill_manual(values = swCols[1:length(unique(wp3Data()$WP))]) +
                theme_few()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                      legend.position = "bottom") +
                guides(fill = "none")
        } else {wp3TSplot <- ggplot() +
            geom_bar(data = wp3Data(),
                     mapping = aes(x = Year,
                                   fill = WP),
                     position = position_dodge(preserve = "single")) +
            geom_ma(data = tsWp3(),
                    mapping = aes(x = `PublicationYear`, y = TotalRecords),
                    ma_fun = SMA, n = 5, color = swCols[2], linetype = 1, size = 1) +
            scale_x_continuous(n.breaks = ((max(wp3Data()$Year, na.rm = T)-min(wp3Data()$Year, na.rm = T))/5)) +
            scale_fill_manual(values = swCols[1:length(unique(wp3Data()$WP))]) +
            theme_few()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  legend.position = "bottom") +
            guides(fill = "none")
        }
        wp3TSplot
    })
    # Create downloadable csv product
    output$wp3Download <- downloadHandler(filename = function(){
        paste0("SW_EcologicalEffects_", input$wp3years[1], "-", input$wp3years[2], "_", paste(input$wp3region,collapse = "-"), "_", Sys.Date(), ".csv")
    },
    content = function(file){
        write.csv(wp3Data(), file, row.names = FALSE)
    })
    #====
    
    #===
    # Output for the Fisheries Effects tab ----
    #===
    
    #====
    
    #===
    # Output for the Spatial Management tab ----
    #===
    
    #====
}


# Run the application 
shinyApp(ui = ui, server = server)
# runApp(appDir = paste0(getwd(), "/Systematic Reviews/Analysis Task 1.2/Scripts/SEAwiseReviewAggregator/"),launch.browser = TRUE)
