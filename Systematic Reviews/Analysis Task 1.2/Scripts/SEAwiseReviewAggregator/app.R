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
# Data import and cleaning ----
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
wp4$Study.type <- ifelse(is.na(wp4$Study.type), "Other", wp4$Study.type)
wp4$Study.type <- ifelse(wp4$Study.type == "combination of field surveys, byctach and over many decades", "Other", wp4$Study.type)
# wp4$Pressure_level <- ifelse(is.na(wp4$Pressure_level), "Not specified", wp4$Pressure_level)


# wp5 <- read.csv(file = "Systematic Reviews/Analysis Task 1.2/Databases/Database_5_20220829.csv", header = T) # Path when run manually
wp5 <- read.csv(file = "../../Databases/Database_5_20220829.csv", header = T)
wp5$Name <- NULL
wp5 <- wp5[wp5$Exclusion.Criteria == "K",]
wp5$Exclusion.Criteria <- NULL
wp5$Sampling.Method.used.for.data.collection <- ifelse(wp5$Sampling.Method.used.for.data.collection == "Regular & Irregular Fisheries Independent Survey", "Regular Fisheries Independent Survey _ Irregular Fisheries Independent Survey", wp5$Sampling.Method.used.for.data.collection)
wp5$species <- gsub(pattern = "crustaceans", replacement = "Crustaceans", wp5$species)
wp5$species <- gsub(pattern = "various", replacement = "Various", wp5$species)
wp5$species <- gsub(pattern = "Various", replacement = "Species assemblage", wp5$species)
wp5$species <- gsub(pattern = "Scombrus scombrus", replacement = "Scomber scombrus", wp5$species)
wp5$species <- gsub(pattern = "Merlangius merlangius", replacement = "Merlangius merlangus", wp5$species)
wp5$species <- gsub(pattern = "Melanogarmmus aeglefinus", replacement = "Melanogrammus aeglefinus", wp5$species)
wp5$species <- gsub(pattern = "Pelagic species", replacement = "Pelagic assemblage", wp5$species)
wp5$species <- gsub(pattern = "small pelagics", replacement = "Pelagic assemblage", wp5$species)
wp5$species <- gsub(pattern = "Trachurus trachurus", replacement = "Trachurus spp", wp5$species)
wp5$species <- gsub(pattern = "elasmobranch", replacement = "Elasmobranch", wp5$species)
wp5$species <- gsub(pattern = "Flatfish species", replacement = "Species assemblage", wp5$species)
wp5$species <- gsub(pattern = "Lophius budegassa", replacement = "Lophius spp", wp5$species)
wp5$species <- gsub(pattern = "Lophius", replacement = "Lophius spp", wp5$species)
wp5$life.stage <- gsub("juv and adults", "juveniles and adults", wp5$life.stage)
wp5$life.stage <- gsub("Juveniles", "juveniles", wp5$life.stage)
wp5$life.stage <- gsub("all", "full lifecycle", wp5$life.stage)

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

wp5tasks <- c("5.1", "5.2", "5.3", "5.4", "5.5")
wp5spList <- paste(gsub(pattern = " _ ", ", ", unique(wp5$species)), collapse = ", ")
wp5spList <- paste(gsub(pattern = "_ ", ", ", wp5spList), collapse = ", ")
wp5spList <- unlist(strsplit(paste(gsub(pattern = "_", ", ", wp5spList), collapse = ", "), split = ", "))
wp5spList <- wp5spList[!wp5spList %in% c("")]
wp5spList <- unique(wp5spList[order(wp5spList)])
wp5LHSlist <- c("adults", "juveniles", "early life  stages", "full lifecycle")
wp5FSList <- c("Commercial", "Recreational", "Research")
wp5ATList <- unique(wp5$analysis_category)[!unique(wp5$analysis_category) %in% c("")]
wp5ATList <- wp5ATList[order(wp5ATList)]

wp5SampMethList <- c("Regular Fisheries Independent Survey", "Irregular Fisheries Independent Survey", "Active Acoustic Sampling Survey", "Fisheries Dependent Data", "Simulated dynamics", "Tagging", "Interview", "Visual Analyses")

#====

#===
# Look and Feel ----
#===
swCols <- c("#210384", "#037184", "#00B292", "#00B262", "#86C64E", "#C6E83E")
#====


# Define UI ----
ui <- fluidPage(
    list(tags$head(HTML('<link rel="icon", href="SEAwise_Logo_Multicolour.png", type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
            title="", windowTitle="SEAwise Review Aggregator"
        )
    ),
    navbarPage(
        
        # Apply Theme
        theme = bs_theme(version = 4, bootswatch = "minty",
                         # base_font = "Futura",
                         # heading_font = "Roboto",
                         bg = "#FFFFFF",
                         fg = "#210384",
                         primary = "#00B292",
                         secondary = "#037184",
                         success = "#00B262",
                         info = "#86C64E",
                         danger = "#C6E83E"
                         ),
        # Application title
        title = div(img(src="SEAwise_Logo_Multicolour.png", height = '70px', width = '50px'), "SEAwise Review Aggregator"),
        
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
                                       choices = regionlist),
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
                                       choices = regionlist),
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
            title = "Ecological Effects",
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
                                       choices = regionlist),
                    # Dropdown for selecting multiple Driver Categories
                    selectInput(inputId = "wp3DC",
                                label = "Driver Categories",
                                choices = unique(wp3$Driverscategory),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Driver sub-Categories
                    selectInput(inputId = "wp3DSC",
                                label = "Driver sub-Categories",
                                choices = unique(wp3$Environmental.Drivers.),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Process of Fisheries Productivity
                    selectInput(inputId = "wp3PFP",
                                label = "Processes Being Modified",
                                choices = unique(wp3$Process),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Species
                    selectInput(inputId = "wp3Spp",
                                label = "Species",
                                choices = unique(wp3$Species),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Life-History Stages
                    selectInput(inputId = "wp3LHS",
                                label = "Life-History Stages",
                                choices = unique(wp3$Life.stage),
                                multiple = TRUE),
                    # Create data download button
                    downloadButton(outputId = "wp3Download",
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
                    plotOutput("wp3TS")
                )
            )
        ),
        # Fourth tab ----
        tabPanel(
            title = "Fisheries Effects",
            # Sidebar with filtering options
            sidebarLayout(
                sidebarPanel(
                    # Slider for date ranges
                    sliderInput(inputId = "wp4years",
                                label = "Publication Year Range",
                                min = min(wp4$Year, na.rm = T),
                                max = max(wp4$Year, na.rm = T),
                                value = c(min(wp4$Year, na.rm = T),max(wp4$Year, na.rm = T)),
                                step = 1,
                                sep = ""),
                    # Checkboxes for selecting by regions
                    checkboxGroupInput(inputId = "wp4region",
                                       label = "Regions",
                                       choices = regionlist),
                    # Dropdown for selecting multiple Study Types
                    selectInput(inputId = "wp4ST",
                                label = "Study Type",
                                choices = unique(wp4$Study.type),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Ecosystem Component
                    selectInput(inputId = "wp4EC",
                                label = "Ecosystem Component",
                                choices = unique(wp4$Ecosystem.component_level1),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Ecosystem sub-Component
                    selectInput(inputId = "wp4ESC",
                                label = "Ecosystem sub-Component",
                                choices = unique(wp4[!is.na(wp4$Ecosystem.component_level2), c("Ecosystem.component_level2")]),
                                multiple = TRUE),
                    # # Dropdown for selecting multiple Species             #### Needs more work on categorising species 
                    # selectInput(inputId = "wp4Spp",
                    #             label = "Species",
                    #             choices = unique(wp4$Species.taxonomic.group.s.),
                    #             multiple = TRUE),
                    # Dropdown for selecting Response Category
                    selectInput(inputId = "wp4RC",
                                label = "Response Measured (Category)",
                                choices = unique(wp4$Response.variable_category),
                                multiple = TRUE),
                    # Dropdown for selecting multiple Types of Pressure
                    selectInput(inputId = "wp4ToP",
                                label = "Pressure Exerted by Fisheries",
                                choices = unique(wp4$Pressure.type),
                                multiple = TRUE),
                    # Dropdown for selecting Target or Non-target Ecosystem Components
                    selectInput(inputId = "wp4TnT",
                                label = "Target or Non-target Ecosystem Components",
                                choices = unique(wp4[wp4$Pressure_level != "Not specified" | is.na(wp4$Pressure_level), c("Pressure_level")]),
                                multiple = TRUE),
                    # Dropdown for selecting Fishery Type
                    selectInput(inputId = "wp4FT",
                                label = "Fishery Segment",
                                choices = unique(wp4$Fishery.type),
                                multiple = TRUE),
                    # Create data download button
                    downloadButton(outputId = "wp4Download",
                                   label = "Download CSV")
                ),
                # Introduce the tab and show a plot of the records being retained by the filters in the main panel
                mainPanel(
                    h1("SEAwise Systematic Reviews"),
                    p(paste0("This app. provides access to the results from the range of SEAwise systematic reviews, with the themes ", paste(labels(allRev), collapse = ", "), ".")),
                    h2("Socio-economic Interactions with Fishing"),
                    p("This tab ......"),
                    p("The downloaded file from this tab contains bibliographic information for each record, the common fields that were extracted by each of the different reviews, as well as a series of binary information fields specific to WP2, i.e. the socio-economic interactions with fisheries."),
                    p("The filters and displays on this page include the spatio-temporal ones seen on the front page as well as a few example filters selected from all of the possible variables extracted from these records.  This list of filters is a work in progress and your feedback will be used to ensure they developed to be useful and appropriate."),
                    plotOutput("wp4TS")
                )
            )
        ),
        # Fifth tab ----
        tabPanel(
            title = "Spatial Management",
            # Sidebar with filtering options
            sidebarLayout(
                sidebarPanel(
                    # Slider for date ranges
                    sliderInput(inputId = "wp5years",
                                label = "Publication Year Range",
                                min = min(wp5$Year, na.rm = T),
                                max = max(wp5$Year, na.rm = T),
                                value = c(min(wp5$Year, na.rm = T),max(wp5$Year, na.rm = T)),
                                step = 1,
                                sep = ""),
                    # Checkboxes for selecting by regions
                    checkboxGroupInput(inputId = "wp5region",
                                       label = "Regions",
                                       choices = regionlist),
                    # Dropdown for selecting WP5 Tasks             
                    selectInput(inputId = "wp5task",
                                label = "WP5 Tasks",
                                choices = wp5tasks,
                                multiple = TRUE),
                    # Dropdown for selecting multiple Species             
                    selectInput(inputId = "wp5Spp",
                                label = "Species",
                                choices = wp5spList,
                                multiple = TRUE),
                    # Dropdown for selecting multiple Species             
                    selectInput(inputId = "wp5LHS",
                                label = "Life-history Stages",
                                choices = wp5LHSlist,
                                multiple = TRUE),
                    # Dropdown for selecting multiple Habitat Categories
                    selectInput(inputId = "wp5HC",
                                label = "Broad Scale Habitats",
                                choices = unique(wp5$habitats),
                                multiple = TRUE),
                    # Dropdown for selecting Fishery Type
                    selectInput(inputId = "wp5FT",
                                label = "Fishery Segment",
                                choices = unique(wp5$Fishery.type),
                                multiple = TRUE),
                    # Dropdown for selecting Driver/pressure
                    selectInput(inputId = "wp5DP",
                                label = "Driver/Pressure",
                                choices = unique(wp5$Driver.pressure.type),
                                multiple = TRUE),
                    # Dropdown for selecting Type of Analyses
                    selectInput(inputId = "wp5AT",
                                label = "Type of Analysis",
                                choices = wp5ATList,
                                multiple = TRUE),
                    # radio button for selecting if Maps are produced
                    checkboxInput(inputId = "wp5map",
                                  label = "Only records that produced maps.",
                                  value = FALSE),
                    # Create data download button
                    downloadButton(outputId = "wp5Download",
                                   label = "Download CSV")
                ),
                # Introduce the tab and show a plot of the records being retained by the filters in the main panel
                mainPanel(
                    h1("SEAwise Systematic Reviews"),
                    p(paste0("This app. provides access to the results from the range of SEAwise systematic reviews, with the themes ", paste(labels(allRev), collapse = ", "), ".")),
                    h2("Socio-economic Interactions with Fishing"),
                    p("This tab ......"),
                    p("The downloaded file from this tab contains bibliographic information for each record, the common fields that were extracted by each of the different reviews, as well as a series of binary information fields specific to WP2, i.e. the socio-economic interactions with fisheries."),
                    p("The filters and displays on this page include the spatio-temporal ones seen on the front page as well as a few example filters selected from all of the possible variables extracted from these records.  This list of filters is a work in progress and your feedback will be used to ensure they developed to be useful and appropriate."),
                    plotOutput("wp5TS")
                )
            )
        )
    )
)
#====

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
                grepl(pattern = paste(input$wp3DC, collapse = "|"),
                      x = wp3$Driverscategory)  &
                grepl(pattern = paste(input$wp3DSC, collapse = "|"),
                      x = wp3$Environmental.Drivers.)  &
                grepl(pattern = paste(input$wp3PFP, collapse = "|"),
                      x = wp3$Process)  &
                grepl(pattern = paste(input$wp3Spp, collapse = "|"),
                      x = wp3$Species)  &
                grepl(pattern = paste(input$wp3LHS, collapse = "|"),
                      x = wp3$Life.stage), ]
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
    # create reactive dataset modified by user input and utilised to draw figures and create downloadable dataset
    wp4Data <- reactive({
        wp4[wp4$Year >= input$wp4years[1] &
                wp4$Year <= input$wp4years[2] &
                grepl(pattern = paste(input$wp4region, collapse = "|"),
                      x = wp4$Region,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp4ST, collapse = "|"),
                      x = wp4$Study.type)  &
                grepl(pattern = paste(input$wp4EC, collapse = "|"),
                      x = wp4$Ecosystem.component_level1)  &
                grepl(pattern = paste(input$wp4ESC, collapse = "|"),
                      x = wp4$Ecosystem.component_level2)  &
                # grepl(pattern = paste(input$wp4Spp, collapse = "|"),          ### Needs more work on categorising species
                #       x = wp4$Species.taxonomic.group.s.)  &
                grepl(pattern = paste(input$wp4RC, collapse = "|"),
                      x = wp4$Response.variable_category)  &
                grepl(pattern = paste(input$wp4ToP, collapse = "|"),
                      x = wp4$Pressure.type)  &
                grepl(pattern = paste(input$wp4TnT, collapse = "|"),
                      x = wp4$Pressure_level)  &
                grepl(pattern = paste(input$wp4FT, collapse = "|"),
                      x = wp4$Fishery.type), ]
    })
    
    tsWp4 <- reactive({
        merge(data.frame(PublicationYear=as.numeric(rownames(table(wp4Data()[!duplicated(wp4Data()$SW.ID), "Year"]))),
                         TotalRecords=as.vector(table(wp4Data()[!duplicated(wp4Data()$SW.ID), "Year"]))),
              data.frame(PublicationYear=c(min(as.numeric(rownames(table(wp4Data()[!duplicated(wp4Data()$SW.ID), "Year"])))):max(as.numeric(rownames(table(wp4Data()[!duplicated(wp4Data()$SW.ID), "Year"])))))),
              by="PublicationYear",
              all = TRUE)
    })
    # draw bar plot of records by area with a moving average of totals
    output$wp4TS <- renderPlot({
        req(wp4Data(), tsWp4())
        if(nrow(wp4Data()) <= 6){ 
            wp4TSplot <- ggplot() +
                geom_bar(data = wp4Data(),
                         mapping = aes(x = Year,
                                       fill = WP),
                         position = position_dodge(preserve = "single")) +
                scale_x_continuous(n.breaks = ((max(wp4Data()$Year, na.rm = T)-min(wp4Data()$Year, na.rm = T))/5)) +
                scale_fill_manual(values = swCols[1:length(unique(wp4Data()$WP))]) +
                theme_few()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                      legend.position = "bottom") +
                guides(fill = "none")
        } else {wp4TSplot <- ggplot() +
            geom_bar(data = wp4Data(),
                     mapping = aes(x = Year,
                                   fill = WP),
                     position = position_dodge(preserve = "single")) +
            geom_ma(data = tsWp4(),
                    mapping = aes(x = `PublicationYear`, y = TotalRecords),
                    ma_fun = SMA, n = 5, color = swCols[2], linetype = 1, size = 1) +
            scale_x_continuous(n.breaks = ((max(wp4Data()$Year, na.rm = T)-min(wp4Data()$Year, na.rm = T))/5)) +
            scale_fill_manual(values = swCols[1:length(unique(wp4Data()$WP))]) +
            theme_few()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  legend.position = "bottom") +
            guides(fill = "none")
        }
        wp4TSplot
    })
    # Create downloadable csv product
    output$wp4Download <- downloadHandler(filename = function(){
        paste0("SW_EcologicalEffects_", input$wp4years[1], "-", input$wp4years[2], "_", paste(input$wp4region,collapse = "-"), "_", Sys.Date(), ".csv")
    },
    content = function(file){
        write.csv(wp4Data(), file, row.names = FALSE)
    })
    #====
    
    #===
    # Output for the Spatial Management tab ----
    #===
    # create reactive dataset modified by user input and utilised to draw figures and create downloadable dataset
    wp5Data0 <- reactive({
        wp5[wp5$Year >= input$wp5years[1] &
                wp5$Year <= input$wp5years[2] &
                grepl(pattern = paste(input$wp5region, collapse = "|"),
                      x = wp5$Region,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp5task, collapse = "|"),
                      x = wp5$WP5.task,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp5Spp, collapse = "|"),       
                      x = wp5$species,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp5LHS, collapse = "|"),       
                      x = wp5$life.stage,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp5HC, collapse = "|"),
                      x = wp5$habitats,
                      ignore.case = TRUE)  &
                grepl(pattern = paste(input$wp5FT, collapse = "|"),
                      x = wp5$Fishery.type,
                      ignore.case = T) &
                grepl(pattern = paste(input$wp5DP, collapse = "|"),
                      x = wp5$Driver.pressure.type)  &
                grepl(pattern = paste(input$wp5AT, collapse = "|"),
                      x = wp5$analysis_category), ]
    })
    
    wp5Data <- reactive({
        if(input$wp5map == TRUE){
            wp5Data0()[grepl(pattern = "yes", x = wp5Data0()$Maps.provided., ignore.case = TRUE),]
        } else {wp5Data0()}
    })
    
    tsWp5 <- reactive({
        merge(data.frame(PublicationYear=as.numeric(rownames(table(wp5Data()[!duplicated(wp5Data()$SW.ID), "Year"]))),
                         TotalRecords=as.vector(table(wp5Data()[!duplicated(wp5Data()$SW.ID), "Year"]))),
              data.frame(PublicationYear=c(min(as.numeric(rownames(table(wp5Data()[!duplicated(wp5Data()$SW.ID), "Year"])))):max(as.numeric(rownames(table(wp5Data()[!duplicated(wp5Data()$SW.ID), "Year"])))))),
              by="PublicationYear",
              all = TRUE)
    })
    # draw bar plot of records by area with a moving average of totals
    output$wp5TS <- renderPlot({
        req(wp5Data(), tsWp5())
        if(nrow(wp5Data()) <= 6){ 
            wp5TSplot <- ggplot() +
                geom_bar(data = wp5Data(),
                         mapping = aes(x = Year,
                                       fill = WP),
                         position = position_dodge(preserve = "single")) +
                scale_x_continuous(n.breaks = ((max(wp5Data()$Year, na.rm = T)-min(wp5Data()$Year, na.rm = T))/5)) +
                scale_fill_manual(values = swCols[1:length(unique(wp5Data()$WP))]) +
                theme_few()+
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                      legend.position = "bottom") +
                guides(fill = "none")
        } else {wp5TSplot <- ggplot() +
            geom_bar(data = wp5Data(),
                     mapping = aes(x = Year,
                                   fill = WP),
                     position = position_dodge(preserve = "single")) +
            geom_ma(data = tsWp5(),
                    mapping = aes(x = `PublicationYear`, y = TotalRecords),
                    ma_fun = SMA, n = 5, color = swCols[2], linetype = 1, size = 1) +
            scale_x_continuous(n.breaks = ((max(wp5Data()$Year, na.rm = T)-min(wp5Data()$Year, na.rm = T))/5)) +
            scale_fill_manual(values = swCols[1:length(unique(wp5Data()$WP))]) +
            theme_few()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                  legend.position = "bottom") +
            guides(fill = "none")
        }
        wp5TSplot
    })
    # Create downloadable csv product
    output$wp5Download <- downloadHandler(filename = function(){
        paste0("SW_EcologicalEffects_", input$wp5years[1], "-", input$wp5years[2], "_", paste(input$wp5region,collapse = "-"), "_", Sys.Date(), ".csv")
    },
    content = function(file){
        write.csv(wp5Data(), file, row.names = FALSE)
    })
    #====
}


# Run the application 
shinyApp(ui = ui, server = server)
# runApp(appDir = paste0(getwd(), "/Systematic Reviews/Analysis Task 1.2/Scripts/SEAwiseReviewAggregator/"),launch.browser = TRUE)
