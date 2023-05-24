library(shiny)
library(shinyfullscreen)
shinyUI(fluidPage(
    # Application title
    titlePanel(""),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(width = 3,
                   tags$head(tags$style(".shiny-plot-output{height:90vh !important;}")),
        selectInput("species", "Species:",
                    c( "Ammodytes tobianus" = "Ammodytes_tobianus",         
                       "Anarhichas lupus" = "Anarhichas_lupus",           
                       "Brosme brosme" = "Brosme_brosme",              
                       "Chelidonichthys cuculus" = "Chelidonichthys_cuculus",    
                       "Chelidonichthys lucerna" = "Chelidonichthys_lucerna",
                       "Clupea harengus" = "Clupea_harengus",
                       "Conger conger" = "Conger_conger",
                       "Dicentrarchus labrax" = "Dicentrarchus_labrax",
                       "Engraulis encrasicolus" = "Engraulis_encrasicholus",
                       "Gadus morhua" = "Gadus_morhua",
                       "Hyperoplus immaculatus" = "Hyperoplus_immaculatus",
                       "Lepidorhombus whiffiagonis" = "Lepidorhombus_whiffiagonis",
                       "Leucoraja naevus" = "Leucoraja_naevus",
                       "Limanda limanda" = "Limanda_limanda",
                       "Loligo vulgaris" = "Loligo_vulgaris",
                       "Lophius budegassa" = "Lophius_budegassa",
                       "Lophius piscatorius" = "Lophius_piscatorius",
                       "Melanogrammus aeglefinus" = "Melanogrammus_aeglefinus",
                       "Merlangius merlangus" = "Merlangius_merlangus",
                       "Merluccius merluccius" = "Merluccius_merluccius",
                       "Microstomus kitt" = "Microstomus_kitt",
                       "Molva molva" = "Molva_molva", 
                       "Mullus surmuletus" = "Mullus_surmuletus",
                       "Platichthys flesus" = "Platichthys_flesus",
                       "Pleuronectes platessa" = "Pleuronectes_platessa",
                       "Pollachius pollachius" = "Pollachius_pollachius",
                       "Pollachius virens" = "Pollachius_virens",
                       "Raja brachyura" = "Raja_brachyura",
                       "Raja clavata" = "Raja_clavata",
                       "Raja montagui" = "Raja_montagui",
                       "Sardina pilchardus" = "Sardina_pilchardus",
                       "Scomber scombrus" = "Scomber_scombrus",
                       "Scopthalmus maximus" = "Scophthalmus_maximus",
                       "Scopthalmus rhombus" = "Scophthalmus_rhombus",
                       "Scyliorhinus canicula" = "Scyliorhinus_canicula",
                       "Solea solea" = "Solea_solea",
                       "Sepia officinalis" = "Sepia_officinalis",
                       "Sprattus sprattus" = "Sprattus_sprattus",
                       "Squalus acanthias" = "Squalus_acanthias",
                       "Trachurus trachurus" = "Trachurus_trachurus",
                       "Trisopterus esmarkii" = "Trisopterus_esmarkii",
                       "Trisopterus luscus" = "Trisopterus_luscus",
                       "Zeus faber" = "Zeus_faber"),
                    selected = "Pleuronectes_platessa"),
        
        uiOutput("case_controls"),
        uiOutput("projection_controls"),
        uiOutput("stage_controls"),
        uiOutput("model_controls"),
        uiOutput("year_controls"),
        # textOutput("year_info"),
        uiOutput("quarter_controls"),
        uiOutput("coord_x"),
        uiOutput("coord_y"),
        
        checkboxInput("ices_rectangles", "draw ICES statistical rectangles", value = FALSE),
        
        actionButton("show_map", "Update map")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(width = 9,
                img(src='seawise.png', style="float:top;margin:-50px 0px;float:right"),
                tabsetPanel(
                  tabPanel("Maps",fullscreen_this(plotOutput("density_map", width = "80%"))),
                  tabPanel("Species",tableOutput("species_info"))
      ))
    )
))
