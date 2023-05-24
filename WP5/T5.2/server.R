# load packages
library(ggplot2)
library(maps)
library(fields)
library(viridis)
library(data.table)

# load the data
# if(!exists("grid"))load(file = "data/predictions_all.RData")
# if(!exists("all_combs"))load(file = "data/dashboard_combinations.RData")
species_info <- read.csv("data/species_info.csv")
# # load world map
world_map <- map_data("world")
world_map$lon <- world_map$long
# draw the map to increase speed
world_map <- ggplot(world_map, aes(x = lon, y = lat, group = group))

# helper function to add ICES rectangles
geom_ices_squares <- function(xlim = c(-49,70), ylim = c(36, 85.5), draw = T,...){
  outside <- F
  if(xlim[1] < -49) {xlim[1] <- -49; outside <- T}
  if(xlim[2] > 70)  {xlim[2] <- 70; outside <- T}
  if(ylim[1] < -49) {ylim[1] <- -49; outside <- T}
  if(ylim[2] > 85.5){ylim[2] <- 85.5; outside <- T}
  if(outside) print("longitude & latitude must be inside the interval [-49;70] & [36;85.5]")
  to_draw <- list(#coord_cartesian(xlim, ylim),
    geom_vline(xintercept = -49:70,...),
    geom_hline(yintercept = seq(36,85.5, by = 0.5),...),
    scale_x_continuous(sec.axis = sec_axis(~.x,
                                           breaks = -49.5:69.5,
                                           labels = paste(rep(LETTERS[1:12],each=10),0:9,sep=''),
                                           guide = guide_axis(check.overlap = TRUE))),
    scale_y_continuous(sec.axis = sec_axis(~.x,
                                           breaks = seq(36.25, 85.25, by = 0.5),
                                           labels = sprintf("%02d",c(1:99)),
                                           guide = guide_axis(check.overlap = TRUE))),
    coord_quickmap(xlim, ylim)
    
  )
  if(!draw) to_draw <- to_draw[[5]]
  return(to_draw)
}

update_selection <- function(sel,new, slider = F){
  if(!slider){
    if(!is.null(sel)){
      if(!(sel %in% new)){
        sel <- new[1]
      }
    } else sel <- new[1]
  }
  if(slider){
    if(!is.null(sel[1]) & !is.null(sel[2])){
      if(sel[1] < min(new)){
        sel[1] <- min(new)
      }
      if(sel[2] > max(new)){
        sel[2] <- max(new)
      }
    }
  }
  sel
}
shinyServer(function(input, output) {
  
  # create some reactiveValues to store the current selection
  values <- reactiveValues(projection.sel= "historical",
                           stage.sel = "adu",
                           model.sel = "gam",
                           case_study.sel = "FAO_27",
                           years_min.sel = all_combs$year_min[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1],
                           years_max.sel = all_combs$year_max[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1],
                           lon_min.sel = all_combs$lon_min[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1],
                           lon_max.sel = all_combs$lon_max[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1],
                           lat_min.sel = all_combs$lat_min[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1],
                           lat_max.sel = all_combs$lat_max[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1],
                           q.sel = all_combs$q_min[all_combs$species == "Pleuronectes_platessa" & all_combs$case_study == "FAO_27"][1])
  
  
  output$case_controls <- renderUI({
    
    case_studies   <- unique(all_combs$case_study[all_combs$species == input$species])
    case_studies   <- setNames(case_studies,case_studies)
    
    values$case_study.sel <- update_selection(values$case_study.sel,case_studies)
    
    selectInput("case_study", "Region:",
                case_studies,
                selected = values$case_study.sel)
  })
  
  
  output$projection_controls <- renderUI({
    projections <- unique(all_combs$climate_prediction[all_combs$species == input$species &
                                                         all_combs$case_study == input$case_study])
    projections <- setNames(projections,projections)
    
    values$projection.sel <- update_selection(values$projection.sel,projections)
    
    selectInput("projection", "Climate scenario:",
                projections,
                selected = values$projection.sel)
  })
  
  output$stage_controls <- renderUI({
    stages <- unique(all_combs$stage[all_combs$species == input$species &
                                       all_combs$case_study == input$case_study & 
                                       all_combs$climate_prediction == input$projection])
    stages <- setNames(stages,stages)
    
    values$stage.sel <- update_selection(values$stage.sel,stages)
    selectInput("stage", "Lifestage:",
                stages,
                selected = values$stage.sel)
  })
  
  
  output$model_controls <- renderUI({
    models <- unique(all_combs$model[all_combs$species == input$species &
                                       all_combs$case_study == input$case_study & 
                                       all_combs$climate_prediction == input$projection &
                                       all_combs$stage == input$stage])
    models <- setNames(models,models)
    values$model.sel <- update_selection(values$model.sel,models)
    
    selectInput("model", "Available models:",
                models,
                selected = values$model.sel)
  })
  
  output$year_controls <- renderUI({
    years <-          unlist(all_combs[all_combs$species == input$species &
                                         all_combs$case_study == input$case_study & 
                                         all_combs$climate_prediction == input$projection &
                                         all_combs$stage == input$stage &
                                         all_combs$model == input$model, c("year_min","year_max")])
    
    stepsize <- ifelse(input$projection == "historical", 1, 5)
    
    if(is.null(values$years_min.sel)) values$years_min.sel <- years[1]
    if(is.null(values$years_max.sel)) values$years_min.sel <- years[2]
    
    if(values$years_min.sel < years[1]) values$years_min.sel <- years[1]
    if(values$years_max.sel > years[2]) values$years_max.sel <- years[2]
    
    if(values$years_min.sel > years[2]) values$years_min.sel <- years[2]
    if(values$years_max.sel < years[1]) values$years_max.sel <- years[1]
    
    sliderInput("year_range", "years:",
                min = years[1], max = years[2], 
                value = c(values$years_min.sel),#values$years_max.sel), 
                step = stepsize, sep = "")
  })
  
  output$quarter_controls <- renderUI({
    Qs <-          unlist(all_combs[all_combs$species == input$species &
                                      all_combs$case_study == input$case_study & 
                                      all_combs$climate_prediction == input$projection &
                                      all_combs$stage == input$stage &
                                      all_combs$model == input$model, c("q_min","q_max")])
    
    
    if(is.null(values$q.sel)) values$q.sel <- Qs[1]
    
    if(values$q.sel < Qs[1]) values$q.sel <- Qs[1]
    if(values$q.sel > Qs[2]) values$q.sel <- Qs[2]
    
    sliderInput("quarter", "Quarter:",
                min = Qs[1], max = Qs[2], 
                value = values$q.sel, 
                step = 1, sep = "")
  })
  
  
  output$coord_x <- renderUI({
    lon_range <- unlist(all_combs[all_combs$species == input$species &
                                    all_combs$case_study == input$case_study & 
                                    all_combs$climate_prediction == input$projection &
                                    all_combs$stage == input$stage &
                                    all_combs$model == input$model,c("lon_min","lon_max")])
    
    
    if(is.null(values$lon_min.sel)) values$lon_min.sel <- lon_range[1]
    if(is.null(values$lon_max.sel)) values$lon_max.sel <- lon_range[2]
    if(values$lon_min.sel < lon_range[1]) values$lon_min.sel <- lon_range[1]
    if(values$lon_max.sel > lon_range[2]) values$lon_max.sel <- lon_range[2]
    if(values$lon_min.sel > lon_range[2]) values$lon_min.sel <- lon_range[2]
    if(values$lon_max.sel < lon_range[1]) values$lon_max.sel <- lon_range[1]
    
    
    sliderInput("xlims", "longitude:",
                min = floor(lon_range[1]), 
                max = ceiling(lon_range[2]),
                value = c(floor(values$lon_min.sel), 
                          ceiling(values$lon_max.sel)), step = .5)
  })
  output$coord_y <- renderUI({
    lat_range <- unlist(all_combs[all_combs$species == input$species &
                                    all_combs$case_study == input$case_study & 
                                    all_combs$climate_prediction == input$projection &
                                    all_combs$stage == input$stage &
                                    all_combs$model == input$model,c("lat_min","lat_max")])
    
    if(is.null(values$lat_min.sel)) values$lat_min.sel <- lat_range[1]
    if(is.null(values$lat_max.sel)) values$lat_max.sel <- lat_range[2]
    if(values$lat_min.sel < lat_range[1]) values$lat_min.sel <- lat_range[1]
    if(values$lat_max.sel > lat_range[2]) values$lat_max.sel <- lat_range[2]
    if(values$lat_min.sel > lat_range[2]) values$lat_min.sel <- lat_range[2]
    if(values$lat_max.sel < lat_range[1]) values$lat_max.sel <- lat_range[1]
    
    sliderInput("ylims", "latitude:",
                min = floor(lat_range[1]), 
                max = ceiling(lat_range[2]), 
                value = c(floor(values$lat_min.sel), ceiling(values$lat_max.sel)), 
                step = .25)
  })
  
  output$printValues <- renderPrint({
    paste0(values$lat_min.sel,"_",values$projection.sel)
  })
  
  observeEvent(input$show_map,{
    
    # update reactive values with slider inputs
    values$projection.sel = input$projection
    values$stage.sel = input$stage
    values$model.sel = input$model
    values$case_study.sel = input$case_study
    values$years_min.sel = input$year_range#[1]
    # values$years_max.sel = input$year_range[2]
    values$q.sel = input$quarter
    values$lon_min.sel = input$xlims[1]
    values$lon_max.sel = input$xlims[2]
    values$lat_min.sel = input$ylims[1]
    values$lat_max.sel = input$ylims[2]
    
    isolate({ to_plot <- grid[grid$species == input$species &
                                grid$case_study == input$case_study & 
                                grid$climate_prediction == input$projection &
                                grid$stage == input$stage &
                                grid$model == input$model &
                                grid$quarter == as.numeric(input$quarter) &
                                grid$lon >= input$xlims[1] & grid$lon <= input$xlims[2] & 
                                grid$lat >= input$ylims[1] & grid$lat <= input$ylims[2],]
    
    if(nrow(to_plot)>0){
      to_plot$group <- NA
      
      available_years <- sort(unique(to_plot$Year))
      # start_idx <- which(available_years==input$year_range[1])
      # end_idx   <- which(available_years==input$year_range[2])
      # timeslices <- available_years#[start_idx:end_idx]
      
      # if(length(timeslices)>6){
      #   timeslices <- timeslices[unique(round(seq(start_idx,end_idx, length.out = 6)))]
      # }
      # timeslices[timeslices>input$year_range[2]] <- input$year_range[2]
      # timeslices[timeslices<input$year_range[1]] <- input$year_range[1]
      # 
      # to_plot <- to_plot[to_plot$Year %in% timeslices, ]
      to_plot <- to_plot[to_plot$Year %in% c(input$year_range), ]#,input$year_range[2]), ]
      
      # lanThres      <- 1.5
      # wgh   <- sort(unique(to_plot$density))
      # difw  <- diff(log10(wgh))
      # max_wgh <- max(wgh, na.rm = T)
      # if(any(difw > lanThres)) max_wgh <- wgh[rev(which(difw <= lanThres)+1)]
      # to_plot$density[to_plot$density > max_wgh] <- max_wgh
      
      # output$year_info <- renderText({
      #   paste0("Maximum 6 plots (",paste0(timeslices, collapse = "; "),") are drawn, adjust year range according your preferences")
      # })
      
      input_ices_rectangles <- input$ices_rectangles
      input_xlims <- input$xlims
      input_ylims <- input$ylims
    }
    
    })
    if(nrow(to_plot)>0){
      output$density_map <- renderPlot({
        #draw plot
        world_map +
          geom_raster(data = to_plot,aes(lon,lat,fill = density),interpolate = F) +
          geom_ices_squares(xlim = input_xlims, ylim = input_ylims, input_ices_rectangles,
                            linetype = "solid", colour = "black", size = .5) +
          #scale_fill_viridis_c(direction = -1, name = "biomass (kg²/km)") +
          scale_fill_gradientn(colours = tim.colors(30)) + 
          geom_polygon(fill="grey80", colour = "grey60") +
          facet_wrap(.~Year, ncol = 1) +
          theme_bw()  +
          theme(axis.text.y.right = element_text(size=6),
                axis.text.x.top = element_text(size=6),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.position = "bottom") +
          xlab("longitude") +
          ylab("latitude")
      })
    }
    
  })
  output$species_info <- renderTable(species_info)
})
