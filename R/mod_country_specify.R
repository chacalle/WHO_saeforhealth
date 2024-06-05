#' country_specify UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'


#surveyPrev_ind_list <-  full_ind_des # surveyPrev::surveyPrevIndicators
#surveyPrev_ind_list <- surveyPrev::surveyPrevIndicators

#indicator_choices_vector <- setNames(surveyPrev_ind_list$ID, surveyPrev_ind_list$Description)
#load(file='data/DHS_meta_preload_04172024.rda')
#DHS.country.meta <- DHS.country.meta.preload
#DHS.survey.meta <- DHS.survey.meta.preload
#DHS.dataset.meta <- DHS.dataset.meta.preload


mod_country_specify_ui <- function(id){
  ns <- NS(id)



  fluidPage(
    div(class = "module-title",
        h4("Country Meta Data Input")),

    fluidRow(

      column(4,
             #h4("Data Input"),              #div(style = "margin-top: 10px;",
             #tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")

             ### country name
             selectInput(ns("country"), with_red_star("Choose a country "),
                         character(0)),
             ### survey year
             selectInput(ns("Svy_year"),  with_red_star("Choose survey year "), choices = character(0)),

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")


             ### survey indicators
             selectInput(ns("Svy_ind_group"),  with_red_star("Choose an indicator group "), choices = character(0)),

             shinyWidgets::pickerInput(ns("Svy_indicator"),  with_red_star("Choose an indicator "),
                                       choices = character(0),
                                       multiple = F,
                                       selected = NULL),

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")


             ### admin region selection

             checkboxGroupInput(
               ns("admin_levels_analysis"),
               with_red_star("Choose Admin levels for analysis "),
               #choices = c("Admin-0" = "National", "Admin-1" = "First Level", "Admin-2" = "Second Level"),
               choices = c('National','Admin-1','Admin-2'),
               inline = F
             ),
             textOutput(ns("selected_admin_levels")),

             selectInput(ns("AdminLevel"), "Check out maps for Admin levels",
                         choices=character(0)),

             tags$hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;")

             ### toggle input for interactive map
             shinyWidgets::materialSwitch(inputId = ns("mapType"), label = HTML("<strong>Interactive Map Enabled</strong>"),
                                          status = "success",value =T)

      ),
      column(8,
             div(style = "width: max(50%, 600px); margin: auto;float: left;",
                 uiOutput(ns("country_meta_display"))
             ),

             div(style = "width: max(50%, 600px); margin-top: -15px;margin-bottom: -30px; float: left; font-size: 1.625rem;",
                 tableOutput(ns("gadmTable"))
             ),

             div(style = "width: max(50%, 600px); margin: auto;float: left;",
                 uiOutput(ns("text_admin_display"))
             ),

             #hr(style="border-top-color: #E0E0E0;"), # (style="border-top: 2px solid #707070;"),
             #div(
             #   id = "map-container",
             # style = "width: max(50%, 600px); margin: auto; margin-top: 20px; float: left;",
             # leaflet::leafletOutput(ns("country_map"))
             #))
             div(
               id = "map-container",
               style = "width: max(50%, 600px); margin: auto; margin-top: 20px; float: left;",
               uiOutput(ns("mapUI"))
             ))
    )#,
    #fluidRow(column(12,
    #actionButton(ns("switch_bar"), "Switch to another panel")))

  )
}

#' country_specify Server Functions
#'
#' @noRd
mod_country_specify_server <- function(id,CountryInfo,AnalysisInfo,parent_session){
  moduleServer( id, function(input, output, session){


    ns <- session$ns

    observeEvent(input$switch_bar, {
      message('switching')
      updateTabItems(parent_session, "Overall_tabs", selected = "data_upload")
    })

    ### initialize selection for country, indicator group and indicators
    # update country

    if(F){
    country_name_list <- sort(DHS.country.meta[['CountryName']])
    updateSelectInput(inputId = "country", choices = c('',country_name_list))
    surveyPrev_ind_list <- full_ind_des

    }

    observeEvent(CountryInfo$WHO_version(),{

      if(is.null(CountryInfo$WHO_version())){return(NULL)}


      ### Internal version, all countries
      if(!CountryInfo$WHO_version()){
        country_name_list <- sort(DHS.country.meta[['CountryName']])
        updateSelectInput(inputId = "country", choices = c('',country_name_list))
      }

      ### WHO version app, subset of countries
      if(CountryInfo$WHO_version()){
        country_name_list <- WHO.app.countries
        updateSelectInput(inputId = "country", choices = c('',country_name_list))
      }


    })





    # update indicator group
    surveyPrev_ind_list <- full_ind_des
    updateSelectInput(inputId = "Svy_ind_group", choices = sort(unique(full_ind_des$Topic),decreasing = F))


    ### preload Zambia
    observeEvent(CountryInfo$use_preloaded_Zambia(),{

      if(CountryInfo$use_preloaded_Zambia()){
        freezeReactiveValue(input, "country")
        updateSelectInput(inputId = "country", choices = c('Zambia'))

        CountryInfo$country('Zambia')
        CountryInfo$svyYear_list('2018')
        country_GADM <- zmb.ex.GADM.list
        CountryInfo$GADM_list(country_GADM)
        CountryInfo$GADM_display_selected(country_GADM[['National']])
        freezeReactiveValue(input, "Svy_year")
        updateSelectInput(inputId = "Svy_year", choices = sort(CountryInfo$svyYear_list(),decreasing = T))
      }else{return()}

    })


    observeEvent(CountryInfo$use_preloaded_Madagascar(),{

      #message('loading MDG')
      if(CountryInfo$use_preloaded_Madagascar()){
        freezeReactiveValue(input, "country")
        updateSelectInput(inputId = "country", choices = c('Madagascar'))

        CountryInfo$GADM_strata_level(2)
        CountryInfo$country('Madagascar')
        CountryInfo$svyYear_list('2021')

        country_iso3 <- DHS.country.meta[DHS.country.meta$CountryName== CountryInfo$country(),'ISO3_CountryCode']
        country_GADM <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_analysis.rds'))
        country_GADM_smoothed <- readRDS(file=paste0('data/GADM_shp/',country_iso3,'/',country_iso3,'_GADM_display.rds'))

        CountryInfo$GADM_list(country_GADM)
        CountryInfo$GADM_list_smoothed(country_GADM_smoothed)

        CountryInfo$GADM_display_selected(country_GADM[['National']])
        freezeReactiveValue(input, "Svy_year")
        updateSelectInput(inputId = "Svy_year", choices = sort(CountryInfo$svyYear_list(),decreasing = T))
      }else{return()}

    })


    ### update country specific information once a country has been selected
    CountryInfo$country('')

    observeEvent(input$country, {

      if(is.null(input$country) || input$country == ""){return()}

      if(CountryInfo$use_preloaded_Zambia()){return()}
      if(CountryInfo$use_preloaded_Madagascar()){return()}

      if (input$country == CountryInfo$country()) {return()}

      freezeReactiveValue(input, "Svy_year")


      if (!all(sapply(CountryInfo$svy_dat_list(), is.null))| !is.null(CountryInfo$svy_GPS_dat()) ) {
      #if (input$country != CountryInfo$country()) {


        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId ="change_country_confirm",
          text = HTML(paste0("<p> Are you sure you want to switch to another country/survey? <br><br>",
                             "Uploaded data, fitted models and results for <br>",
                             "<div style='background-color: #D0E4F7; padding: 10px; font-size: large;margin-top:15px;margin-bottom:15px;'>",
                             "<strong> DHS ",CountryInfo$svyYear_selected(),' survey in ',CountryInfo$country(),"</strong> <br>",
                             "</div>",
                             "will all be ",
                             "<strong> <font color='red'> deleted</strong> </font>.</p>")),
          type = "warning",
          showCancelButton = TRUE,
          btn_labels = c("Cancel", "Confirm"),
          html=T
        )
      }else{

        ### Update country info
        CountryInfo$reset_val()
        AnalysisInfo$reset_results()

        freezeReactiveValue(input, "Svy_year")

        CountryInfo$country(input$country)
        CountryInfo$svyYear_selected('')

        CountryInfo$svyYear_list(get_survey_year(input$country))
        updateSelectInput(inputId = "Svy_year", choices = c('',sort(CountryInfo$svyYear_list(),decreasing = T)))



      }


    })

    observeEvent(input$change_country_confirm, {

      freezeReactiveValue(input, "Svy_year")

      if(CountryInfo$WHO_version()){
        country_name_list <- WHO.app.countries
      }else{
        country_name_list <- sort(DHS.country.meta[['CountryName']])
      }


      if ((input$change_country_confirm)) {
        # User confirmed the change

        ### Clear all existing data
        CountryInfo$reset_val()
        AnalysisInfo$reset_results()


        ### Update country info
        CountryInfo$country(input$country)
        message(paste0('changed to ',CountryInfo$country()))
        updateSelectInput(inputId = "country", selected = CountryInfo$country(),
                          choices = c('',country_name_list))
        #updateSelectInput(inputId = "Svy_ind_group", choices = sort(unique(full_ind_des$Topic),decreasing = F))




        ### Update country info
        CountryInfo$svyYear_list(get_survey_year(input$country))
        CountryInfo$svyYear_selected('')
        updateSelectInput(inputId = "Svy_year", choices = c('',sort(CountryInfo$svyYear_list(),decreasing = T)))


      } else {
        # User did not confirm, reset the selectInput to the last valid value
        updateSelectInput(inputId = "country", selected = CountryInfo$country(),
                          choices = c('',country_name_list))
      }
    })



    ###############################################################
    ### update survey year selection, add confirmation
    ###############################################################

    ### update survey year selection
    CountryInfo$svyYear_selected('')

    observeEvent(input$Svy_year, {

      if(is.null(input$Svy_year) || input$Svy_year == ""){return()}

      if (input$Svy_year == CountryInfo$svyYear_selected()) {return()}


      if (!all(sapply(CountryInfo$svy_dat_list(), is.null))| !is.null(CountryInfo$svy_GPS_dat()) ) {
        #if (input$country != CountryInfo$country()) {

        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId ="change_svy_yr_confirm",
          text = HTML(paste0("<p> Are you sure you want to switch to another country/survey? <br><br>",
                             "Uploaded data, fitted models and results for <br>",
                             "<div style='background-color: #D0E4F7; padding: 10px; font-size: large;margin-top:15px;margin-bottom:15px;'>",
                             "<strong> DHS ",CountryInfo$svyYear_selected(),' survey in ',CountryInfo$country(),"</strong> <br>",
                             "</div>",
                             "will all be ",
                             "<strong> <font color='red'> deleted</strong> </font>.</p>")),
          type = "warning",
          showCancelButton = TRUE,
          btn_labels = c("Cancel", "Confirm"),
          html=T
        )
      }else{

        ### Update survey info
        CountryInfo$svyYear_selected(input$Svy_year)

        ### get shapefiles
        ### show a spinner for waiting
        session$sendCustomMessage('controlSpinner', list(action = "show", message = "Loading country and survey specific shapefile, please wait..."))

        if(!CountryInfo$WHO_version()){
          country_shapefile <- get_country_shapefile(country=input$country,source=NULL)
        }else{
          country_shapefile <- get_country_shapefile(country=input$country,source='WHO')
        }

        CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
        CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

        CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


        Sys.sleep(1)

        session$sendCustomMessage('controlSpinner', list(action = "hide"))


      }



    })

    observeEvent(input$change_svy_yr_confirm, {

      if ((input$change_svy_yr_confirm)) {
        # User confirmed the change

        ### Clear all existing data
        CountryInfo$reset_val()
        AnalysisInfo$reset_results()


        ### Update country info
        CountryInfo$country(input$country)
        CountryInfo$svyYear_selected(input$Svy_year)
        CountryInfo$svyYear_list(get_survey_year(input$country))

        message(paste0('changed to survey ',CountryInfo$svyYear_selected()))

        updateSelectInput(inputId = "Svy_year", choices = c('',sort(CountryInfo$svyYear_list(),decreasing = T)),
                          selected=CountryInfo$svyYear_selected())

        ### get shapefiles
        ### show a spinner for waiting
        session$sendCustomMessage('controlSpinner', list(action = "show", message = "Loading country and survey specific shapefile, please wait..."))

        if(!CountryInfo$WHO_version()){
          country_shapefile <- get_country_shapefile(country=input$country,source=NULL)
        }else{
          country_shapefile <- get_country_shapefile(country=input$country,source='WHO')
        }

        CountryInfo$GADM_list(country_shapefile$country_shp_analysis)
        CountryInfo$GADM_list_smoothed(country_shapefile$country_shp_smoothed)

        CountryInfo$GADM_display_selected(country_shapefile$country_shp_smoothed[['National']])


        Sys.sleep(1)

        session$sendCustomMessage('controlSpinner', list(action = "hide"))



      } else {
        # User did not confirm, reset the selectInput to the last valid value
        updateSelectInput(inputId = "Svy_year", choices = c('',sort(CountryInfo$svyYear_list(),decreasing = T)),
                          selected=CountryInfo$svyYear_selected())
      }
    })


    ###############################################################
    ### update survey group selection
    ###############################################################

    ### update available indicators based on selection of indicator group

    ind_choice_vec <- reactiveVal('')

    current_svy_ind_group_selection <- reactiveVal('')

    observeEvent(input$Svy_ind_group, {
      freezeReactiveValue(input, "Svy_indicator")

      if (input$Svy_ind_group == current_svy_ind_group_selection()) {return()}

      if (!is.null(AnalysisInfo$model_screen_list())) {
        #if (input$Svy_ind_group != current_svy_ind_group_selection()) {

        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId ="change_svy_ind_group_confirm",
          text = HTML(paste0("<p> Are you sure you want to switch to another indicator? <br><br>",
                             "Fitted models and results for <br>",
                             "<div style='background-color: #D0E4F7; padding: 10px; font-size: large;margin-top:15px;margin-bottom:15px;'>",
                             "<strong>",(surveyPrev_ind_list[surveyPrev_ind_list$ID==current_svy_ind_selection(),]$Description),"</strong> <br>",
                             "</div>",
                             "will be ",
                             "<strong> <font color='red'> deleted</strong> </font>.</p>")),
          type = "warning",
          showCancelButton = TRUE,
          btn_labels = c("Cancel", "Confirm"),
          html=T
        )
      }else{

        current_svy_ind_group_selection(input$Svy_ind_group)  # Update the valid selection to the new value

        group_ind_list <- full_ind_des %>%
          subset( Topic==input$Svy_ind_group)

        indicator_choices_vector <- setNames(group_ind_list$ID, group_ind_list$Description)

        ind_choice_vec(indicator_choices_vector)

        shinyWidgets::updatePickerInput(inputId = "Svy_indicator", choices = sort(indicator_choices_vector,decreasing = F))

      }

    })


    observeEvent(input$change_svy_ind_group_confirm, {

      if ((input$change_svy_ind_group_confirm)) {
        # User confirmed the change
        current_svy_ind_group_selection(input$Svy_ind_group)  # Update the valid selection to the new value
        message(paste0('changed to ',current_svy_ind_group_selection()))
        updateSelectInput(session, "Svy_ind_group", selected = current_svy_ind_group_selection(),
                          choices =sort(unique(full_ind_des$Topic),decreasing = F))

        AnalysisInfo$reset_results()


        group_ind_list <- full_ind_des %>%
          subset( Topic==input$Svy_ind_group)

        indicator_choices_vector <- setNames(group_ind_list$ID, group_ind_list$Description)
        ind_choice_vec(indicator_choices_vector)

        shinyWidgets::updatePickerInput(inputId = "Svy_indicator", choices = sort(indicator_choices_vector,decreasing = F))


      } else {
        # User did not confirm, reset the selectInput to the last valid value
        updateSelectInput(session, "Svy_ind_group", selected = current_svy_ind_group_selection(),
                          choices =sort(unique(full_ind_des$Topic),decreasing = F))
      }
    })




    ###############################################################
    ### update survey indicator selection based on group
    ###############################################################

    ### update survey indicator selection
    ### also prompt the user to confirm if analysis already done

    current_svy_ind_selection <- reactiveVal('')

    observeEvent(input$Svy_indicator, {

      if(is.null(input$Svy_indicator) || input$Svy_indicator == ""){return()}

      if (input$Svy_indicator == current_svy_ind_selection()) {return()}

      ### if fitted models, ask for confirmation
      if (!is.null(AnalysisInfo$model_screen_list())) {
        #if (input$Svy_indicator != current_svy_ind_selection()) {

        new_ind_des <- (surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description)
        prev_ind_des <- (surveyPrev_ind_list[surveyPrev_ind_list$ID==current_svy_ind_selection(),]$Description)

        shinyWidgets::confirmSweetAlert(
          session = session,
          inputId ="change_svy_ind_confirm",
          text = HTML(paste0("<p> Are you sure you want to change the indicator to <br>",
                             "<div style='background-color: #D0E4F7; padding: 10px; font-size: large;margin-bottom:15px;'>",
                             "<strong>",new_ind_des, "</strong>? <br>",
                             "</div>",
                             "Fitted models and results for <br>",
                             "<div style='background-color: #D0E4F7; padding: 10px; font-size: large;margin-top:15px;margin-bottom:15px;'>",
                             "<strong>",prev_ind_des,"</strong> <br>",
                             "</div>",
                             "will be ",
                             "<strong> <font color='red'> deleted</strong> </font>.</p>")),
          type = "warning",
          showCancelButton = TRUE,
          btn_labels = c("Cancel", "Confirm"),
          html=T
        )
      }else{

        CountryInfo$svy_indicator_var(input$Svy_indicator)
        CountryInfo$svy_indicator_des(surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description)

        current_svy_ind_selection(input$Svy_indicator)
      }

    })


    observeEvent(input$change_svy_ind_confirm, {

      if ((input$change_svy_ind_confirm)) {
        # User confirmed the change
        current_svy_ind_selection(input$Svy_indicator)  # Update the valid selection to the new value
        message(paste0('changed to ',current_svy_ind_selection()))
        shinyWidgets::updatePickerInput(session, "Svy_indicator", selected = current_svy_ind_selection(),choices =ind_choice_vec())

        AnalysisInfo$reset_results()
        CountryInfo$svy_indicator_var(input$Svy_indicator)
        CountryInfo$svy_indicator_des(surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description)

      } else {
        # User did not confirm, reset the selectInput to the last valid value
        shinyWidgets::updatePickerInput(session, "Svy_indicator", selected = current_svy_ind_selection(),choices =ind_choice_vec())
      }
    })




    ### prompt when recode are not all available for the selected combination
    observe({
      req(input$country)
      req(input$Svy_year)
      req(input$Svy_indicator)


      recode.avail <- check_dat_avail(country = input$country , svy_year = input$Svy_year , indicator =input$Svy_indicator)

      if(length(recode.avail$missing_recode)>0){
        showNoRecodeModal(recode=recode.avail$missing_recode,
                          Svy_indicator=CountryInfo$svy_indicator_des())
      }


    })



    ### update admin level visualization

    observe({
      req(CountryInfo$GADM_list())
      updateSelectInput(session, "AdminLevel", choices = names(CountryInfo$GADM_list()))
      updateCheckboxGroupInput(session, "admin_levels_analysis", choices = names(CountryInfo$GADM_list()),
                               selected = names(CountryInfo$GADM_list()))
      #updateSelectInput(session, "admin_levels_analysis", selected = 'National')

    })


    ### make sure national is always selected
    observe({
      selected <- input$admin_levels_analysis
      if (is.null(selected) || !("National" %in% selected)) {
        selected <- c("National", selected)
        updateCheckboxGroupInput(session, "admin_levels_analysis", selected = selected)
      }
    })

    ### display: update GADM files based on selection of admin level

    observeEvent(input$AdminLevel, {

      if(is.null(input$AdminLevel) || input$AdminLevel == ""){return()
      }else{

        CountryInfo$GADM_display_selected(CountryInfo$GADM_list_smoothed()[[input$AdminLevel]])
        CountryInfo$GADM_display_selected_level(input$AdminLevel)
      }
    })

    ### analysis: update GADM files based on selection of admin level

    observeEvent(input$admin_levels_analysis, {

      if(is.null(input$admin_levels_analysis) || length(input$admin_levels_analysis) == 0){return()
      }else{

        CountryInfo$GADM_analysis_levels(input$admin_levels_analysis)
      }
    })


    ### text regarding meta information on country, survey etc.

    output$country_meta_display <- renderUI({
      req(CountryInfo$country())
      req(CountryInfo$svyYear_selected())

      country <- CountryInfo$country()
      svy_year <- CountryInfo$svyYear_selected()
      admin_level <- CountryInfo$GADM_display_selected_level()
      #indicator_description <- surveyPrev_ind_list[surveyPrev_ind_list$ID==input$Svy_indicator,]$Description


      HTML(paste0(
        "<p style='font-size: large;'>",
        "You've selected ",
        "<strong style='background-color: #D0E4F7;'>", country, "</strong>",
        " with survey in ",
        "<strong style='background-color: #D0E4F7;'>", svy_year, "</strong>",
        ", to estimate ",
        "<br> <strong style='background-color: #D0E4F7;'>", CountryInfo$svy_indicator_des(), "</strong>.",
        "<br> You intend to conduct analysis at ",
        "<strong style='background-color: #D0E4F7;'>", concatenate_vector_with_and(CountryInfo$GADM_analysis_levels()), "</strong>",
        " level(s).",
        "<br><br>",
        "Please review the table and map below for your Admin level selections. ",
        "Choose a different level to display if necessary.",
        "</p>",
        "<hr style='border-top-color: #E0E0E0;'>"
      ))


    })

    ### present number of regions at each admin level

    output$gadmTable <- renderTable({
      req(CountryInfo$country())
      req(CountryInfo$svyYear_selected())

      gadm_list <- CountryInfo$GADM_list()
      if (is.null(gadm_list) || length(gadm_list) == 0) {

        GADM_num_df <-check_gadm_levels(NULL)
        return(GADM_num_df) # Do not render the table if no data is supplied
      }

      GADM_num_df <- check_gadm_levels(gadm_list)
    }, align = "l",rownames = TRUE)


    ### text display above the map

    output$text_admin_display <- renderUI({
      req(CountryInfo$country())
      req(CountryInfo$svyYear_selected())

      req(CountryInfo$GADM_display_selected_level())

      country <- CountryInfo$country()
      admin_level <- CountryInfo$GADM_display_selected_level()

      HTML(paste0(
        "<hr style='border-top-color: #E0E0E0;'>",
        "<p style='font-size: large;'>",
        "The map below displays ",
        "<span style='background-color: #D0E4F7;'><strong>", admin_level,
        "</strong></span> boundaries of ",
        "<span style='background-color: #D0E4F7;'><strong>", country,
        "</strong></span>. ",
        "</p>"
      ))

    })


    ###############################################################
    ### country boundaries map
    ###############################################################

    observeEvent(input$mapType,{


      CountryInfo$display_interactive(input$mapType)

    })

    observeEvent(CountryInfo$display_interactive(),{

      interactive_map <- CountryInfo$display_interactive()
      shinyWidgets::updateMaterialSwitch(session=session, inputId="mapType", value = interactive_map)

    })



    ### determine interactive/static map for selected country
    output$mapUI <- renderUI({
      if (input$mapType) {  # if TRUE, show interactive map
        leaflet::leafletOutput(ns("interactive_country_map"))
      } else {  # if FALSE, show static map
        plotOutput(ns("static_country_map"))
      }
    })

    ### interactive map
    output$interactive_country_map <- leaflet::renderLeaflet({

      req(CountryInfo$svyYear_selected())

      req(CountryInfo$GADM_display_selected_level())
      req(CountryInfo$GADM_list())


      #gadm_list <- CountryInfo$GADM_list()

      selected_level <- CountryInfo$GADM_display_selected_level()
      gadmData <-  CountryInfo$GADM_list()[[selected_level]]

      #if(is.null(selected_level)){selected_level='National'}

      #message('map is rendering')
      if (is.null(gadmData)) {

        # If no country is selected, return an empty Leaflet map
        leaf_plot <- leaflet::leaflet()

        if(CountryInfo$use_basemap()=='OSM'){
          leaf_plot <- leaf_plot %>% leaflet::addTiles()
        }

      } else {

        leaf_plot<- country.boundary.leaflet(gadm.level= selected_level,
                                             gadmData=gadmData,
                                             use.basemap=CountryInfo$use_basemap())
        #leaf_plot <- leaflet::leaflet(gadmData) %>%
        #leaflet::addTiles() %>%
        #leaflet::addPolygons(weight = 1)
      }

      return(leaf_plot)

    })


    ### static map

    output$static_country_map <- renderPlot({

      req(CountryInfo$svyYear_selected())

      req(CountryInfo$GADM_display_selected_level())
      req(CountryInfo$GADM_list())


      #gadm_list <- CountryInfo$GADM_list()

      selected_level <- CountryInfo$GADM_display_selected_level()
      gadmData <-  CountryInfo$GADM_list()[[selected_level]]

      if (is.null(gadmData)) {

        # If no country is selected, return nothing

        return(NULL)

      } else {

        map_plot <- ggplot2::ggplot() +
          #ggspatial::annotation_map_tile(type = "osm",zoom=0) +
          ggplot2::geom_sf(data = gadmData, color = "#00008B", size = 2) +
          ggplot2::theme_bw()
      }

      return(map_plot)
    })

  })
}

## To be copied in the UI
# mod_country_specify_ui("country_specify_1")

## To be copied in the server
# mod_country_specify_server("country_specify_1")
