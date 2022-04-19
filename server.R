

server <- function(input, output, session) {
  
  # Close session after closing app --------------------------
  
  session$onSessionEnded(stopApp) # commenting out to test using lighthouse
  
  # Links to tabs --------------------------------------------
  
  observeEvent(input$link_to_industryOverview_tab, {
    updateTabsetPanel(session, "navbar", selected = "overview")
  })
  
  observeEvent(input$link_to_regional_tab, {
    updateTabsetPanel(session, "navbar", selected = "regional")
  })
  
  
  # Page1: Reactive chart qualification level ------------------------------------------------------------------
  
  selection <- reactive({
    stat_hq %>%
      filter(Region == input$region &
               Sector == input$sector) 
  })
  
  numberRows <- reactive({
    selection() %>% nrow()
  })
  
  highestQualVolMedianChart <- reactive({
    if(input$showMedian == 'No'){
      selection() %>%
        ggplot(aes(x = Level_order,
                   text = paste(paste0("Percentage: ", formatC(perc_hq*100,
                                                               digits = 0,
                                                               format = "f"), "%"), 
                                paste0("Volume: ", formatC(number_students_hq, 
                                                           digits = 0, 
                                                           format = "f", 
                                                           big.mark = ",")), 
                                paste0("Apprentices: ", formatC(number_students_app,
                                                                digits = 0,
                                                                format = "f",
                                                                big.mark = ",")),
                                sep = "\n"))) + 
        geom_col(aes(y = perc_hq), 
                 fill = c("#f3f2f1","#7FCFF2","#62B7E4","#489FD6","#3088C8","#1D70B8"))  +
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(labels = levelsRelabelled) +
        labs(y = "", x = "",
             title = "") +
        theme(legend.position="none",
              plot.title = element_text((hjust = 0.5)),
              axis.text.y = element_text(face = "bold", color = "#0b0c0c",
                                         size = 12, angle = 0),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
        )  + 
        coord_flip()
    } else {
      stat_hq %>% 
        filter(Region == input$region & 
                 Sector == input$sector) %>%
        ggplot(aes(x = Level_order,
                   text = paste(paste0("Percentage: ", formatC(perc_hq*100,
                                                               digits = 0,
                                                               format = "f"), "%"), 
                                paste0("Volume: ", formatC(number_students_hq, 
                                                           digits = 0, 
                                                           format = "f", 
                                                           big.mark = ",")), 
                                paste0("Apprentices: ", formatC(number_students_app,
                                                                digits = 0,
                                                                format = "f",
                                                                big.mark = ",")),
                                sep = "\n"))) + 
        geom_point(aes(y = median_income),
                   size = 1,
                   shape = 21,
                   color = "#4c2c92",
                   fill = "#4c2c92") +
        geom_text(data = selection(), 
                  aes(x = Level_order, 
                      y = median_income,
                      label = paste0("£", formatC(signif(median_income, digits = 2)/1000, 
                                                  digits = 0, 
                                                  format = "f", 
                                                  big.mark = ","), "K")), 
                  nudge_x = 0.25,
                  nudge_y = -0.5,
                  colour = "#0b0c0c", 
                  size = 3,
                  check_overlap = TRUE) +
        geom_segment(aes(x = Level_order, 
                         xend = Level_order, 
                         y = 0, 
                         yend = median_income),
                     size = 0.5, 
                     color = "#4c2c92") +
        labs(y = "Average Earnings (£)", x = "",
             title = "") +
        theme(legend.position="none",
              plot.title = element_text((hjust = 0.5)),
              axis.text.y = element_text(face="bold", color="#0b0c0c",
                                         size=12, angle = 0),
              axis.text.x = element_blank(),  
              axis.ticks.x = element_blank(),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(labels = comma_format(big.mark = ",") ) +
        scale_x_discrete(labels = levelsRelabelled) +
        coord_flip()               
    }
    
  })
  
  
  # Page1: Reactive chart sub-sector ------------------------------------------------------------------
  
  selection_subs <- reactive({
    stat_subs %>%
      filter(Region == input$region &
               Sector == input$sector) 
  })
  
  numberRows_subs <- reactive({
    selection_subs() %>% nrow()
  })
  
  subsVolMedianChart <- reactive({
    if(input$showMedian == 'No'){ 
      selection_subs() %>%
        ggplot(aes(x = Subsector, 
                   text = paste(paste0("Percentage: ", formatC(perc_subs*100,
                                                               digits = 0,
                                                               format = "f"), "%"), 
                                paste0("Volume: ", formatC(number_students_subs, 
                                                           digits = 0, 
                                                           format = "f", 
                                                           big.mark = ",")), 
                                paste0("Apprentices: ", formatC(number_students_app,
                                                                digits = 0,
                                                                format = "f",
                                                                big.mark = ",")),
                                sep = "\n"))) +
        geom_col(aes(y = perc_subs), 
                 fill = "#28a197")  +
        labs(y = "", x = "",
             title = "") +
        theme(legend.position="none", 
              plot.title = element_text((hjust = 0.5)),
              axis.text.y = element_text(face="bold", color="#0b0c0c",
                                         size=12, angle=0),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip() 
    } else {
      selection_subs() %>%
        ggplot(aes(x = Subsector,
                   text = paste(paste0("Percentage: ", formatC(perc_subs*100,
                                                               digits = 0,
                                                               format = "f"), "%"), 
                                paste0("Volume: ", formatC(number_students_subs, 
                                                           digits = 0, 
                                                           format = "f", 
                                                           big.mark = ",")), 
                                paste0("Apprentices: ", formatC(number_students_app,
                                                                digits = 0,
                                                                format = "f",
                                                                big.mark = ",")),
                                
                                sep = "\n"))) +
        geom_count(aes(y = median_income),
                   size = 1,
                   shape = 21,
                   color = "#4c2c92",
                   fill = "#4c2c92") + 
        geom_text(data = selection_subs(), 
                  aes(x = Subsector, y = median_income,
                      label = paste0("£", formatC(signif(median_income/1000, digits = 2), 
                                                  digits = 0, 
                                                  format = "f", 
                                                  big.mark = ","), "K")),
                  nudge_x = 0.25,
                  nudge_y = -0.5,
                  size = 3,
                  colour="#0b0c0c", 
                  check_overlap = TRUE) +
        geom_segment(aes(x = Subsector, xend = Subsector, 
                         y = 0, 
                         yend = median_income),
                     size = 0.5, color = "#4c2c92") +
        labs(y = "Average Earnings (£)", x = "",
             title = "") +
        theme(legend.position="none",
              plot.title = element_text((hjust = 0.5)),
              axis.text.y = element_text(face="bold", color="#0b0c0c",
                                         size=12, angle=0),
              axis.text.x = element_blank(),  
              axis.ticks.x = element_blank(),panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())  + 
        scale_y_continuous(labels = comma_format(big.mark = ",") )+
        coord_flip()
      
    }
  })
  
  # Page1: Reactive chart subject ------------------------------------------------------
  
  # choices in select input
  sect_sub_v <-  reactive({
    stat_subs_sub %>%
      filter(Region == input$region &
               Sector == input$sector) %>%
      distinct(Subsector, .keep_all = FALSE) %>%
      unlist(use.names = F)
  })
  
  hq_sub_v <-  reactive({
    stat_hq_sub %>%
      filter(Region == input$region &
               Sector == input$sector) %>%
      distinct(Level_order, .keep_all = FALSE) %>%
      unlist(use.names = F)
  })
  
  
  
  observe({  
    updateSelectInput(session, 
                      "inSelect2",
                      choices = subsector_v, 
                      selected = "All subsectors")
    
    updateSelectInput(session, 
                      "inSelect",
                      choices = levelsRelabelled, 
                      selected = "All levels")
  })
  
  indSubChart <- reactive({
    stat_subs_sub %>%
      filter(Region == input$region &
               Sector == input$sector &
               Subsector == input$inSelect2 &
               Level_order == input$inSelect) %>%
      ggplot(aes(x = reorder(Subject, -perc), y = perc,
                 text = paste(
                   paste0("Percentage: ", formatC(perc*100,
                                                  digits = 0,
                                                  format = "f"), "%"),
                   paste0("Volume: ", formatC(number_students_sub, 
                                              digits = 0, 
                                              format = "f", 
                                              big.mark = ",")),
                   paste0("Average Earnings: ", "£", formatC(signif(median_income/1000, digits = 2), 
                                                             digits = 0, 
                                                             format = "f", 
                                                             big.mark = ","), "K"),
                   sep = "\n"))) +
      geom_col(fill = "#003078")  +
      labs(title = "", x = "", y = "") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      theme(plot.title = element_text((hjust = 0.5)),
            plot.margin = unit(c(0,0,0,0), "mm"),
            axis.text.y = element_text(face = "bold", 
                                       color="#0b0c0c", 
                                       size = 12, 
                                       angle = 0),
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()
      ) 
    
  })
  
  # Page2: Reactive stack chart InWork % --------------------------------------
  
  observe({  
    updateSelectInput(session, 
                      "inSelect3",
                      choices = level_v2, 
                      selected = "Level 2")                   
  })
  
  level_reac <- reactive({
    students_in_work %>% 
      filter(Region == input$regionp &
               Sector == input$sectorp & 
               Level_order == input$inSelect3) %>%
      distinct(new_level, .keep_all = FALSE) %>%
      unlist(use.names = F) 
  })
  
  # title for stacked chart page 2 -- gives the level
  inWorkChartTitle <- reactive({
    s <- students_in_work %>% 
      filter(Region == input$regionp &
               Sector == input$sectorp & 
               Level_order == input$inSelect3) %>%
      distinct(Level_order, .keep_all = FALSE) %>%
      unlist(use.names = F) 
    s <- tolower(s[1])
  })
  
  inWorkChart <- reactive({
    students_in_work %>% 
      mutate(var_to_show = case_when(new_level == level_reac() ~ paste("At", inWorkChartTitle()), 
                                     new_level < level_reac() ~ paste("Lower than", inWorkChartTitle()), 
                                     TRUE ~ paste("Higher than", inWorkChartTitle())),
             var_to_plot = case_when(new_level == level_reac() ~ "At level", 
                                     new_level < level_reac() ~ "Lower than level", 
                                     TRUE ~ "Higher than level")) %>%
      mutate(var_to_plot = factor(var_to_plot, levels = c("Higher than level", "At level", "Lower than level"), ordered = T)) %>%
      # calculate percentages
      group_by(Region, Sector, var_to_plot, var_to_show) %>%
      summarise(students = sum(number_students)) %>%
      ungroup() %>% 
      left_join(students_in_work %>% 
                  group_by(Region, Sector) %>%
                  summarise(total = sum(number_students)) %>%
                  ungroup() , 
                by = c("Region", "Sector")) %>%
      mutate(perc = students/total) %>%
      filter(Region == input$regionp &
               Sector == input$sectorp ) %>%
      ggplot(aes(fill = var_to_plot, 
                 y = perc, 
                 x = Sector, 
                 text = paste0(var_to_show, ": ", formatC(perc*100,
                                                          digits = 0,
                                                          format = "f"), "%"))) +
      geom_bar(position = "stack", stat = "identity") +
      labs(title = NULL, 
           x = "", 
           y = "") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("#1D70B8","#7FCFF2", "#f3f2f1")) +
      theme(legend.position = "none", 
            plot.margin = unit(c(0,0,0,0), "mm"),
            axis.title=element_text(size = 14),
            plot.title = element_text(hjust = 0),
            axis.text.x = element_text(face = "bold", 
                                       color="#0b0c0c", 
                                       size = 12, 
                                       angle = 0),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())   
  })
  
  
  # select percentage of people in work
  
  percInWork <- reactive({
    students_in_work %>% 
      mutate(var_to_show = case_when(new_level == level_reac() ~ paste("At", inWorkChartTitle()), 
                                     new_level < level_reac() ~ paste("Lower than", inWorkChartTitle()), 
                                     TRUE ~ paste("Higher than", inWorkChartTitle())),
             var_to_plot = case_when(new_level == level_reac() ~ "At level", 
                                     new_level < level_reac() ~ "Lower than level", 
                                     TRUE ~ "Higher than level")) %>%
      mutate(var_to_plot = factor(var_to_plot, levels = c("Higher than level", "At level", "Lower than level"), ordered = T)) %>%
      # calculate percentages
      group_by(Region, Sector, var_to_plot, var_to_show) %>%
      summarise(students = sum(number_students)) %>%
      ungroup() %>% 
      left_join(students_in_work %>% 
                  group_by(Region, Sector) %>%
                  summarise(total = sum(number_students)) %>%
                  ungroup() , 
                by = c("Region", "Sector")) %>%
      mutate(perc = students/total) %>%
      filter(Region == input$regionp &
               Sector == input$sectorp ) %>%
      select(perc)
  })
  
  
  # Render Charts ------------------------------------------------------------------
  
  output$highestQualVolMedianChart <- renderPlotly({
    ggplotly(highestQualVolMedianChart(), tooltip = "text")
  })
  
  output$subsVolMedianChart <- renderPlotly({
    ggplotly(subsVolMedianChart(), tooltip = c("text"))
  })
  
  
  output$indSubChart <- renderPlotly({
    ggplotly(indSubChart(), tooltip = "text")
  })
  
  
  output$studinWorkChart <- renderPlotly({
    ggplotly(inWorkChart(), tooltip = "text")
  })
  
  
  # Page1: Render top quals table ------------------------------------------------------
  
  observe({  
    updateSelectInput(session, 
                      "inSelect",
                      choices = hq_sub_v(), 
                      selected = "All levels")
    
    updateSelectInput(session, 
                      "inSelect2",
                      choices = sect_sub_v(), 
                      selected = "All subsectors")
  })
  
  output$hqSubTable <- renderDataTable({
    
    DT::datatable(stat_hq_sub %>% 
                    filter(Region == input$region &
                             Sector == input$sector &
                             Level_order == input$inSelect &
                             Subsector == input$inSelect2) %>%
                    mutate(median_income = signif(median_income, 2)) %>%
                    # final selection 
                    select(Qualification, 
                           Subject, 
                           Percentage = perc, 
                           "Average Earnings" = median_income),
                  rownames = FALSE,
                  options = list(searching = FALSE,
                                 pageLength = 10,
                                 lengthMenu = c(10,20), 
                                 searchHighlight = TRUE,
                                 dom = 'p',
                                 scrollX = TRUE),
                  width = '625px',
                  height = '400px' ,
                  style = 'bootstrap', class = 'table-bordered table-condensed align-center') %>%
      formatPercentage(3, digits = 1) %>%
      formatCurrency(4, digits = 0, currency = "£", mark = ",") %>%
      formatStyle(0, target = 'row',
                  color = 'black',
                  fontSize = '16px',
                  backgroundColor = 'white',
                  lineHeight='100%')
    
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, 
                      "inSelect",
                      choices = hq_sub_v(), 
                      selected = "All levels")
    
    updateSelectInput(session, 
                      "inSelect2",
                      choices = sect_sub_v(), 
                      selected = "All subsectors")
    
  }) 
  
  
  # Page1: Render KPIS --------------------------------------------------------------------    
  
  # percentage of people in sector
  output$perc_in_sector <- renderUI({
    
    perc  <- kpis %>%
      filter(Sector == input$sector, 
             Region == input$region) %>%
      select(perc_students_sector)
    perc <- round(perc, digits = 2) 
    
    tags$b(paste0(perc[[1]]*100, "%"), 
           style = "font-size:40px; text-align:center; color:	#ffffff")
  })
  
  # median income
  output$median_in_sector <- renderUI({
    
    median_inc <- kpis %>%
      filter(Sector == input$sector, 
             Region == input$region) %>%
      select(median_income_sector) 
    median_inc <- signif(median_inc[[1]], digits = 2) 
    median_inc <- prettyNum(median_inc, big.mark = ",")
    
    tags$b(paste0("£", median_inc), 
           style = "font-size:40px; text-align:center; color:	#ffffff")
  })
  
  # direction of change
  output$directionSector<- renderUI({
    wf <- wf %>%
      filter(Sector == input$sector,
             Region == input$region) %>%
      select(direction = Years2022.2027)
    tags$b(paste(round(wf$direction[[1]], digits = 1), "%"), 
           style = "font-size:40px; text-align:center; color:	#ffffff")
    
  })
  
  
  # Page2: Reactive treeplot ------------------------------------------------------
  
  # filter region/sector
  selected_region_sector <- reactive({
    qualifications %>%
      filter(Region == input$regionp & 
               IndustrySector == input$sectorp) %>%
      select(Region, 
             IndustrySector, 
             Level, 
             Qual, 
             NextQual,
             LevelNextQual,
             Links, perc_qual) %>%
      mutate(ColourLevel = case_when(Level == "Level 2" ~ "#1d70b8",
                                     Level == "Level 3" ~ "#003078",
                                     Level == "Level 4/5" ~ "#912b88",
                                     Level == "Level 6" ~ "#4c2c92",
                                     Level == "Level 7+"~ "#28a197",
                                     TRUE ~   "#ffffff"),
             ColourNextLevel = case_when(LevelNextQual == "Level  2" ~ "#1d70b8",
                                         LevelNextQual == "Level 3" ~ "#003078",
                                         LevelNextQual == "Level 4/5" ~ "#912b88",
                                         LevelNextQual == "Level 6" ~ "#4c2c92",
                                         LevelNextQual == "Level 7+"~ "#28a197",
                                         TRUE ~   "#ffffff"))
  })
  
  # choices in select input for level based on previous selections
  level_select_vect <-  reactive({
    selected_region_sector() %>% 
      distinct(Level, .keep_all = FALSE) %>%
      unlist(use.names = F)
  })
  
  
  # update selectizeInput
  observeEvent(input$reset, {
    updateSelectInput(session, 
                      "inSelect3",
                      choices = level_select_vect(), 
                      selected = "Level 2")
    
  })
  
  
  # filter level/subject and self-joins to get next quals on 4 levels
  tree_data <- reactive({
    selected_region_sector() %>%
      filter(Level == input$inSelect3) %>% 
      left_join(selected_region_sector(), by = c("Region", 
                                                 "IndustrySector",
                                                 "NextQual" = "Qual",
                                                 "LevelNextQual" = "Level", 
                                                 "ColourNextLevel" = "ColourLevel"), 
                na_matches = "na",
                suffix = c(".1", ".2")) %>% 
      left_join(selected_region_sector(), by = c("Region", 
                                                 "IndustrySector",
                                                 "NextQual.2" = "Qual",
                                                 "LevelNextQual.2" = "Level", 
                                                 "ColourNextLevel.2" = "ColourLevel"), 
                na_matches = "na",
                suffix = c("", ".3")) %>% 
      select(Qual, starts_with(c("NextQual", "Links", "Level", "Colour"))) %>%
      # replace nas with 0 in number of students
      mutate_at(vars(matches("Links")), ~replace(., is.na(.), 0)) %>%
      # replace nas for vector colors
      mutate_at(vars(matches("Colour")), ~na_if(., "#ffffff")) %>% 
      # restrict each next qual the number of nodes to top 5
      group_by(Qual, Level) %>%
      arrange(desc(Links.1), .by_group = T) %>%
      mutate(numbering = dplyr::row_number()) %>%
      filter(numbering <= 10) %>%
      ungroup()
  })
  
  # Make vectors for colors
  color1 <- reactive({tree_data() %>% 
      distinct(Qual, ColourLevel, .keep_all = F) })
  color2 <- reactive({tree_data() %>% 
      distinct(Qual, NextQual, ColourNextLevel, .keep_all = F) %>% 
      filter(!is.na(NextQual))})
  color3 <- reactive({tree_data() %>% 
      distinct(Qual, NextQual, NextQual.2, ColourNextLevel.2, .keep_all = F) %>% 
      filter(!is.na(NextQual) & !is.na(NextQual.2))})
  color4 <- reactive({tree_data() %>% 
      distinct(Qual, NextQual, NextQual.2, NextQual.3, ColourNextLevel.3, .keep_all = F) %>%
      filter(!is.na(NextQual) & !is.na(NextQual.2) & !is.na(NextQual.3))})
  
  
  # Page2: Render treeplot --------------------------------------------------
  
  output$treePlot <- renderCollapsibleTree({
    
    collapsibleTree(tree_data(), 
                    hierarchy = c("Qual", "NextQual", "NextQual.2", "NextQual.3"),
                    root = paste("Your selection:"),
                    zoomable = FALSE,
                    fontSize = 12, 
                    fill = c("#f3f2f1",
                             color1()$ColourLevel, 
                             color2()$ColourNextLevel, 
                             color3()$ColourNextLevel.2, 
                             color4()$ColourNextLevel.3),
                    fillByLevel = TRUE,
                    collapsed = TRUE,
                    tooltip = F ,
                    width = 1200
    )
    
  })
  
  
  
  # Page 2: Render top 3 subjects table --------------------------------------------------
  
  output$t3subjectsTable <- renderDataTable({
    
    DT::datatable(t3_subjects %>% 
                    filter(Region == input$regionp &
                             Sector == input$sectorp &
                             Level_order == input$inSelect3) %>%
                    mutate(perc = round(perc, 4)) %>%
                    select(Subject = Subject2, 
                           Percentage = perc),
                  rownames = NULL,
                  colnames = NULL,
                  options = list(searching = FALSE,
                                 pageLength = 3,
                                 dom = 't',
                                 bSort = F,
                                 scrollX = F),
                  width = '200px',
                  height = '100px',
                  style = 'bootstrap', class = 'stripe') %>%
      formatPercentage(2, digits = 1) %>%
      formatStyle(0, target = 'row',
                  color = '#ffffff',
                  fontSize = '16px',
                  backgroundColor = '#1d70b8',
                  lineHeight='60%')
    
  })
  
  # Page 1&2: reactive Box & KPIs titles --------------------------------------------------------------
  
  # page titles
  output$page1title <- renderUI({
    s <- selection() 
    if(s$Region[1] %in% c("London", "Yorkshire and The Humber")) {
      tags$b(paste0(s$Sector[1], " in ", s$Region[1],
                    ": overview of employee education levels and qualification choices"), 
             style = "font-size: 24px;")
    } else {
      tags$b(paste0(s$Sector[1], " in the ", s$Region[1], 
                    ": overview of employee education levels and qualification choices"),
             style = "font-size: 24px;")
    }
  }) 
  
  output$page2title <- renderUI({
    sr <- selected_region_sector() %>% 
      distinct(Region, .keep_all = FALSE) %>%
      unlist(use.names = F)
    sis <- selected_region_sector() %>% 
      distinct(IndustrySector, .keep_all = FALSE) %>%
      unlist(use.names = F)
    if(sr[[1]] %in% c("London", "Yorkshire and The Humber")) {
      tags$b(paste0(sis[[1]], " in ", sr[[1]], ": post-16 qualification pathways"),
             style = "font-size: 24px;")
    } else {
      tags$b(paste0(sis[[1]], " in the ", sr[[1]], ": post-16 qualification pathways"),
             style = "font-size: 24px;")
    }
    
  }) 
  
  # page 1: kpi percentage employees
  output$kpiSector <- renderUI({
    s <- selection() 
    
    tags$b(paste0("of employees work in ", tolower(s$Sector[1])), 
           style = "font-size: 18px; color: #ffffff")
    
  })
  
  # page 1: kpi growth
  output$kpiChange<- renderUI({
    wf <- wf %>%
      filter(Sector == input$sector,
             Region == input$region) %>%
      select(direction = Years2022.2027)
    changeS <- ifelse(wf$direction[[1]] >= 0, "growth", "decline")
    tags$b(paste0("forecast annual employment ", changeS), 
           style = "font-size: 18px; color: #ffffff")
  })
  
  # page 1: subsector/qualification level chart
  output$box2title <- renderText({
    if(input$showMedian == 'No') {
      paste0("Distribution of employees by industry sub-sector and highest level of education")
    } else {
      paste0("Employee earnings by industry sub-sector and highest level of education")
    }
  })
  
  # page 1: subject chart and qualification table
  output$box3title <- renderText({
    s <- selection() 
    ss <- stat_subs_sub %>% 
      filter(Region == input$region &
               Sector == input$sector &
               Subsector == input$inSelect2 &
               Level_order == input$inSelect) 
    ss[c('Level_order', 'Subsector')] <- sapply(ss[c('Level_order', 'Subsector')], function(x) tolower(x))
    HTML(
      paste0("Subject and qualification choices for employees at ", ss$Level_order[1], 
             " working in ", ss$Subsector[1]))
  })
  
  
  # page 2: title for stacked chart box
  output$box7title <- renderText({
    s <- selected_region_sector() %>%
      filter( Level == input$inSelect3)
    s[c('Level')] <- sapply(s[c('Level')], function(x) tolower(x))
    HTML(paste0(formatC(percInWork()$perc[2]*100,
                        digits = 0,
                        format = "f"), "% ",
                "of employees entered work with a ",
                inWorkChartTitle(), " qualification"))
  })
  
  # page 2: treeplot
  output$box4title <- renderText({
    s <- selected_region_sector() %>%
      filter( Level == input$inSelect3)
    s[c('Level')] <- sapply(s[c('Level')], function(x) tolower(x))
    HTML(paste("Post-16 qualification pathways starting at ", 
               s$Level[1], "expand the chart to view selected popular pathways to higher levels of education"))
  })
  
  
  
}

