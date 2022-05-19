theme_set(theme_hc(base_family = "Helvetica"))

defaultW <- getOption("warn")
options(warn = -1)

server <- function(input, output, session) {

  # Close session after closing app --------------------------

  session$onSessionEnded(stopApp) # commenting out to test using lighthouse

  # Links to tabs --------------------------------------------

  observeEvent(input$link_to_industryOverview_tab, {
    updateTabsetPanel(session, "navbar", selected = "overview")
  })

  observeEvent(input$link_to_qualificationPathways_tab, {
    updateTabsetPanel(session, "navbar", selected = "pathways")
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
    if (input$showMedian == "Percentage") {
      selection_subs() %>%
        ggplot(aes(
          x = Subsector,
          text = paste(paste0("Percentage: ", formatC(perc_subs * 100,
            digits = 1,
            format = "f"
          ), "%"),
          paste0("Volume: ", formatC(number_students_subs,
            digits = 0,
            format = "f",
            big.mark = ","
          )),
          sep = "\n"
          )
        )) +
        geom_col(aes(y = perc_subs),
          fill = "#28a197"
        ) +
        labs(
          y = "", x = "",
          title = ""
        ) +
        theme(
          legend.position = "none",
          plot.title = element_text((hjust <- 0.5)),
          axis.text.y = element_text(
            face = "bold", color = "#0b0c0c",
            size = 12, angle = 0
          ),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent) +
        coord_flip()
    } else {
      selection_subs() %>%
        ggplot(aes(
          x = Subsector,
          text = paste(paste0("Percentage: ", formatC(perc_subs * 100,
            digits = 1,
            format = "f"
          ), "%"),
          paste0("Volume: ", formatC(number_students_subs,
            digits = 0,
            format = "f",
            big.mark = ","
          )),
          sep = "\n"
          )
        )) +
        geom_count(aes(y = median_income),
          size = 1,
          shape = 21,
          color = "#4c2c92",
          fill = "#4c2c92"
        ) +
        geom_text(
          data = selection_subs(),
          aes(
            x = Subsector, y = median_income,
            label = paste0("£", formatC(median_income,
              digits = 0,
              format = "f",
              big.mark = ","
            ))
          ),
          nudge_x = 0.25,
          nudge_y = -0.5,
          size = 3,
          colour = "#0b0c0c",
          check_overlap = TRUE
        ) +
        geom_segment(aes(
          x = Subsector, xend = Subsector,
          y = 0,
          yend = median_income
        ),
        size = 0.5, color = "#4c2c92"
        ) +
        labs(
          y = "", x = "",
          title = ""
        ) +
        theme(
          legend.position = "none",
          plot.title = element_text((hjust <- 0.5)),
          axis.text.y = element_text(
            face = "bold", color = "#0b0c0c",
            size = 12, angle = 0
          ),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        scale_y_continuous(labels = comma_format(big.mark = ",")) +
        ylim(0, 50000) +
        coord_flip()
    }
  })


  # Page1: Reactive chart qualification level ------------------------------------------------------------------

  selection <- reactive({
    stat_hq %>%
      filter(Region == input$region &
        Sector == input$sector)
  })

  highestQualVolMedianChart <- reactive({
    if (input$showMedian == "Percentage") {
      selection() %>%
        ggplot(aes(
          x = Level_order,
          text = paste(paste0("Percentage: ", formatC(perc_hq * 100,
            digits = 1,
            format = "f"
          ), "%"),
          paste0("Volume: ", formatC(number_students_hq,
            digits = 0,
            format = "f",
            big.mark = ","
          )),
          sep = "\n"
          )
        )) +
        geom_bar(aes(y = perc_hq),
          fill = "#489FD6",
          stat = "identity"
        ) +
        labs(
          y = "", x = "",
          title = ""
        ) +
        theme(
          legend.position = "none",
          plot.title = element_text((hjust <- 0.5)),
          axis.text.y = element_text(
            face = "bold", color = "#0b0c0c",
            size = 12, angle = 0
          ),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(labels = levelsRelabelled) +
        coord_flip()
    } else {
      selection() %>%
        ggplot(aes(
          x = Level_order,
          text = paste(paste0("Percentage: ", formatC(perc_hq * 100,
            digits = 1,
            format = "f"
          ), "%"),
          paste0("Volume: ", formatC(number_students_hq,
            digits = 0,
            format = "f",
            big.mark = ","
          )),
          sep = "\n"
          )
        )) +
        geom_point(aes(y = median_income),
          size = 1,
          shape = 21,
          color = "#4c2c92",
          fill = "#4c2c92"
        ) +
        geom_text(
          data = selection(),
          aes(
            x = Level_order,
            y = median_income,
            label = paste0("£", formatC(median_income,
              digits = 0,
              format = "f",
              big.mark = ","
            ))
          ),
          nudge_x = 0.25,
          nudge_y = -0.5,
          colour = "#0b0c0c",
          size = 3,
          check_overlap = TRUE
        ) +
        geom_segment(aes(
          x = Level_order,
          xend = Level_order,
          y = 0,
          yend = median_income
        ),
        size = 0.5,
        color = "#4c2c92"
        ) +
        labs(
          y = "", x = "",
          title = ""
        ) +
        theme(
          legend.position = "none",
          plot.title = element_text((hjust <- 0.5)),
          axis.text.y = element_text(
            face = "bold", color = "#0b0c0c",
            size = 12, angle = 0
          ),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()
        ) +
        scale_y_continuous(labels = comma_format(big.mark = ",")) +
        scale_x_discrete(labels = levelsRelabelled) +
        ylim(0, 50000) +
        coord_flip()
    }
  })



  # Page1: Reactive chart subject ------------------------------------------------------

  observe({
    updateSelectInput(session,
      "inSelect2",
      choices = subsector_v,
      selected = "All sub-sectors"
    )

    updateSelectInput(session,
      "inSelect",
      choices = levelsRelabelled,
      selected = "All levels"
    )
  })

  # choices in select input
  sect_sub_v <- reactive({
    stat_subs_sub %>%
      filter(Region == input$region &
        Sector == input$sector) %>%
      distinct(Subsector, .keep_all = FALSE) %>%
      unlist(use.names = F)
  })

  hq_sub_v <- reactive({
    stat_hq_sub %>%
      filter(Region == input$region &
        Sector == input$sector) %>%
      distinct(Level_order, .keep_all = FALSE) %>%
      unlist(use.names = F)
  })


  indSubChart <- reactive({
    stat_subs_sub %>%
      filter(Region == input$region &
        Sector == input$sector &
        Subsector == input$inSelect2 &
        Level_order == input$inSelect) %>%
      ggplot(aes(
        x = reorder(Subject, -perc), y = perc,
        text = paste(
          paste0("Percentage: ", formatC(perc * 100,
            digits = 1,
            format = "f"
          ), "%"),
          paste0("Volume: ", formatC(number_students_sub,
            digits = 0,
            format = "f",
            big.mark = ","
          )),
          paste0("Average Earnings: ", "£", formatC(median_income,
            digits = 0,
            format = "f",
            big.mark = ","
          )),
          sep = "\n"
        )
      )) +
      geom_col(fill = "#003078") +
      labs(title = "", x = "", y = "") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      theme(
        plot.title = element_text((hjust <- 0.5)),
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        axis.text.y = element_text(
          face = "bold",
          color = "#0b0c0c",
          size = 12,
          angle = 0
        ),
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
      selected = "Level 2"
    )
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
  inWorkChartLevel <- reactive({
    s <- students_in_work %>%
      filter(Region == input$regionp &
        Sector == input$sectorp &
        Level_order == input$inSelect3) %>%
      distinct(Level_order, .keep_all = FALSE) %>%
      unlist(use.names = F)
    s <- tolower(s[1])
  })

  selection_in_work <- reactive({
    students_in_work %>%
      mutate(
        var_to_show = case_when(
          new_level == level_reac() ~ paste("At", inWorkChartLevel()),
          new_level < level_reac() ~ paste("Lower than", inWorkChartLevel()),
          TRUE ~ paste("Higher than", inWorkChartLevel())
        ),
        var_to_plot = case_when(
          new_level == level_reac() ~ "At level",
          new_level < level_reac() ~ "Lower than level",
          TRUE ~ "Higher than level"
        )
      ) %>%
      mutate(var_to_plot = factor(var_to_plot, levels = c("Higher than level", "At level", "Lower than level"), ordered = T)) %>%
      filter(Region == input$regionp &
        Sector == input$sectorp)
  })

  inWorkChart <- reactive({
    selection_in_work() %>%
      ggplot(aes(
        fill = var_to_plot,
        y = perc,
        x = Sector,
        text = paste0(var_to_show)
      )) +
      geom_bar(position = "stack", stat = "identity") +
      labs(
        title = NULL,
        x = "",
        y = ""
      ) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(values = c("#7FCFF2", "#1D70B8", "#f3f2f1")) +
      theme(
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "mm"),
        axis.title = element_text(size = 14),
        plot.title = element_text(hjust = 0),
        axis.text.x = element_text(
          face = "bold",
          color = "#0b0c0c",
          size = 12,
          angle = 0
        ),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })


  # Render Charts ------------------------------------------------------------------

  output$highestQualVolMedianChart <- renderPlotly({
    ggplotly(highestQualVolMedianChart(), tooltip = "text")
  })

  output$subsVolMedianChart <- renderPlotly({
    ggplotly(subsVolMedianChart(), tooltip = c("text"))
  })


  output$indSubChart <- renderPlotly({
    validate(
      need(nrow(stat_subs_sub %>%
        filter(Region == input$region &
          Sector == input$sector &
          Subsector == input$inSelect2 &
          Level_order == input$inSelect)) > 0, "There are no employees matching this selection. Please select again.")
    )
    ggplotly(indSubChart(), tooltip = "text")
  })


  output$studinWorkChart <- renderPlotly({
    validate(
      need(nrow(selection_in_work()) > 0, "There are no employees matching this selection. Please select again.")
    )
    ggplotly(inWorkChart(), tooltip = "text")
  })


  # Page1: Render top quals table ------------------------------------------------------

  observe({
    updateSelectInput(session,
      "inSelect",
      choices = hq_sub_v(),
      selected = "All levels"
    )

    updateSelectInput(session,
      "inSelect2",
      choices = sect_sub_v(),
      selected = "All sub-sectors"
    )
  })

  output$hqSubTable <- renderDataTable({
    DT::datatable(stat_hq_sub %>%
      filter(Region == input$region &
        Sector == input$sector &
        Level_order == input$inSelect &
        Subsector == input$inSelect2) %>%
      # final selection
      select(Qualification,
        Level = Level_order_UI,
        Percentage = perc,
        "Average earnings" = median_income
      ),
    rownames = FALSE,
    options = list(
      searching = FALSE,
      pageLength = 10,
      lengthMenu = c(10, 20),
      searchHighlight = TRUE,
      dom = "p",
      scrollX = TRUE
    ),
    width = "625px",
    height = "350px",
    style = "bootstrap", class = "table-bordered table-condensed align-center"
    ) %>%
      formatPercentage(3, digits = 1) %>%
      formatCurrency(4, digits = 0, currency = "£", mark = ",") %>%
      formatStyle(0,
        target = "row",
        color = "black",
        fontSize = "14px",
        backgroundColor = "white",
        lineHeight = "100%"
      )
  })

  observeEvent(input$reset, {
    updateSelectInput(session,
      "sector",
      choices = sectors_v,
      selected = "Construction"
    )

    updateSelectInput(session,
      "region",
      choices = regions_v,
      selected = "England"
    )

    updateSelectInput(session,
      "inSelect",
      choices = hq_sub_v(),
      selected = "All levels"
    )

    updateSelectInput(session,
      "inSelect2",
      choices = sect_sub_v(),
      selected = "All sub-sectors"
    )
  })


  # Page1: Render KPIS --------------------------------------------------------------------

  # percentage of people in sector
  output$perc_in_sector <- renderUI({
    perc <- kpis %>%
      filter(
        Sector == input$sector,
        Region == input$region
      ) %>%
      select(perc_students_sector)
    perc <- round(perc, digits = 3)

    tags$b(paste0(perc[[1]] * 100, "%"),
      style = "font-size:36px; text-align:center; color:	#ffffff"
    )
  })

  # median income
  output$median_in_sector <- renderUI({
    median_inc <- kpis %>%
      filter(
        Sector == input$sector,
        Region == input$region
      ) %>%
      select(median_income_sector)
    median_inc <- median_inc[[1]]
    median_inc <- prettyNum(median_inc, big.mark = ",")

    tags$b(paste0("£", median_inc),
      style = "font-size:36px; text-align:center; color:	#ffffff"
    )
  })

  # direction of change
  output$directionSector <- renderUI({
    wf <- wf %>%
      filter(
        Sector == input$sector,
        Region == input$region
      ) %>%
      select(direction = Years2022.2027)
    tags$b(paste0(round(wf$direction[[1]], digits = 1), "%"),
      style = "font-size:36px; text-align:center; color:	#ffffff"
    )
  })


  # Page2: Reactive treeplot ------------------------------------------------------

  # "#7FCFF2","#62B7E4","#489FD6","#3088C8","#1D70B8"

  # filter region/sector
  selected_region_sector <- reactive({
    qualifications %>%
      filter(Region == input$regionp &
        IndustrySector == input$sectorp) %>%
      mutate(
        ColourLevel = case_when(
          Level == "Level 2" ~ "#f3f2f1",
          Level == "Level 3" ~ "#7FCFF2",
          Level == "Level 4/5" ~ "#489FD6",
          Level == "Level 6" ~ "#1D70B8",
          Level == "Level 7+" ~ "#0b0c0c",
          TRUE ~ "#ffffff"
        ),
        ColourNextLevel = case_when(
          LevelNextQual == "Level  2" ~ "#f3f2f1",
          LevelNextQual == "Level 3" ~ "#7FCFF2",
          LevelNextQual == "Level 4/5" ~ "#489FD6",
          LevelNextQual == "Level 6" ~ "#1D70B8",
          LevelNextQual == "Level 7+" ~ "#0b0c0c",
          TRUE ~ "#ffffff"
        )
      )
  })

  # choices in select input for level based on previous selections
  level_select_vect <- reactive({
    selected_region_sector() %>%
      distinct(Level, .keep_all = FALSE) %>%
      unlist(use.names = F)
  })


  # update selectizeInput
  observeEvent(input$reset, {
    updateSelectInput(session,
      "inSelect3",
      choices = level_select_vect(),
      selected = "Level 2"
    )
  })


  # filter level and self-joins to get next quals on 4 levels
  tree_data <- reactive({
    selected_region_sector() %>%
      filter(Level == input$inSelect3) %>%
      left_join(selected_region_sector(),
        by = c("Region",
          "IndustrySector",
          "NextQual" = "Qual",
          "LevelNextQual" = "Level",
          "ColourNextLevel" = "ColourLevel"
        ),
        na_matches = "na",
        suffix = c(".1", ".2")
      ) %>%
      left_join(selected_region_sector(),
        by = c("Region",
          "IndustrySector",
          "NextQual.2" = "Qual",
          "LevelNextQual.2" = "Level",
          "ColourNextLevel.2" = "ColourLevel"
        ),
        na_matches = "na",
        suffix = c("", ".3")
      ) %>%
      select(Qual, starts_with(c("NextQual", "Links", "Level", "Colour"))) %>%
      # replace nas with 0 in number of students
      mutate_at(vars(matches("Links")), ~ replace(., is.na(.), 0)) %>%
      # replace nas for vector colors
      mutate_at(vars(matches("Colour")), ~ na_if(., "#ffffff")) %>%
      # restrict each next qual the number of nodes to top 10
      group_by(Qual, Level) %>%
      arrange(desc(Links.1), .by_group = T) %>%
      mutate(numbering = dplyr::row_number()) %>%
      filter(numbering <= 10) %>%
      ungroup()
  })

  # Make vectors for colors
  color1 <- reactive({
    tree_data() %>%
      distinct(Qual, ColourLevel, .keep_all = F)
  })
  color2 <- reactive({
    tree_data() %>%
      distinct(Qual, NextQual, ColourNextLevel, .keep_all = F) %>%
      filter(!is.na(NextQual))
  })
  color3 <- reactive({
    tree_data() %>%
      distinct(Qual, NextQual, NextQual.2, ColourNextLevel.2, .keep_all = F) %>%
      filter(!is.na(NextQual) & !is.na(NextQual.2))
  })
  color4 <- reactive({
    tree_data() %>%
      distinct(Qual, NextQual, NextQual.2, NextQual.3, ColourNextLevel.3, .keep_all = F) %>%
      filter(!is.na(NextQual) & !is.na(NextQual.2) & !is.na(NextQual.3))
  })


  # Page2: Render treeplot --------------------------------------------------

  output$treePlot <- renderCollapsibleTree({
    validate(
      need(nrow(tree_data()) > 0, "There are no employees matching this selection. Please select again.")
    )
    collapsibleTree(tree_data(),
      hierarchy = c("Qual", "NextQual", "NextQual.2", "NextQual.3"),
      root = paste("Your selection:"),
      zoomable = FALSE,
      fontSize = 12,
      fill = c(
        "#f3f2f1",
        color1()$ColourLevel,
        color2()$ColourNextLevel,
        color3()$ColourNextLevel.2,
        color4()$ColourNextLevel.3
      ),
      fillByLevel = TRUE,
      collapsed = TRUE,
      tooltip = T,
      width = 1200
    )
  })


  # Page 1&2: reactive Box & KPIs titles --------------------------------------------------------------

  `%notin%` <- Negate(`%in%`)

  # page titles
  output$page1title <- renderUI({
    if (input$region %in% c("England", "Yorkshire and The Humber")) {
      tags$b(paste0(
        input$sector, " in ", input$region,
        ": overview of employee education levels and qualification choices"
      ),
      style = "font-size: 24px;"
      )
    } else {
      tags$b(paste0(
        input$sector, " in the ", input$region,
        ": overview of employee education levels and qualification choices"
      ),
      style = "font-size: 24px;"
      )
    }
  })

  output$page2title <- renderUI({
    if (input$regionp %in% c("England", "Yorkshire and The Humber")) {
      tags$b(paste0(input$sectorp, " in ", input$regionp, ": post-16 qualification pathways"),
        style = "font-size: 24px;"
      )
    } else {
      tags$b(paste0(input$sectorp, " in the ", input$regionp, ": post-16 qualification pathways"),
        style = "font-size: 24px;"
      )
    }
  })



  # KPIs selection
  selection_kpis <- reactive({
    kpis %>%
      filter(
        Sector == input$sector,
        Region == input$region
      )
  })

  # IT is an acronym so it's written in upper case
  sector_to_show <- reactive({
    if (selection_kpis()$Sector[1] == "IT") {
      selection_kpis()$Sector[1]
    } else {
      tolower(selection_kpis()$Sector[1])
    }
  })

  # page 1: kpi percentage employees
  output$kpiSector <- renderUI({
    if (selection_kpis()$Region[1] %in% c("England", "Yorkshire and The Humber")) {
      tags$b(paste0("Proportion of employees aged 25-30 in ", selection_kpis()$Region[1], " that work in ", sector_to_show()),
        style = "font-size: 12px; color: #ffffff"
      )
    } else {
      tags$b(paste0("Proportion of employees aged 25-30 in the ", selection_kpis()$Region[1], " that work in ", sector_to_show()),
        style = "font-size: 12px; color: #ffffff"
      )
    }
  })


  # page 1: kpi percentage earnings
  output$kpiEarn <- renderUI({
    if (selection_kpis()$Region[1] %in% c("England", "Yorkshire and The Humber")) {
      tags$b(paste0("Annual average earnings of employees aged 25-30 in ", selection_kpis()$Region[1], " that work in ", sector_to_show()),
        style = "font-size: 12px; color: #ffffff"
      )
    } else {
      tags$b(paste0("Annual average earnings of employees aged 25-30 in the ", selection_kpis()$Region[1], " that work in ", sector_to_show()),
        style = "font-size: 12px; color: #ffffff"
      )
    }
  })

  # page 1: kpi growth
  output$kpiChange <- renderUI({
    wf <- wf %>%
      filter(
        Sector == input$sector,
        Region == input$region
      ) %>%
      select(direction = Years2022.2027)

    changeS <- ifelse(wf$direction[[1]] >= 0, "growth", "decline")

    if (selection_kpis()$Region[1] %in% c("England", "Yorkshire and The Humber")) {
      tags$b(paste0("Projected annual ", changeS, " in employment in ", sector_to_show(), " in ", selection_kpis()$Region[1], " up to 2027 (Working Futures)"),
        style = "font-size: 12px; color: #ffffff"
      )
    } else {
      tags$b(paste0("Projected annual ", changeS, " in employment in ", sector_to_show(), " in the ", selection_kpis()$Region[1], " up to 2027 (Working Futures)"),
        style = "font-size: 12px; color: #ffffff"
      )
    }
  })

  # page 1: subsector/qualification level chart
  output$box2title <- renderText({
    if (input$showMedian == "Percentage") {
      paste0("Distribution of employees by industry sub-sector and highest level of education")
    } else {
      paste0("Employee earnings by industry sub-sector and highest level of education")
    }
  })

  # page 1: subject chart and qualification table
  output$box3title <- renderText({
    validate(need(
      input$inSelect %notin% c("", NA),
      "Subject and qualification choices for employees"
    ))
    HTML(
      paste0(
        "Subject and qualification choices for employees at ", tolower(input$inSelect),
        " working in ", tolower(input$inSelect2)
      )
    )
  })


  # page 2: title for stacked chart box

  # select percentage of people in work

  selection_in_work <- reactive({
    students_in_work %>%
      mutate(
        var_to_show = case_when(
          new_level == level_reac() ~ paste("At", inWorkChartLevel()),
          new_level < level_reac() ~ paste("Lower than", inWorkChartLevel()),
          TRUE ~ paste("Higher than", inWorkChartLevel())
        ),
        var_to_plot = case_when(
          new_level == level_reac() ~ "At level",
          new_level < level_reac() ~ "Lower than level",
          TRUE ~ "Higher than level"
        )
      ) %>%
      mutate(var_to_plot = factor(var_to_plot, levels = c("Higher than level", "At level", "Lower than level"), ordered = T)) %>%
      filter(Region == input$regionp &
        Sector == input$sectorp)
  })



  output$box7title <- renderText({
    percInWork <- reactive({
      selection_in_work() %>%
        filter(Level_order == input$inSelect3) %>%
        select(perc)
    })

    HTML(paste0(
      formatC((percInWork()$perc[1]) * 100,
        digits = 1,
        format = "f"
      ), "%",
      " of employees have a highest qualification at ",
      inWorkChartLevel()
    ))
  })

  # page 2: treeplot
  output$box4title <- renderText({
    s <- selected_region_sector() %>%
      filter(Level == input$inSelect3)
    HTML(paste(
      "Post-16 qualification pathways starting at ",
      tolower(s$Level[[1]])
    ))
  })

  # page 2: treeplot legend
  svg_html_legend <- paste(
    '<svg width="450" height="20">',
    '<circle cx="10" cy="10" r="8" style="fill: #f3f2f1;"></circle>',
    '<circle cx="90" cy="10" r="8" style="fill: #7FCFF2;"></circle>',
    '<circle cx="170" cy="10" r="8" style="fill: #489FD6;"></circle>',
    '<circle cx="260" cy="10" r="8" style="fill: #1D70B8";></circle>',
    '<circle cx="340" cy="10" r="8" style="fill: #0b0c0c;></circle>',
    '<circle cx="10" cy="10" r="8" style="fill: none; stroke: black; stroke-width: 2;"></circle>',
    '<circle cx="90" cy="10" r="8" style="fill: none; stroke: black; stroke-width: 2;"></circle>',
    '<circle cx="170" cy="10" r="8" style="fill: none; stroke: black; stroke-width: 2;"></circle>',
    '<circle cx="260" cy="10" r="8" style="fill: none; stroke: black; stroke-width: 2;"></circle>',
    '<circle cx="340" cy="10" r="8" style="fill: none; stroke: black; stroke-width: 2;"></circle>',
    '<text x="25" y="12" alignment-baseline="middle" style="font-size: 15px;">Level 2</text>',
    '<text x="105" y="12" alignment-baseline="middle" style="font-size: 15px;">Level 3</text>',
    '<text x="185" y="12" alignment-baseline="middle" style="font-size: 15px;">Level 4/5</text>',
    '<text x="275" y="12" alignment-baseline="middle" style="font-size: 15px;">Level 6</text>',
    '<text x="355" y="12" alignment-baseline="middle" style="font-size: 15px;">Level 7+</text>'
  )

  output$svglegend <- renderUI({
    HTML(svg_html_legend)
  })

  # Download data -----------------------------------------------------------

  to_download_pg1 <- reactiveValues(
    subsectors_table = subsectors_table,
    highest_qualification_table = highest_qualification_table,
    qualifications_titles_table = qualifications_titles_table,
    subjects_table = subjects_table,
    income_proportions_table = income_proportions_table,
    working_futures_table = working_futures_table
    # qualifications_pathways_table = qualifications_pathways_table,
    # progression_to_work_by_level_table = progression_to_work_by_level_table
  )


  output$download_btn1 <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)


      reactiveValuesToList(to_download_pg1) %>%
        imap(function(x, y) {
          if (!is.null(x)) {
            file_name <- glue("{y}_data.csv")
            write.csv(x, file.path(temp_directory, file_name), row.names = F)
          }
        })

      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )


  to_download_pg2 <- reactiveValues(
    qualifications_pathways_table = qualifications_pathways_table,
    progression_to_work_by_level_table = progression_to_work_by_level_table
  )


  output$download_btn2 <- downloadHandler(
    filename = function() {
      paste("data_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)


      reactiveValuesToList(to_download_pg2) %>%
        imap(function(x, y) {
          if (!is.null(x)) {
            file_name <- glue("{y}_data.csv")
            write.csv(x, file.path(temp_directory, file_name), row.names = F)
          }
        })

      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    },
    contentType = "application/zip"
  )
}
