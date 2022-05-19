# library(shinya11y)

fluidPage(
  #  use_tota11y(),
  shinyjs::useShinyjs(),
  includeCSS("www/dfe_shiny_gov_style.css"),
  title = "Unit for Future Skills - Career Explorer Dashboard",



  # Set metadata for browser ===============================================================

  tags$html(lang = "en"),
  # meta_general(
  #   application_name = "UFS Career Explorer dashboard",
  #   description = "Post-16 Career Pathway Explorer",
  #   robots = "index,follow",
  #   generator = "R-Shiny",
  #   subject = "Outcomes for graduates",
  #   rating = "General",
  #   referrer = "no-referrer"
  # ),

  # Set title for search engines
  HTML("<title>Post-16 Career Pathway Explorer: employees aged 25-30 in sustained employment, Tax year 2018-19</title>"),


  # Navbar ===========================================================================

  # This CSS sets the 5th item on the navbar to the right
  tagList(
    tags$head(tags$style(HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(5) {
                           float: right;
                           }
                           ")))
  ),
  navbarPage("",
    id = "navbar",

    # Homepage tab ==========================================================
    tabPanel(
      value = "homepage", title = "Homepage",

      ## Tab content --------------------------------------------------------

      fluidPage(
        fluidRow(
          column(
            12,
            h1("Career pathways: post-16 qualifications held by employees"),
            welcome_text(), # defined in R/dashboard_text.R
            br(),
            br()
          ),

          ## Left panel ---------------------------------------------------------------
          column(
            6,
            div(
              div(
                class = "panel panel-info",
                div(
                  class = "panel-heading",
                  style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                  h2("Contents")
                ),
                div(
                  class = "panel-body",
                  h3(actionLink("link_to_industryOverview_tab", "Industry sector overview")),
                  industry_overview_text(), # defined in R/dashboard_text.R
                  br(),
                  h3(actionLink("link_to_qualificationPathways_tab", "Qualification pathways")),
                  qualification_pathways_text(), # defined in R/dashboard_text.R
                  br()
                )
              )
            ),
          ),

          ## Right panel ------------------------------------------------------

          column(
            6,
            div(
              div(
                class = "panel panel-info",
                div(
                  class = "panel-heading",
                  style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                  h2("IDBR and SIC background")
                ),
                div(
                  class = "panel-body",
                  sic_groups_text(), # defined in R/dashboard_text.R
                )
              )
            )
          )
        )
      )
    ), # End of homepage tabPanel()


    # Industry Sector Overview tab ==========================================================

    tabPanel(
      value = "overview", title = "Industry sector overview",

      ## Side bar ===========================================================================
      sidebarLayout(
        sidebarPanel(
          width = 2,

          ### Help text ---------------------------------------------------------------------
          helpText("Choose an industry sector and region to view the education level and earnings of employees."),
          br(),

          ### Sector input -------------------------------------------------------------------
          selectizeInput("sector",
            options = list(create = TRUE),
            label = "Choose an industry sector:",
            choices = sectors_v,
            multiple = F,
            selected = "Construction"
          ),

          ### Region input --------------------------------------------------------------------
          selectizeInput("region",
            options = list(create = TRUE),
            label = "Choose a region:",
            choices = regions_v,
            multiple = F,
            selected = "England"
          ),

          ### Show earnings button --------------------------------------------------------------------

          radioButtons("showMedian",
            selected = "Average earnings",
            label = div(
              style = "white-space: nowrap; ",
              "Choose a metric to show:"
            ),
            choices = c("Percentage", "Average earnings"),
            inline = F,
            width = "50%"
          ),
          uiOutput("SubjQualInputPanel"),
        ), # end of sidebar

        ## Main panel 1============================================================================

        mainPanel(
          width = 10,

          ### TITLE 1-----------------------------------------------------------------------------

          uiOutput("page1title"),
          br(),
          div("Data for employees aged 25-30 in sustained employment in the 2018-19 tax year", style = "font-size: 16px; font-style: italic;"),
          br(),

          ### Sections ------------------------------------------------------------------------------

          #### KPIs ----------------------------------------------------------------------------

          box(
            title = NULL,
            width = 600,
            status = "primary",
            solidHeader = T,
            column(
              id = "second",
              align = "left",
              width = 3,
              style = "height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
              uiOutput("perc_in_sector"),
              uiOutput("kpiSector")
            ),
            column(
              id = "third",
              width = 1
            ),
            column(
              id = "first",
              align = "left",
              width = 3,
              style = "height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
              uiOutput("median_in_sector"),
              uiOutput("kpiEarn")
            ),
            column(
              id = "third",
              width = 1
            ),
            column(
              id = "second",
              align = "left",
              width = 3,
              style = "height:15vh; min-height:96px; padding:5px; word-wrap: break-word;",
              uiOutput("directionSector"),
              uiOutput("kpiChange")
            ),
            column(
              id = "third",
              width = 1
            )
          ),
          box(
            title = NULL,
            width = 600,
            status = "primary",
            solidHeader = T,
            column(
              id = "third", width = 12,
              style = "height:3vh; padding:0px;"
            )
          ),

          #### Subsector & Level Charts ----------------------------------------------------------------------------

          tabsetPanel(
            id = "subsectorlevel",
            tabPanel(
              "Sub-sector and level",
              box(
                width = 12,
                br(),
                details(
                  inputId = "SubsLev",
                  label = "How to read these tabs",
                  help_text = "The boxes at the top of the page show summary data for the selected region and sector.
                  The employment projections are for total employment in the region and sector between 2022 and 2027; they are not restricted to employees aged 25-30. See the homepage for more detail.
                  Select the ‘sub-sector and level’ tab for information on employee numbers and average earnings by highest level of education and detailed industry sector. Select ‘Percentage’ in the side panel to display employee numbers and ‘Average earnings’ to display earnings.
                  Select the ‘subject and qualification’ tab for more information about the highest qualifications held by employees."
                ),
                div(
                  textOutput("box2title"),
                  style = "font-size: 20px; font-weight: bold; margin-top: 15px;"
                ),
                column(
                  width = 6,
                  style = "padding:5px;",
                  plotlyOutput("subsVolMedianChart")
                ),
                column(
                  width = 6,
                  style = "padding:5px;",
                  plotlyOutput("highestQualVolMedianChart")
                )
              )
            ),

            #### Qualification table & Subject Chart ----------------------------------------------------------------------------

            tabPanel(
              "Subject and qualification",
              br(),
              details(
                inputId = "SubjectQualification",
                label = "How to read these tabs",
                help_text = "The boxes at the top of the page show summary data for the selected region and sector.
                  The employment projections are for total employment in the region and sector between 2022 and 2027; they are not restricted to employees aged 25-30. See the homepage for more detail.
                  Select the ‘sub-sector and level’ tab for information on employee numbers and average earnings by highest level of education and detailed industry sector. Select ‘Percentage’ in the side panel to display employee numbers and ‘Average earnings’ to display earnings.
                  Select the ‘subject and qualification’ tab for more information about the highest qualifications held by employees."
              ),
              div(textOutput("box3title"), style = "font-size: 20px; font-weight: bold;  margin-top: 15px; margin-bottom: 15px;"),
              tabsetPanel(
                id = "qualificationsubject",
                tabPanel(
                  "Top 20 post-16 qualifications",
                  br(),
                  details(
                    inputId = "SubjectQualification",
                    label = "How to read these tabs",
                    help_text = "Select the ‘top 20 post-16 qualifications’ tab for the most popular highest qualifications held by employees and their average earnings.
                    Select the ‘Subject area of highest qualification’ tab for a breakdown of the subject areas of employees’ highest qualifications and their average earnings.
                    Charts can be filtered by qualification level and sub-sector using the drop down selectors in the side panel.
                    The table displays up to 20 qualifications, ordered by the number of employees who hold them.
                    Qualifications with small numbers of employees are not included in this table, which means some selections result in a table with no data fewer than 20 qualifications."
                  ),
                  DT::dataTableOutput("hqSubTable")
                ),
                tabPanel(
                  "Subject area of highest post-16 qualification",
                  br(),
                  details(
                    inputId = "SubjectQualification",
                    label = "How to read these tabs",
                    help_text = "Select the ‘top 20 post-16 qualifications’ tab for the most popular highest qualifications held by employees and their average earnings.
                    Select the ‘Subject area of highest qualification’ tab for a breakdown of the subject areas of employees’ highest qualifications and their average earnings.
                    Charts can be filtered by qualification level and sub-sector using the drop down selectors in the side panel.
                    The table displays up to 20 qualifications, ordered by the number of employees who hold them.
                    Qualifications with small numbers of employees are not included in this table, which means some selections result in a table with no data fewer than 20 qualifications."
                  ),
                  div(plotlyOutput("indSubChart"), align = "center")
                )
              )
            )
          )
        ) # end of main panel
      )
    ), # end of Industry Sector Overview tab

    # Qualification Pathway  tab ==========================================================

    tabPanel(
      value = "pathways", title = "Qualification pathways",

      ## Side bar ===========================================================================
      sidebarLayout(
        sidebarPanel(
          width = 2,

          ### Help text ---------------------------------------------------------------------
          helpText("Choose an industry sector, region and qualification level to view the most common career pathways of employees."),

          ### Sector to work input -------------------------------------------------------------------
          selectInput(
            "sectorp",
            label = "Choose an industry sector:",
            choices = sector_v2,
            selected = "Construction"
          ),

          ### Region to live input -------------------------------------------------------------------
          selectInput(
            "regionp",
            label = "Choose a region:",
            choices = region_v2,
            selected = "England"
          ),

          ### Level to study input -------------------------------------------------------------------
          selectInput(
            "inSelect3",
            label = "Choose a qualification level:",
            choices = level_v2,
            selected = "Level 2"
          ),

          ### Download button -------------------------------------------------------------------

          downloadButton(
            outputId = "download_btn2",
            label = "Download",
            icon = shiny::icon("download")
          )
        ), # end of sidebar

        ## Main panel 2============================================================================

        mainPanel(
          width = 10,
          style = "height: 100vh; overflow-y: auto;",

          ### TITLE 2-----------------------------------------------------------------------------

          uiOutput("page2title"),
          div("Data for employees aged 25-30 in sustained employment in the 2018-19 tax year", style = "font-size: 16px; font-style: italic; margin-top: 15px;"),


          ### Sections ------------------------------------------------------------------------------

          #### Stacked bar chart ---------------------------------------------------------------

          div(textOutput("box7title"), style = "font-size: 20px; font-weight: bold;  margin-top: 15px; margin-bottom: 15px;"),
          div(plotlyOutput("studinWorkChart", height = 100), style = "align-content: left;"),


          #### Tree plot ---------------------------------------------------------------

          div(textOutput("box4title"), style = "font-size: 20px; font-weight: bold;  margin-top: 15px; margin-bottom: 15px;"),
          div("This chart shows the most common education pathways taken by the
                          highest earning employees in sustained employment
                          in the tax year 2018-19.
                          Click on the qualification names or nodes to expand the chart and explore different pathways to higher level qualifications.
                          In the tooltip, the leaf count shows how many different qualification end points can be reached by expanding the chart.
                           Please note that this it is not a complete list of pathways.", style = "font-size: 16px; font-style: italic;"),
          br(),
          uiOutput("svglegend"),
          box(width = 12, collapsibleTreeOutput("treePlot"))
        ) # end of main panel
      )
    ), # end of Qualification Pathway tab

    # Accessibility ===========================================================

    tabPanel(
      "Accessibility",
      # warning_text(inputId = "accessWarn", text = "THIS IS A DRAFT STATEMENT - NEEDS UPDATING AFTER TESTING"),
      accessibility_statement() # defined in R/accessibility_statement.R
    ),

    # Support links ===========================================================

    tabPanel(
      "Support and feedback",
      support_links() # defined in R/supporting_links.R
    )
  ), # end of navbar page

  # Footer ====================================================================

  shinyGovstyle::footer(TRUE)
) # end of fluid page
