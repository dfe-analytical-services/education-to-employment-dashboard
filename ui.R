

fluidPage(
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
                   h1("Post-16 Career Pathway Explorer: employees aged 25-30 in sustained employment, Tax year 2018-19"),
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
                     h3(actionLink("link_to_industryOverview_tab", "Industry Sector Overview")),
                     industry_overview_text(), # defined in R/dashboard_text.R
                     br(),
                     h3(actionLink("link_to_qualificationPathways_tab", "Qualification Pathways")),
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
             value = "overview", title = "Industry Sector Overview",
             
             ## Side bar ===========================================================================
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 
                 ### Help text ---------------------------------------------------------------------
                 helpText("Choose an industry sector and region to view the education level and earnings of employees."),
                 br(), 
                 
                 ### Sector input -------------------------------------------------------------------
                 selectizeInput("sector", options = list(create = TRUE), 
                                label = "Choose an industry sector:",
                                choices = sectors_v,
                                multiple = F,
                                selected = "Construction"),
                 
                 ### Region input --------------------------------------------------------------------
                 selectizeInput("region", options = list(create = TRUE),
                                label = "Choose a region:",
                                choices = regions_v,
                                multiple = F,
                                selected = "London"),
                 
                 ### Show earnings button --------------------------------------------------------------------
                 
                 radioButtons("showMedian",
                              selected = "No",
                              label = div(style = "white-space: nowrap; ", 
                                          "Show Average Earnings:"), 
                              choices = c("No","Yes"),
                              inline = F, 
                              width = "50%"),
                 br(), 
                 
                 conditionalPanel(
                   condition="input.subsectorlevel=='Subject and Qualification'",
                   
                   ### Help text ---------------------------------------------------------------------
                 helpText("Choose an industry sub-sector and qualification level for detail on subject and qualifications choices."),
                 br(),
                 
                 ### Reset button -------------------------------------------------------------------
                 
                 actionButton("reset", "Reset", 
                              style = "color: #0b0c0c; 
                                       font-size: 12px; 
                                       font-weight: bold; 
                                       background-color: #ffffff"),
                 br(), br(),
                 
                 ### Sub-Sector input -------------------------------------------------------------------
                 selectizeInput("inSelect2", 
                                options = list(create = TRUE),
                                label = "Choose an industry sub-sector:",
                                choices = subsector_v,
                                multiple = F,
                                width = "100%",
                                selected = "All subsectors"),
  
                 
                 ### Level input -------------------------------------------------------------------
                   selectizeInput("inSelect",
                                options = list(create = TRUE),
                                label = "Choose a qualification level:",
                                choices = levelsRelabelled,
                                multiple = F,
                                selected = "All levels")
                 
               
                 )
                 
                 ), # end of sidebar
               
              ## Main panel 1============================================================================
            
              mainPanel(
                width = 10,
                style = "height: 100vh; overflow-y: auto;",
              
                 ### TITLE 1-----------------------------------------------------------------------------
              
                uiOutput("page1title"),
                br(),
                div("Data for employees aged 25-30 in sustained employment in the 2018-19 tax year", style = "font-size: 16px; font-style: italic;"),
                br(),
              
                 ### Sections ------------------------------------------------------------------------------
              
                    #### KPIs ----------------------------------------------------------------------------
                
                  box(title = NULL,
                      width = 600,
                      status = "primary",
                      solidHeader = T,
                      column(
                        id = "second", 
                             align = "left", 
                             width = 3,
                             style='height:10vh; padding:5px; word-wrap: break-word;', 
                             uiOutput("perc_in_sector"), 
                             uiOutput("kpiSector")
                      ),
                      column(
                        id = "third", 
                             width = 1),
                      column(
                        id = "first", 
                             align = "left", 
                             width = 3,
                             style='height:10vh; padding:5px; word-wrap: break-word;', 
                             uiOutput("median_in_sector"),
                             tags$b("annual average earnings", 
                                    style = "font-size: 16px; color: #ffffff")
                      ), 
                      column(
                        id = "third", 
                             width = 1),
                      column(
                        id = "second", 
                             align="left", 
                             width = 3,
                             style='height:10vh; padding:5px; word-wrap: break-word;',
                             uiOutput("directionSector"),
                             uiOutput("kpiChange")
                      ),
                      column(
                        id = "third", 
                             width = 1)
                  ),

                box(title = NULL,
                    width = 600,
                    status = "primary",
                    solidHeader = T,
                    column(id = "third", width = 12, 
                           style='height:3vh; padding:0px;')), 
                
                #### Subsector & Level Charts ----------------------------------------------------------------------------

                tabsetPanel(id = "subsectorlevel",

                  tabPanel("Sub-sector and Level",
                           br(),
                           details(
                           inputId = "SubsLev",
                           label = "How to use these tabs",
                           help_text = "Select the Sub-sector and Level tab for a
                           breakdown of employee numbers and median earnings by highest 
                           level of education and detailed industry sector. 
                           Select the Subject and Qualification tab for more information 
                           about the highest qualifications employees hold."
                            ),
                           
                        div(textOutput("box2title"), style =  "font-size: 20px; font-weight: bold; margin-top: 15px;"),
                            column(width = 6, 
                                   style='height:20vh; padding:5px;',
                                   plotlyOutput("subsVolMedianChart")
                            ),
                            column(width = 6, 
                                   style='height:20vh; padding:5px;',
                                   plotlyOutput("highestQualVolMedianChart")
                            )
                ), 
              
                    #### Qualification table & Subject Chart ----------------------------------------------------------------------------
              
                tabPanel("Subject and Qualification", 
                         div(textOutput("box3title"), style =  "font-size: 20px; font-weight: bold;  margin-top: 15px; margin-bottom: 15px;"),
                          details(
                          inputId = "SubjectQualification",
                          label = "How to read use these tabs",
                          help_text = "When clicking on the highest post-16 qualifications held by employees tab you can see a selection of qualifications bla bla ... ,
                          and when clicking on the distribution of employees by subject of highest qualification chart you can see for selected subsector and qualification level all the subjects bla bla ..."),
                  
                  tabsetPanel(id = "qualificationsubject",
                    tabPanel("Highest post-16 qualifications held by employees: top 20 by volume",
                             column(id = "third", width = 12,
                                    DT::dataTableOutput("hqSubTable"))
                    ), 
                    tabPanel("Distribution of employees by subject of highest qualification", 
                             column(id = "third", width = 12,
                                    div(plotlyOutput("indSubChart"), align = "center"))
                    )
                  )
                )
            )
          ) # end of main panel
       )
), # end of Industry Sector Overview tab
             
            # Qualification Pathway  tab ==========================================================           

            tabPanel(
              value = "pathways", title = "Qualification Pathways",
  
              ## Side bar ===========================================================================
              sidebarLayout(
                sidebarPanel(
                  width = 2,
                  
                 ### Help text ---------------------------------------------------------------------
                helpText("Explain here what these selections do."),
      
                 ### Sector to work input -------------------------------------------------------------------
                selectInput(
                  "sectorp", 
                  label = "What sector do you want to work in?",
                  choices = sector_v2,
                  selected = "Construction"),
              
                 ### Region to live input -------------------------------------------------------------------
              selectInput(
                "regionp", 
                label = "Where you want to live?",
                choices = region_v2,
                selected = "London"),
              
                 ### Level to study input -------------------------------------------------------------------
              selectInput(
                "inSelect3", 
                label = "What qualification level do you want to study at?",
                choices = level_v2,
                selected = "Level 2")
              ), # end of sidebar

              ## Main panel 2============================================================================
                
                mainPanel(
                  width = 10,
                  style = "height: 100vh; overflow-y: auto;",                
                
                 ### TITLE 2-----------------------------------------------------------------------------
                  
                  uiOutput("page2title"),
                  br(),
                 div("Data for employees aged 25-30 in sustained employment in the 2018-19 tax year", style = "font-size: 16px; font-style: italic;"),
                  br(),               
                
                 ### Sections ------------------------------------------------------------------------------

                    #### Stacked bar chart ---------------------------------------------------------------
                 
                          div(textOutput("box7title"), style =  "font-size: 20px; font-weight: bold;  margin-top: 15px; margin-bottom: 15px;"),

                          div(plotlyOutput("studinWorkChart", height = 150), style = "align-content: left;"),
                          hr(),
                      
                      #### Tree plot --------------------------------------------------------------- 
                 
                          div(textOutput("box4title"), style =  "font-size: 20px; font-weight: bold;  margin-top: 15px; margin-bottom: 15px;"),
    
                          div("This chart indicates which are the most common pathways taken by employees in sustained employment 
                              in the tax year 2018-19. It is not exhaustive.", style = "font-size: 16px; font-style: italic;"),
                          br(),
                           uiOutput("svglegend"),
                 
                          collapsibleTreeOutput("treePlot")


           ) # end of main panel
        )
    ), # end of Qualification Pathway tab

# Accessibility ===========================================================

tabPanel(
  "Accessibility",
  warning_text(inputId = "accessWarn", text = "THIS IS A DRAFT STATEMENT - NEEDS UPDATING AFTER TESTING"),
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











