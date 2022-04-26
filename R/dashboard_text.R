# File for storing the text content for the homepage of the dashboard

# Welcome text =========================================================================================

welcome_text <- function() {
  div(
    h2("Welcome"),
    "National careers service (NCS) user research shows that young people, parents and careers leaders
    want better information about pathways into different careers. They need information to help 
    explore and narrow down options and they want information to be engaging and accessible. 
    The longitudinal education outcomes (LEO) study includes information on industry sector for 
    employees alongside data on the education pathways young people follow. 
    Using LEO, we can link the area of work young people enter with the education choices and achievements 
    that guided them there. For more information, see: ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/graduate-outcomes-leo",
      "official statistics publication on Graduate Outcomes",
      .noWS = c("after")
    ),
    ".",
  )
}

# Contents (left panel) ================================================================================

## Industry sector overview text ----------------------------------------------------------------------------------

industry_overview_text <- function() {
  div(
    "This tab takes provide a holistic view of post-16 training routes for young people 
    employed in a specific region and industry sector (at ages 25-30). 
    It is a first attempt to develop these sorts of data for use as part of careers advice.  
    The data will show:",
    br(),
    tags$ul(
      tags$li("For each region of England, the industry sectors young people are employed in and how much they earn."), 
      tags$li("For each industry sector and region combination young people are employed in, a summary of the types of education achieved, 
    including levels of attainment, subject areas studied and most common qualifications held 
    (focussing on their highest level qualification)"), 
      tags$li("The most common qualification combinations that are associated with successful employment in each industry sector.
    For example – given a level 3 qualification in engineering, what are the most common subsequent qualifications at a higher 
    level that have historically resulted in well paid employment in the manufacturing sector.") 
      )
    )
}

## Qualification pathways text ---------------------------------------------------------------------------------------

qualification_pathways_text <- function() {
  div(
    "This tab "
  )
}

# SIC background (right panel) ===========================================================================

sic_groups_text <- function() {
  div(
    h3("IDBR (Inter-Departmental Business Register)"),
    "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.",
    h3("UK SIC (Standard Industrial Classification) code"),
    "The UK Standard Industrial Classification (SIC) of economic activities is used to classify
    businesses by the type of activity they do.",
    h3("Useful links"),
    a(
      href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
      "Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)"
    ),
    br(),
    a(
      href = "https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html",
      "ONS interactive SIC hierarchy"
    ),
    h3("SIC Groups and sections"),
    "Using the ONS Standard Industrial Classification (SIC) of economic activities, there are over 700 detailed industry codes
    at the five digit level, which are then grouped hierarchically at the four, three and two digit level before being grouped into
    21 broad industry sections (see the above link to the ONS interactive hierarchy). In this dashboard, the industry flow and regional analysis
    are both available only at the broadest level of the 21 industry sections. The tables go into more detail, with almost 250
    SIC groups available to view at the three digit level by expanding the broad sections which consist of the following:",
    br(),
    br(),
    tags$ol(
      tags$li("Accommodation and food service activities"),
      tags$li("Activities of extraterritorial organisations and bodies"),
      tags$li("Activities of households as employers - undifferentiated goods-and services-producing activities of households for own use"),
      tags$li("Administrative and support service activities"),
      tags$li("Agriculture, forestry and fishing"),
      tags$li("Arts, entertainment and recreation"),
      tags$li("Construction"),
      tags$li("Education"),
      tags$li("Electricity, gas, steam and air conditioning supply"),
      tags$li("Financial and insurance activities"),
      tags$li("Human health and social work activities"),
      tags$li("Information and communication"),
      tags$li("Manufacturing"),
      tags$li("Mining and quarrying"),
      tags$li("Other service activities"),
      tags$li("Professional, scientific and technical activities"),
      tags$li("Public administration and defence - compulsory social security"),
      tags$li("Real estate activities"),
      tags$li("Transportation and storage"),
      tags$li("Water supply - sewerage, waste management and remediation activities"),
      tags$li("Wholesale and retail trade - repair of motor vehicles and motorcycles")
    )
  )
}