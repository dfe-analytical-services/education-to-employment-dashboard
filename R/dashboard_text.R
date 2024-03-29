# File for storing the text content for the homepage of the dashboard

# Welcome text =========================================================================================

welcome_text <- function() {
  div(
    h2("Welcome"),
    "The longitudinal education outcomes (LEO) study now includes information on the industry sector of employment for Pay As You Earn (PAYE) employees. Using LEO, we can link the industry sector of young employees with the education achievements that helped guide them there.  ",
    br(),
    br(),
    "The data in the career pathways dashboard aims to use this development to:",
    tags$ul(
      tags$li("Explore how LEO can help us to understand the education pathways of employees in different industry sectors."),
      tags$li("Demonstrate how LEO data could be used to develop a careers information tool via an interactive dashboard.")
    ),
    "This dashboard has been produced to support the aims of the ",
    a(
      href = "https://www.gov.uk/government/groups/unit-for-future-skills",
      "Unit for Future Skills",
      .noWS = c("after")
    ),
    ".",
    "The complete underlying data for the dashboard is published alongside the accompanying ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/career-pathways-post-16-qualifications-held-by-employees/2018-19",
      "statistical release",
      .noWS = c("after")
    ),
    ".",
    "You can access specific cuts of data by using the ‘download’ buttons on each page of this dashboard."
  )
}

# Contents (left panel) ================================================================================

## Industry sector overview text ----------------------------------------------------------------------------------

industry_overview_text <- function() {
  div(
    "This tab provides a region-specific overview of employees who work in each industry sector. It includes information on overall levels of education and average earnings, the sub-sectors of work, and subject and qualification choices.
    It also includes a forecast of annual employment growth in the sector between 2022 and 2027. In this dashboard, region is based on the home postcode of the employee.",
    br(), br(),
    "Data included on the page are for employees who turn age 25-30 in the 2018-19 tax year.
    Specifically, the dashboard covers employees born between September 1988 and August 1993
    who were schooled in England. Employees must be in sustained employment and must not be
    studying at a higher education institution.
    For more information, please see the methodology pages in the ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/career-pathways-post-16-qualifications-held-by-employees/2018-19",
      "statistical release",
      .noWS = c("after")
    ),
    ".",
    br(), br(),
    tags$h4("Working Futures employment forecasts"),
    "The forecast for annual change in employment for each sector
    between 2022 and 2027 is sourced from ",
    a(
      href = "https://warwick.ac.uk/fac/soc/ier/wf7downloads/",
      "Working Futures",
      .noWS = c("after")
    ),
    ".",
    "Working Futures is a quantitative assessment of employment prospects in the UK
    labour market undertaken by Warwick Institute for Employment Research.",
    br(), br(),
    "Please note that these projections are based on employment estimates derived from published labour market data.
    They are not precise predictions of future employment levels, but represent the most likely trajectory of labour market change, given long-term trends in the economy and explicit assumptions about likely future economic change.
    The projections were created when it was expected there would be a negotiated Brexit and before the Covid-19 pandemic, which may result in considerable disruption to the UK and world economic system.
    These projections indicate what the labour market might have looked like before the pandemic. They, therefore, cannot be used to provide a measure of how the labour market has changed and the implications of the pandemic.",
    br(), br(),
    tags$h4("Education level, subject, and qualification"),
    "Education level and subject are based on highest qualification achievement at the start of the 2018-19 tax year.
The subject categories used are Sector Subject Area (SSA). Level and subject are based on the full history of education
achievements for each employee, from qualifications achieved at school and into post-16 and adult education.
The table of top qualifications for each region, sector and level group is restricted to qualifications achieved in post-16
education at level 2 or higher. A maximum of 20 qualifications are included for each selection, depending on availability of data.
This table does not include qualifications achieved at school, such as GCSEs.",
    br(), br(),
    "Qualifications achieved in higher education institutions use the Joint Academic Coding System (JACs). The JACs subject categories
for these qualifications have been mapped to SSA tier 1. The titles for these qualifications are a combination of the
qualification type and the JACs principal subject code.",
    br(), br(),
    tags$h4("Median annual earnings"),
    "The median earnings in this dashboard are presented as raw figures.
    They do not seek to control for differences in employee characteristics that may influence earnings over time or across different employee populations.",
    "Earnings estimates are based on information recorded through the PAYE system. The earnings estimates do not include any income that was recorded though the self-assessment tax system. This means that earnings will be underreported for employees who have self-assessment income in addition to earnings from paid employment collected by the PAYE system.
    The PAYE records from HMRC do not include reliable information on the hours worked in employment so it is not possible to accurately distinguish between employees in full time and part time employment.", br(), br(),
    tags$h4("Rounding and suppression"),
    "Employee numbers are rounded to the nearest 10, annual median earnings are rounded to
    the nearest 100 and percentages are provided to the nearest one decimal place.
    Figures have been suppressed with the value ‘u’ for annual earnings based on fewer than 10
    employees and ‘c’ for percentages where the numerator is less than 3 or the denominator is
    less than 6. Employee numbers below 5 are replaced with ‘low.’
    Suppressed figures have been removed from charts and tables."
  )
}

## Qualification pathways text ---------------------------------------------------------------------------------------

qualification_pathways_text <- function() {
  div(
    "This tab provides a summary of common qualification pathways followed by top earning employees of each industry sector and in each region.
    This analysis is designed to show potential qualification pathways that could lead to employment in an industry sector of
    interest.",
    br(), br(),
    "Qualification pathways are constructed from regulated qualifications achieved by employees in post-16 education at
    level 2 and above. Basic skills qualifications, unregulated provision and qualifications achieved at school,
    such as GCSEs, are excluded. The pathways are constructed by pairing qualifications from the achievements of
    each employee. Qualification pairs must show progression from:",
    br(), br(),
    tags$ul(
      tags$li("Level 2 to level 3"),
      tags$li("Level 3 to either level 4/5, level 6 or level 7"),
      tags$li("Level 4/5 to either level 6 or level 7"),
      tags$li("Level 6 to level 7+")
    ),
    "The pathways are modelled; data do not show the same cohort of employees moving through the entire pathway.
  Each qualification pair is based on the numbers of students who move between them, and the analysis takes
  the end qualification of a pair as the starting qualification of a new pair.
  The combining of qualification pairs in this way builds the full collapsible tree chart in the dashboard.
  For more information, please see the methodology pages in the ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/career-pathways-post-16-qualifications-held-by-employees/2018-19",
      "statistical release",
      .noWS = c("after")
    ),
    "."
  )
}

# SIC background (right panel) ===========================================================================

sic_groups_text <- function() {
  div(
    h3("IDBR (Inter-Departmental Business Register)"),
    "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.
    The IDBR data used in this dashboard is from datasets owned by the Office of National Statistics (ONS).
    The ONS does not accept responsibility for any inferences or conclusions derived from the IDBR data by third parties.",
    h3("UK SIC (Standard Industrial Classification) code"),
    "The UK Standard Industrial Classification (SIC) of economic activities is used to classify
    businesses by the type of activity they do.",
    h3("Useful links"),
    a(
      href = "https://www.gov.uk/government/publications/standard-industrial-classification-of-economic-activities-sic",
      "Standard industrial classification of economic activities (SIC) - GOV.UK.(www.gov.uk)", .noWS = c("after")
    ),
    br(),
    a(
      href = "https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html",
      "ONS interactive SIC hierarchy", .noWS = c("after")
    ),
    h3("SIC sector and sub-sector"),
    "This dashboard includes two aggregations of SIC codes: sector and sub-sector.
    Sector is an adjustment of the ONS 21 industry sections. The adjustment is based on the approach used in ",
    a(href = "https://warwick.ac.uk/fac/soc/ier/wf7downloads/", "Working Futures"),
    "and allows a direct link to forecast changes in employment.
    Sub-sector is a custom grouping of SIC 2 to 5 digit codes designed to add extra layers
    of detail to some of the broader sectors.",
    "See below for a description of the sectors included in the dashboard and how they relate to the SIC codes,
    and see the methodology pages in the ",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/career-pathways-post-16-qualifications-held-by-employees/2018-19",
      "statistical release",
      .noWS = c("after")
    ),
    " for more detail about the SIC groupings.",
    br(),
    tags$table(
      border = 0,
      tags$tbody(
        height = 50, width = 1000,
        tags$colgroup(
          tags$col(span = 2, width = 175),
          tags$tr(
            tags$td(align = "left", strong("Sector")),
            tags$td(align = "center", strong("SIC 2007", tags$br(), "Section")),
            tags$td(align = "left", strong("SIC 2007", tags$br(), "Division"))
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Accommodation & food"
            ),
            tags$td(
              align = "center",
              "I"
            ),
            tags$td(
              align = "left",
              "55-56"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Agriculture"
            ),
            tags$td(
              align = "center",
              "A"
            ),
            tags$td(
              align = "left",
              "01-03"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Arts & entertainment"
            ),
            tags$td(
              align = "center",
              "R"
            ),
            tags$td(
              align = "left",
              "90-93"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Construction"
            ),
            tags$td(
              align = "center",
              "F"
            ),
            tags$td(
              align = "left",
              "41-43"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Education"
            ),
            tags$td(
              align = "center",
              "P"
            ),
            tags$td(
              align = "left",
              "85"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Electricity & gas"
            ),
            tags$td(
              align = "center",
              "D"
            ),
            tags$td(
              align = "left",
              "35"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Engineering"
            ),
            tags$td(
              align = "center",
              "C"
            ),
            tags$td(
              align = "left",
              "26-28"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Finance & insurance"
            ),
            tags$td(
              align = "center",
              "K"
            ),
            tags$td(
              align = "left",
              "64-66"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Food, drink & tobacco"
            ),
            tags$td(
              align = "center",
              "C"
            ),
            tags$td(
              align = "left",
              "10-12"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Health & social work"
            ),
            tags$td(
              align = "center",
              "Q"
            ),
            tags$td(
              align = "left",
              "86-88"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "IT"
            ),
            tags$td(
              align = "center",
              "J"
            ),
            tags$td(
              align = "left",
              "61-62"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Manufacturing"
            ),
            tags$td(
              align = "center",
              "C"
            ),
            tags$td(
              align = "left",
              "13-25, 29-33"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Media"
            ),
            tags$td(
              align = "center",
              "J"
            ),
            tags$td(
              align = "left",
              "58-60, 63"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Mining & quarrying"
            ),
            tags$td(
              align = "center",
              "B"
            ),
            tags$td(
              align = "left",
              "05-09"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Other services"
            ),
            tags$td(
              align = "center",
              "S"
            ),
            tags$td(
              align = "left",
              "94-99"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Professional services"
            ),
            tags$td(
              align = "center",
              "M"
            ),
            tags$td(
              align = "left",
              "69-75"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Public admin & defence"
            ),
            tags$td(
              align = "center",
              "O"
            ),
            tags$td(
              align = "left",
              "84"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Real estate"
            ),
            tags$td(
              align = "center",
              "L"
            ),
            tags$td(
              align = "left",
              "68"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Support services"
            ),
            tags$td(
              align = "center",
              "N"
            ),
            tags$td(
              align = "left",
              "77-82"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Transport & storage"
            ),
            tags$td(
              align = "center",
              "H"
            ),
            tags$td(
              align = "left",
              "49-53"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Water & sewerage"
            ),
            tags$td(
              align = "center",
              "E"
            ),
            tags$td(
              align = "left",
              "36-39"
            )
          ),
          tags$tr(
            height = 10,
            tags$td(
              align = "left",
              "Whole & retail trade"
            ),
            tags$td(
              align = "center",
              "G"
            ),
            tags$td(
              align = "left",
              "45-47"
            )
          )
        )
      )
    )
  )
}
