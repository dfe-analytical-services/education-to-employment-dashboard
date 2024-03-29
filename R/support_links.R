support_links <- function() {
  div(
    h2("Give us feedback"),
    "This dashboard is a new service that we are developing. If you have any feedback or suggestions for improvements, please submit them using our ",
    a(
      href = "https://forms.office.com/r/xrpAxmSA1b",
      "feedback form", .noWS = c("after")
    ),
    ".",
    br(),
    h2("Find more information on the data"),
    "The data used to produce the dashboard, along with methodological information can be found on ",
    a(href = "https://explore-education-statistics.service.gov.uk/find-statistics/career-pathways-post-16-qualifications-held-by-employees/2018-19", "Explore Education Statistics", .noWS = c("after")),
    ".",
    br(),
    h2("Contact us"),
    "If you have questions about the dashboard or data within it, please contact us at ",
    a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")),
    ".",
    br(),
    h2("See the source code"),
    "The source code for this dashboard is available in our ",
    a(href = "https://github.com/dfe-analytical-services/education-to-employment-dashboard", "GitHub repository", .noWS = c("after")),
    ".",
    br(),
    br(),
    br(),
    br(),
    br(),
    br()
  )
}
