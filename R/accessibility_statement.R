accessibility_statement <- function() {
  div(
    h2("Accessibility statement for the Career Pathways: post-16 qualifications held by employees dashboard"),
    "This accessibility statement applies to the Career Pathways: post-16 qualifications held by employees dashboard.",
    "This application is run by the Department for Education. We want as many people as possible to be able to use this application,
            and have actively developed this application with accessibilty in mind.",
    br(),
    br(),
    "We follow the reccomendations of the ", a(href = "https://www.w3.org/TR/WCAG21/", "WCAG 2.1 requirements", .noWS = c("after")),
    ". This application has been checked using the ", a(href = "https://github.com/ewenme/shinya11y", "Shinya11y tool", .noWS = c("after")),
    ", which did not detect accessibility issues.", # TODO: CHECK THIS!
    "This application also fully passes the accessibility audits checked by DfE analysts using the ",
    a(href = "https://developers.google.com/web/tools/lighthouse", "Google Developer Lighthouse tool", .noWS = c("after")),
    ". This means that this application:",
    br(),
    tags$div(tags$ul(
      tags$li("uses colours that have sufficient contrast;"),
      tags$li("allows you to zoom in up to 300% without the text spilling off the screen;"),
      tags$li("navigate most of the website using just a keyboard;"),
      tags$li("has its performance regularly monitored, with a team working on any feedback to improve accessibility for all users.")
    )), # TODO: UPDATE THESE BULLET POINTS
    "We've also made the website text as simple as possible to understand.",
    a(href = "https://mcmw.abilitynet.org.uk/", "AbilityNet", .noWS = c("after")),
    " has advice on making your device easier to use if you have a disability. We have trialled this website with multiple screen-reader software (Edge: Read aloud, ChromeVox and Pericles) and made adjustments to optimise their performace where feasible.",
    h3("How accessible this website is"),
    "We recognise that there are still potential issues with accessibility in this application, but we will continue
           to review updates to technology available to us to keep improving accessibility for all of our users. For example, these
           are known issues that we will continue to monitor and improve:",
    tags$div(tags$ul(
      tags$li("Keyboard navigation through the interactive charts is currently limited, and some features are unavailable for keyboard only users;"),
      tags$li("Alternative text in interactive charts is limited to titles and could be more descriptive (although this data is available in csv format).")
    )), # TODO: UPDATE THESE BULLET POINTS

    h3("Feedback and contact information"),
    "If you need information on this website in a different format like accessible PDF, large print, easy read, audio recording or braille or
      if you have any feedback on how we could further improve the accessibility of this application, please contact us at ",
    a(href = "mailto:ufs.contact@education.gov.uk", "ufs.contact@education.gov.uk", .noWS = c("after")),
    ".",
    h3("Technical information about this website’s accessibility"),
    "Department for Education is committed to making its website accessible, in accordance with the Public Sector Bodies
      (Websites and Mobile Applications) (No. 2) Accessibility Regulations 2018.",
    h3("Preparation of this accessibility statement"),
    "This statement was prepared on 19th May 2022 and this website was last tested on 19th May 2022 in advance of publication on 25th May 2022. The test was carried out by the Department for Education.", # TODO: Add date
    br(),
    br(),
  )
}
