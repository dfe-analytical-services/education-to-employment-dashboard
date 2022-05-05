clean_json <- function(file, parent_script = "testUI", basedir = "./", iteration = "current") {
  # Reactable outputs some random keys as part of its table rendering, which snag
  # in shinytest when run across different machines. There's probably a way to fix
  # fix this in shinytest itself, but I gave up on trying and did a post-process
  # fix instead.
  filepath <- paste0(basedir, parent_script, "-", iteration, "/", file)
  shinytest_json <- readr::read_lines(filepath)
  for (i in 1:length(shinytest_json)) {
    shinytest_json[i] <- gsub('ey\": \".*\"', 'ey\": "random_key"', shinytest_json[i])
    shinytest_json[i] <- gsub("\r\n", "\n", shinytest_json[i])
  }
  readr::write_lines(shinytest_json, filepath)
}

run_set_shinytests <- function(dfinputs, outstring, listrecords) {
  # This function loops through a set of inputs and takes a snapshot for each one.
  # dfinputs: data frame containing field list and value list.
  # outstring: the stem for the output filename.
  # listrecords: list of input and output variables to record the values of.
  for (i in 1:nrow(dfinputs)) {
    file <- paste0(outstring, "_", i - 1, ".json")
    eval(parse(text = paste0("app$setInputs(", dfinputs$field[i], '="', dfinputs$value[i], '", wait_ = FALSE, values_ = FALSE)')))
    app$snapshot(
      items = listrecords,
      filename = file
    )
    clean_json(file)
  }
}

# Run the shiny tests.
# Should really set this up to loop over arrays of inputs in order to run
# through the shiny tests.
app <- ShinyDriver$new("../../", loadTimeout = 1.6e+05)

app$snapshotInit("testUI", screenshot = FALSE)

dfLogParams <- list(
  input = c(
    "navbar", "showMedian",
    "showMedian", "qualificationsubject",
    "sector", "region", "sectorp", "regionp", "inSelect3"
  ),
  output = c(
    "page1title", "perc_in_sector", "kpiSector", "median_in_sector",
    "directionSector", "kpiChange", "box2title", "box3title",
    "indSubChart", "page2title", "box7title", "studinWorkChart",
    "box4title", "treePlot"
  )
)

dfTestInputs <- data.frame(
  field = c(
    "navbar", "navbar", "navbar", "navbar", "navbar"
  ),
  value = c(
    "homepage", "overview", "pathways", "Accessibility", "Support and feedback"
  )
)

run_set_shinytests(dfTestInputs, "career-pathways", dfLogParams)
