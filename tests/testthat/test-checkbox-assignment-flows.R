make_assignment_rvals <- function() {
  imported <- tibble::tibble(
    rowid = c("1", "2"),
    grp = c("A", "B"),
    anid = c("x", "y"),
    Fe = c(1, 2)
  )
  shiny::reactiveValues(
    importedData = imported,
    selectedData = imported,
    membershipProbs = tibble::tibble(
      ID = c("x", "y"),
      GroupVal = c("A", "B"),
      BestGroup = c("B", "A"),
      rowid = c("1", "2")
    ),
    edistance = tibble::tibble(
      anid = c("x", "y"),
      grp = c("A", "B"),
      grp_match = c("B", "A"),
      distance = c("1.0", "2.0"),
      rowid = c("1", "2")
    ),
    attrGroups = "grp",
    attrs = c("rowid", "grp", "anid"),
    attr = c("rowid", "grp", "anid"),
    chem = c("Fe")
  )
}

make_credentials_logged_out <- function() {
  shiny::reactiveValues(
    status = FALSE,
    res = tibble::tibble(username = NA_character_)
  )
}

if (!exists("quietly", mode = "function")) {
  quietly <- function(label = NULL, result) result
}
if (!exists("mynotification", mode = "function")) {
  mynotification <- function(...) invisible(NULL)
}

test_that("group membership checkbox assignments update selected rows", {
  rvals <- make_assignment_rvals()
  credentials <- make_credentials_logged_out()
  server_fun <- function(input, output, session) {
    groupServer(
      input = input,
      output = output,
      session = session,
      rvals = rvals,
      credentials = credentials,
      con = NULL
    )
  }

  shiny::testServer(server_fun, {
    session$setInputs(membership_checked_rowids = c("1"))
    session$setInputs(gAssignBestGroup = 1)
    session$flushReact()
    expect_equal(as.character(rvals$selectedData$grp[rvals$selectedData$rowid == "1"]), "B")

    session$setInputs(membership_checked_rowids = c("2"))
    session$setInputs(gGroupAssignChoice = "__new__", gGroupAssignNew = "C")
    session$setInputs(gChangeGroup = 1)
    session$flushReact()
    expect_equal(as.character(rvals$selectedData$grp[rvals$selectedData$rowid == "2"]), "C")
  })
})

test_that("euclidean checkbox assignments update selected rows", {
  rvals <- make_assignment_rvals()
  credentials <- make_credentials_logged_out()
  server_fun <- function(input, output, session) {
    euclideanDistanceSrvr(
      input = input,
      output = output,
      session = session,
      rvals = rvals,
      credentials = credentials,
      con = NULL
    )
  }

  shiny::testServer(server_fun, {
    session$setInputs(ed_checked_rowids = c("1"))
    session$setInputs(edAssignMatchGroup = 1)
    session$flushReact()
    expect_equal(as.character(rvals$selectedData$grp[rvals$selectedData$rowid == "1"]), "B")

    session$setInputs(ed_checked_rowids = c("2"))
    session$setInputs(edGroupAssignChoice = "__new__", edGroupAssignNew = "D")
    session$setInputs(edChangeGroup = 1)
    session$flushReact()
    expect_equal(as.character(rvals$selectedData$grp[rvals$selectedData$rowid == "2"]), "D")
  })
})
