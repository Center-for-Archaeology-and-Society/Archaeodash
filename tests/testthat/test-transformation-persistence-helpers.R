test_that("collapse and split helpers round trip values", {
  vals <- c("A", "B", "C")
  encoded <- collapse_transform_values(vals)
  decoded <- split_transform_values(encoded)
  expect_equal(decoded, vals)
})

test_that("safe table naming and keys are deterministic", {
  key1 <- build_dataset_key(c("user_b", "user_a"))
  key2 <- build_dataset_key(c("user_a", "user_b"))
  expect_equal(key1, key2)
  expect_true(nchar(key1) <= 40)
  expect_match(key1, "^dsk_")

  key3 <- build_dataset_key(c("user_a", "user_c"))
  expect_false(identical(key1, key3))

  tbl <- safe_table_name("User Name--Very Long Table Name!", max_len = 20)
  expect_true(nchar(tbl) <= 20)
  expect_true(grepl("^[a-z0-9_]+$", tbl))
})

test_that("transformation prefix is bounded length", {
  prefix <- transform_prefix(
    username = "very_long_username_for_testing",
    dataset_key = "a_really_long_dataset_key_that_should_be_trimmed",
    transformation_name = "a_really_long_transformation_name_that_should_be_trimmed"
  )
  expect_true(nchar(prefix) <= transform_prefix_max_len())
  expect_true(grepl("^[a-z0-9_]+$", prefix))

  full_names <- paste0(prefix, transform_table_suffixes)
  expect_true(all(nchar(full_names) <= transform_table_name_max_len))
})

test_that("transformation index table keeps expected suffix under 32-char policy", {
  idx <- transform_index_table("a_very_long_username_that_needs_shortening")
  expect_lte(nchar(idx), transform_table_name_max_len)
  expect_match(idx, "_transformations$")
})

test_that("dataset username prefixes include sanitized and truncated forms", {
  prefixes <- dataset_username_table_prefixes("nick-digs", max_len = 32L)
  expect_true("nick-digs" %in% prefixes)
  expect_true("nick_digs" %in% prefixes)

  long_user <- "zz_final_17717023151327"
  long_prefixes <- dataset_username_table_prefixes(long_user, max_len = 32L, hash_len = 8L)
  expect_true(long_user %in% long_prefixes)
  expect_true(substr(janitor::make_clean_names(long_user), 1, 22) %in% long_prefixes)
})

test_that("transformation index listing and single-snapshot load work", {
  skip_if_not_installed("RSQLite")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  username <- "demo_user"
  dataset_key <- build_dataset_key("demo_user_dataset")
  snapshot <- list(
    name = "tx_a",
    created = as.character(Sys.time()),
    chem = c("al"),
    attr = c("grp"),
    attrs = c("grp", "imputation", "transformation"),
    attrGroups = "grp",
    attrGroupsSub = c("A", "B"),
    transform.method = "none",
    impute.method = "none",
    runPCA = FALSE,
    runUMAP = FALSE,
    runLDA = FALSE,
    data.src = "elements",
    xvar = "",
    yvar = "",
    xvar2 = character(),
    yvar2 = character(),
    Conf = FALSE,
    int.set = 0.95,
    plot_theme = "viridis",
    use_symbols = TRUE,
    show_point_labels = FALSE,
    pointLabelColumn = "",
    ratioMode = "append",
    ratioSpecs = tibble::tibble(),
    selectedDataAll = data.frame(rowid = c("1", "2"), grp = c("A", "B"), al = c(1.1, 2.2), stringsAsFactors = FALSE),
    selectedData = data.frame(rowid = c("1", "2"), grp = c("A", "B"), al = c(1.1, 2.2), stringsAsFactors = FALSE),
    pcadf = tibble::tibble(),
    umapdf = tibble::tibble(),
    LDAdf = tibble::tibble()
  )
  expect_true(isTRUE(persist_transformation_db(con, username, dataset_key, snapshot)))

  tx_names <- list_transformations_db(con, username, dataset_key)
  expect_equal(tx_names, "tx_a")

  loaded <- load_single_transformation_db(con, username, dataset_key, "tx_a")
  expect_true(is.list(loaded))
  expect_equal(loaded$name, "tx_a")
  expect_equal(nrow(loaded$selectedData), 2)
})
