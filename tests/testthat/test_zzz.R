## .ONATTACH ---------------------------------------------------------------------

test_that(".onAttach works", {
  version <- utils::packageVersion("daiR")
  message <- glue::glue(
    "Welcome to daiR {version}, your gateway to Google Document AI v1."
  )

  expect_message(daiR:::.onAttach(), message)
})
