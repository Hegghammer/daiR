## .ONATTACH ---------------------------------------------------------------------

test_that(".onAttach works", {
  expect_message(daiR:::.onAttach(), "Welcome to daiR 1.0.1, your gateway to Google Document AI v1.")
})
