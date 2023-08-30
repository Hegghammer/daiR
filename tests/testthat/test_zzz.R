
## .ONATTACH ---------------------------------------------------------------------

test_that(".onAttach works", {

  expect_message(daiR:::.onAttach(), "Welcome to daiR 0.9.9, your gateway to Google Document AI v1.")

})
