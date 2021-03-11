
## .ONLOAD ---------------------------------------------------------------------

test_that(".onLoad initiates auth state", {

  daiR:::.onLoad()
  expect_true(.auth$auth_active)

})

