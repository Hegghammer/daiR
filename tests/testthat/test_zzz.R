## .ONLOAD ---------------------------------------------------------------------

test_that(".onLoad initiates auth state", {

  daiR:::.onLoad()
  expect_true(.auth$auth_active)

})

test_that(".onLoad authenticates", {
  skip_if_no_token()
  skip_if_offline()

  z <- daiR:::.onLoad()
  expect_match(z, "Token obtained and stored in .auth.")
  expect_true(.auth$has_cred())
})
