
## .ONLOAD ---------------------------------------------------------------------

test_that(".onLoad initiates auth state", {

  daiR:::.onLoad()
  expect_true(.auth$auth_active)

})

## .ONATTACH -------------------------------------------------------------------

test_that(".onAttach authenticates", {
  skip_if_no_token()
  skip_if_offline()

  msg <- daiR:::.onAttach()
  expect_match(msg, "Token obtained and stored in .auth.")
  expect_true(.auth$has_cred())
})
