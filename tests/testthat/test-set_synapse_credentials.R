test_that("Invalid function parameter", {
  expect_false(.is_connected_to_genie("GENIE"))
})

test_that("Nothing supplied, see if PAT used over user/pass", {
  expect_message(
    set_synapse_credentials(), "You are now connected to 'Synapse' with your Personal*"
  )
})

test_that("set pat = NULL but PAT exists in environment", {
  expect_message(
    set_synapse_credentials(pat = NULL), "You are now connected to 'Synapse' with your Personal*"
  )
})

test_that("set pat = NULL but PAT exists in environment", {
  expect_error(
    set_synapse_credentials(pat = ""), "*"
  )
})

test_that("set pat = NULL but PAT exists in environment", {
  expect_error(
    set_synapse_credentials(pat = " "), "*"
  )
})

test_that("explicitly call user/pass but set wrong and PAT is in environ", {
  expect_error(
    set_synapse_credentials(username = "kitty", password = "puppy"),
    "There was an error authenticating your username (kitty)*"
  )
})

# <needs more authentication tests testing if authentication works in all scenerios using PAT/username etc>
# <what happens when you pass username only and no password? what happens when username is not NULL but pat is also passed? etc...>
