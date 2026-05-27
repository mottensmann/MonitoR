## read_birdnet_slist ----------------------------------------
test_that("slist is up to date", {
  expect_equal(as.data.frame(read_birdnet_slist(.cached = F)), as.data.frame(read_birdnet_slist(.cached = T)))
})

