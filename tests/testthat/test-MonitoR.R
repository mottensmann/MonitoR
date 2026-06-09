## read_birdnet_slist ----------------------------------------
test_that("slist", {
  expect_equal(as.data.frame(read_birdnet_slist(.cached = F)), as.data.frame(read_birdnet_slist(.cached = T)))
})


## birdNET_process -------------------------------------------
predictions <- birdNET_process(
  audio = system.file("extdata", "20211220_064253.wav", package = "MonitoR"))
test_that("birdNET_process", {
  expect_equal(predictions[["species_name"]][1], 'Glaucidium passerinum_Sperlingskauz')
})

## birdNET_process_batch -------------------------------------
predictions <- run_birdnet(
  wave_files = system.file("extdata", "20211220_064253.wav", package = "MonitoR"))
predictions <- as.data.frame(predictions)
test_that("birdNET_process_batch", {
  expect_equal(predictions[["species_name"]][1], 'Glaucidium passerinum_Sperlingskauz')
})

unlink(system.file("extdata", "20211220_064253.BirdNET.results.txt", package = "MonitoR"))
