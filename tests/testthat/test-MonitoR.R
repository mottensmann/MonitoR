## read_birdnet_slist ----------------------------------------
test_that("slist", {
  expect_equal(as.data.frame(read_birdnet_slist(.cached = F)), as.data.frame(read_birdnet_slist(.cached = T)))
})


## birdNET_process -------------------------------------------
predictions <- birdNET_process(
  audio = system.file("extdata", "20211220_064253.wav", package = "MonitoR"))
test_that("birdNET_process (BirdNET v2.4)", {
  expect_equal(predictions[["species_name"]][1], 'Glaucidium passerinum_Sperlingskauz')
})

## birdNET_process_batch -------------------------------------
predictions <- run_birdnet(
  wave_files = system.file("extdata", "20211220_064253.wav", package = "MonitoR"))
predictions <- as.data.frame(predictions)
test_that("birdNET_process_batch  (BirdNET v2.4)", {
  expect_equal(predictions[["species_name"]][1], 'Glaucidium passerinum_Sperlingskauz')
})

predictions <- run_birdnet(
  wave_files = system.file("extdata", "20211220_064253.wav", package = "MonitoR"),
  model = 'Perch')
predictions <- as.data.frame(predictions)
test_that("birdNET_process_batch  (Perch v2)", {
  expect_equal(predictions[["species_name"]][2], 'Glaucidium passerinum')
})

## formatting-------------------------------------------------
birdnet <- NocMigR2::BirdNET(path = system.file("extdata", package = "MonitoR"), recursive = F)
birdnet <- readxl::read_xlsx(system.file("extdata", "BirdNET.xlsx", package = "MonitoR"))
test_that("NocMigR2  (BirdNET v2.4)", {
  expect_equal(birdnet[["Taxon"]][2], 'Haubenschlangenadler')
})

perch <- NocMigR2::BirdNET(path = system.file("extdata", package = "MonitoR"), recursive = F, model = "Perch")
perch <- readxl::read_xlsx(system.file("extdata", "Perch.xlsx", package = "MonitoR"))
test_that("NocMigR2  (Perch v2)", {
  expect_equal(perch[["Taxon"]][2], 'Sperlingskauz')
})

unlink(system.file("extdata", "20211220_064253.BirdNET.results.txt", package = "MonitoR"))
unlink(system.file("extdata", "20211220_064253.BirdNET.labels.txt", package = "MonitoR"))
unlink(system.file("extdata", "BirdNET.xlsx", package = "MonitoR"))

unlink(system.file("extdata", "20211220_064253.Perch.results.txt", package = "MonitoR"))
unlink(system.file("extdata", "20211220_064253.Perch.labels.txt", package = "MonitoR"))
unlink(system.file("extdata", "Perch.xlsx", package = "MonitoR"))
