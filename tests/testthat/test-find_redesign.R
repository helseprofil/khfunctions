user_args <- list(name = "SYSVAK", year = 2025, dumps = list(), write = F, geonaboprikk = T)
parameters <- get_cubeparameters(user_args = user_args)
load_and_format_files(parameters = parameters)
parameters[["filedesign"]] <- get_filedesign(parameters = parameters)

tellerfilnavn <- parameters$files[["TELLER"]]
tellerfildesign <- parameters$filedesign[[tellerfilnavn]]
InitDesign <- get_initialdesign(design = NULL, tellerfildesign = tellerfildesign, nevnerfildesign = NULL, parameters = parameters)
KUBEdesign <- FinnKubeDesignB(InitDesign = InitDesign, filename = tellerfilnavn, parameters = parameters)
targetdesign <- tildesign <- list(Part = KUBEdesign$TMP)
orgdesign <- fradesign <- tellerfildesign

system.time({
  fasit <- FinnRedesign(fradesign, tildesign, parameters = parameters)
})

system.time({
  new <- find_redesign(fradesign, tildesign, parameters = parameters)
  })



test_that("find_redesign returns identical result to FinnRedesign", {
  expect_equal(fasit,
               find_redesign(fradesign, tildesign, parameters = parameters), ignore_attr = T)
})


rm(list = grep("InitDesign|user_args|targetdesign|orgdesign|tellerfildesign|KUBEdesign|tellerfilnavn|tildesign|fradesign|parameters|BUFFER", ls(), value = T, invert = T))
