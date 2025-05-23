user_args <- list(cube_name = "SYSVAK", year = 2025, dumps = list(), write = F, geonaboprikk = T)
parameters <- get_cubeparameters(user_args = user_args)
load_and_format_files(parameters = parameters)
parameters[["filedesign"]] <- get_filedesign(parameters = parameters)

tellerfilnavn <- parameters$files[["TELLER"]]
tellerfildesign <- parameters$filedesign[[tellerfilnavn]]
InitDesign <- get_initialdesign(design = NULL, tellerfildesign = tellerfildesign, nevnerfildesign = NULL, parameters = parameters)
KUBEdesign <- FinnKubeDesignB(InitDesign = InitDesign, filename = tellerfilnavn, parameters = parameters)
tildesign <- list(Part = KUBEdesign$TMP)
fradesign <- tellerfildesign


test_that("find_redesign returns identical result to FinnRedesign", {
  expect_equal(FinnRedesign(fradesign, tildesign, parameters = parameters),
               find_redesign(fradesign, tildesign, parameters = parameters))
})
