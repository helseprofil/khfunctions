user_args <- list(name = "BARNEHAGE_KVALITET", year = 2025, dumps = list(), write = F, geonaboprikk = T)
parameters <- get_cubeparameters(user_args = user_args)
load_and_format_files(parameters = parameters)
parameters[["filedesign"]] <- get_filedesign(parameters = parameters)
parameters[["PredFilter"]] <- set_predictionfilter(parameters = parameters)


# merge_teller_nevner
tellerfilnavn <- parameters$files[["TELLER"]]
tellerfildesign <- parameters$filedesign[[tellerfilnavn]]
nevnerfildesign <- NULL
InitDesign <- get_initialdesign(design = NULL, tellerfildesign = tellerfildesign, nevnerfildesign = nevnerfildesign, parameters = parameters)
KUBEdesign <- FinnKubeDesignB(InitDesign = InitDesign, filename = tellerfilnavn, parameters = parameters)
TNdesign <- list(Part = KUBEdesign$TMP)

# do_redesign_recode_file
redesign <- find_redesign(orgdesign = tellerfildesign, targetdesign = TNdesign, parameters = parameters)
file <- fetch_filegroup_from_buffer(filegroup = tellerfilnavn, parameters = parameters)
FIL = data.table::copy(file)
dt = data.table::copy(file)

system.time({fasit <- OmkodFil(FIL = FIL, RD = redesign, parameters = parameters)})
system.time({new <- do_filter_and_recode_dimensions(dt = dt, redesign = redesign, parameters = parameters)})

test_that("do_recode_file returns same result as omkodfil", {
  
  expect_equal(fasit,
               do_filter_and_recode_dimensions(dt = file, redesign = redesign, parameters = parameters), ignore_attr = T)
  
})
