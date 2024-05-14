# Last update on GitHub
.lastvers <- "14.05.2024"

# Find latest local update date
.localvers <- character()
if(file.exists("R/KHupdate.R")){
.localvers <- grep(".lastvers <-", readLines("R/KHupdate.R", n = 3), value = T)
.localvers <- sub(".*\"(.*)\".*", "\\1", .localvers)
} 

if(length(.localvers) == 0){
  .localvers <- "unknown"
}

#' Looking for updates to master/production branch
.updateproject <- function(lu, loc){
  
  # Check if master branch is active, if not return and let the user keep on dev work. 
  b <- system("git branch --show-current", intern = TRUE)
  
  if(b != "master"){
    message("\nYou are on the branch '", b, 
            "'.\nKeep on the good dev work or switch to the main branch `system(git checkout main)` to continue data processing!")
    return(invisible(NULL))
  } 
  
  message("\nYou are on the master/production branch")
  
  if(isTRUE(lu == loc)){
    message("\nThe project is up to date, you are ready to go!")
    return(invisible(NULL))
  }
  
  # Update all files if on master branch and updates available
  if(lu != loc){
    choice <- menu(choices = c("Yes", "No"),
                   title = paste0("\nUpdates available!!",
                                  "\n\nLast version on GitHub: ", lu,
                                  "\nYour local version: ", loc,
                                  "\n\nUpdate files (recommended)?"))
    
    if(choice == 1){
      message("\nFetching updates...")
      invisible(system("git fetch origin master"))
      invisible(system("git reset --hard origin/master"))
      invisible(system("git pull"))
    } else {
      message("\nSkipping updates, the project files might be outdated.")
    }
  }
}

.updateproject(lu = .lastvers,
               loc = .localvers)
