pkg <- c("rvest", "xml2", "magrittr", "selectr")
sapply(pkg, require, character.only = TRUE)

fylkeUrl <- "https://www.ssb.no/klass/klassifikasjoner/104/endringer"
fylkeXpath <-
  "/html/body/div/div/div[2]/div/div/div[2]/div[1]/div[2]/div/div/div[2]/div[3]/div/table"

fylkeCss <- "html body div#app.klass div div#page div.sitewrapper div div.content.klass-item div.main div.tabs div.tab-content div div.tabBody div#endringer-panel div table.change-table.alternate"

ssbFylke <- xml2::read_html(fylkeUrl)
html_table(ssbFylke, fill = TRUE)

fylkeTbl <- rvest::html_nodes(ssbFylke, xpath = fylkeXpath) %>%
  rvest::html_table()


fylke <- fylkeUrl %>%
  xml2::read_html() %>%
  html_nodes(xpath = fylkeXpath)

fylke <- fylkeUrl %>%
  xml2::read_html() %>%
  html_node(xpath = ".//script[contains(., 'change-table.alternate')]")

html_nodes(ssbFylke, "table")
