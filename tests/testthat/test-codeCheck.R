context("interfaceplot test")

test_that("interfaceplot creation works", {
  expected_result <- structure(list(from = c("core", "fancymodule"), 
                                    to = c("fancymodule", "crazymodule"), 
                                    num_items = c(1L, 1L), 
                                    items = c("pm_global","vm_exchange")), 
                               row.names = c(NA, -2L), 
                               class = c("tbl_df","tbl", "data.frame"))
  
  cc <- codeCheck(system.file("dummymodel",package="lucode"))
  ifp <- interfaceplot(cc)
  expect_identical(ifp,expected_result)
  
  expected_result2 <- structure(c("core", "fancymodule", "fancymodule", "crazymodule", 
                                "1", "1", "pm_global", "vm_exchange"), .Dim = c(2L, 4L))
  
  ifp2 <- interfaceplot_legacy(cc, interactive=FALSE)
  expect_identical(ifp2,expected_result2)
  
  expected_result3 <- structure(list(x = list(nodes = structure(list(id = structure(c(1L, 3L, 2L), .Label = c("core", "crazymodule", "fancymodule"), class = "factor"), 
                                                                     color = c("#E41A1C", "#377EB8", "#4DAF4A"), 
                                                                     label = structure(c(1L, 3L, 2L), 
                                                                                       .Label = c("core", "crazymodule", "fancymodule"), class = "factor")), 
                                                                row.names = c(NA, -3L), class = "data.frame"),
                                              edges = structure(list(from = structure(1:2, .Label = c("core", "fancymodule"), class = "factor"), 
                                                                     to = structure(2:1, .Label = c("crazymodule", "fancymodule"), class = "factor"), 
                                                                     value = structure(c(1L, 1L), .Label = "1", class = "factor"), 
                                                                     title = structure(1:2, .Label = c("pm_global", "vm_exchange"), class = "factor"), 
                                                                     color = c("#E41A1C", "#377EB8")), 
                                                                row.names = 1:2, class = "data.frame"), 
                                              nodesToDataframe = TRUE, 
                                              edgesToDataframe = TRUE, 
                                              options = list(width = "100%", height = "100%", nodes = list(shape = "dot"), manipulation = list(enabled = FALSE)), 
                                              groups = NULL, width = NULL, height = NULL, idselection = list(enabled = FALSE), byselection = list(enabled = FALSE), 
                                              main = NULL, submain = NULL, footer = NULL, background = "rgba(0, 0, 0, 0)"), 
                                     width = NULL, height = NULL, sizingPolicy = list(defaultWidth = NULL, defaultHeight = NULL, padding = NULL, 
                                                                                      viewer = list(defaultWidth = NULL, defaultHeight = NULL, padding = NULL, 
                                                                                                    fill = TRUE, suppress = FALSE, paneHeight = NULL), 
                                                                                      browser = list(defaultWidth = NULL, defaultHeight = NULL, padding = NULL,
                                                                                                     fill = FALSE, external = FALSE), 
                                                                                      knitr = list(defaultWidth = NULL, defaultHeight = NULL, figure = TRUE)), 
                                     dependencies = NULL, elementId = NULL, preRenderHook = NULL, jsHooks = list()), 
                                class = c("visNetwork", "htmlwidget"), package = "visNetwork")
  
  ifp3 <- interfaceplot_legacy(cc, interactive=TRUE)
  expect_identical(ifp3,expected_result3)
  
})
