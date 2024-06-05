library(shinytest2)

test_that("{shinytest2} recording: tech_stock_app2_a", {
  app <- AppDriver$new(name = "tech_stock_app2_a", height = 698, width = 1139)
  app$expect_values(output = "stock_table")
  app$set_inputs(stock = c("AAPL", "TSLA"))
  app$set_inputs(stock = c("AAPL", "TSLA", "INTC"))
  app$set_inputs(stock = c("AAPL", "TSLA", "INTC", "ADBE"))
  app$stop()
})
