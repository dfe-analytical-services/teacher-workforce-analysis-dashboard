# -----------------------------------------------------------------------------
# UI test: Check that the app loads without error
#
# Always make sure it is passing before merging any new code in
#
# This should prevent your app from ever failing to start up on the servers
# -----------------------------------------------------------------------------

# Start an app running
app <- AppDriver$new(
  name = "basic_load",
  height = 846,
  width = 1445,
  load_timeout = 120 * 1000,
  timeout = 60 * 1000,
  wait = TRUE,
  expect_values_screenshot_args = FALSE # Turn off as we don't need screenshots
)

# Wait until Shiny is not busy for 5ms so we know any processes are complete
app$wait_for_idle(5)

# Test that the app will start up without error
# Checks that the title is as expected
test_that("App loads and title of app appears as expected", {
  expect_equal(
    app$get_text("title"),
    # This is the title of the app on load, you should change to match your app's title
    # The app title is usually set early on in the ui.R script or through a variable in the global.R script
    "Teacher Workforce Supply Dashboard (England) - User guide"
  )
})
