# Teacher workforce analysis dashboard

---

## Introduction 

> ⚠️ **Warning:** This dashboard is being developed using dummy data.

This dashboard provides an accessible explanation of how postgraduate initial teacher training (PGITT)
trainee need is estimated using the Department for Education’s teacher workforce model. 
It allows users to explore how projections of pupil numbers influence the future teacher demand trajectory 
for both primary and secondary state-funded schools in England as well as the 
inflows and outflows to the teacher workforce, both historical and forecast, and how these relate to PGITT trainee need. 

This application is deployed in the following places:

- ...
- ...
- ...

---

## Requirements

The following requirements are necessary for running the application yourself or contributing to it.

### i. Software requirements (for running locally)

- Installation of R Studio 2024.04.2+764 "Chocolate Cosmos" or higher

- Installation of R 4.4.1 or higher

- Installation of RTools44 or higher

### ii. Programming skills required (for editing or troubleshooting)

- R at an intermediate level, [DfE R learning resources](https://dfe-analytical-services.github.io/analysts-guide/learning-development/r.html)

- Particularly [R Shiny](https://shiny.rstudio.com/)

### iii. Access requirements

There are no access requirements as all data is available in the repository.

---

## How to use

...

### Running the app locally

1. Clone or download the repo. 

2. Open the R project in R Studio.

3. Run `renv::restore()` to install dependencies.

4. Run `shiny::runApp()` to run the app locally.

### Folder structure

All R code outside of the core `global.R`, `server.R`, and `ui.R` files is stored in the `R/` folder. There is a `R/helper_functions.R` file for common custom functions, and scripts for the different UI panels in the `R/ui_panels/` folder.

...

### Packages

Package control is handled using renv. As in the steps above, you will need to run `renv::restore()` if this is your first time using the project.

Whenever you add new packages, make sure to use `renv::snapshot()` to record them in the `renv.lock` file.

### Tests

Automated tests have been created using shinytest2 that test the app loads and also give other examples of ways you can use tests. You should edit the tests as you add new features into the app and continue to add and maintain the tests over time.

GitHub Actions provide CI by running the automated tests and checks for code styling on every pull request into the main branch. The yaml files for these workflows can be found in the .github/workflows folder.

You should run `shinytest2::test_app()` regularly to check that the tests are passing against the code you are working on.

### Deployment

The app is deployed to Department for Education's shinyapps.io subscription using GitHub actions. The yaml file for this can be found in the .github/workflows folder. Maintenance of this is provided by the explore education statistics platforms team.

### Navigation

In general all .r files will have a usable outline, so make use of that for navigation if in RStudio: `Ctrl-Shift-O`.

### Code styling 

The function `styler::style_dir()` will tidy code according to tidyverse styling using the styler package. Run this regularly as only tidied code will be allowed to be committed. This function also helps to test the running of the code and for basic syntax errors such as missing commas and brackets.

You should also run `lintr::lint_dir()` regularly as lintr will check all pull requests for the styling of the code, it does not style the code for you like styler, but is slightly stricter and checks for long lines, variables not using snake case, commented out code and undefined objects amongst other things.

---

## How to contribute

...

### Flagging issues

If you spot any issues with the application, please flag it in the "Issues" tab of this repository, and label as a bug. Include as much detail as possible to help the developers diagnose the issue and prepare a suitable remedy.

### Making suggestions

You can also use the "Issues" tab in GitHub to suggest new features, changes or additions. Include as much detail on why you're making the suggestion and any thinking towards a solution that you have already done.

---

## Contact

Email the app developers: ittstatistics.publications@education.gov.uk.

Email the Explore Education Statistics team: explore.statistics@education.gov.uk
