## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(trisk.model)


assets_testdata <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios_testdata <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial_features_testdata <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
ngfs_carbon_price_testdata <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))

## -----------------------------------------------------------------------------
str(assets_testdata)

## -----------------------------------------------------------------------------
head(assets_testdata)

## -----------------------------------------------------------------------------
str(financial_features_testdata)

## -----------------------------------------------------------------------------
head(financial_features_testdata)

## -----------------------------------------------------------------------------
str(ngfs_carbon_price_testdata)

## -----------------------------------------------------------------------------
head(ngfs_carbon_price_testdata)

## -----------------------------------------------------------------------------
str(scenarios_testdata)

## -----------------------------------------------------------------------------
head(scenarios_testdata, 50)

