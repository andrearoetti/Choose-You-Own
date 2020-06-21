## Introduction ##

if (!require(tidyverse)) install.packages('tidyverse')
if (!require(lattice)) install.packages('lattice')
if (!require(caret)) install.packages('caret')
if (!require(readxl)) install.packages('readxl')
library(tidyverse)
library(lattice)
library(caret)
library(readxl)

credit_card <- read_xlsx("data/default of credit card clients.xlsx")

