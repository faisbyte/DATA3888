# DATA3888 - Optiver 13

## Running the Shiny App

First, clone the repository

```
git clone https://github.com/faisbyte/DATA3888
cd DATA3888
```

Then, make sure you have installed all the packages we have used using the command below in R Studio's Console:

```
install.packages(c(
  "shiny",
  "bs4Dash",
  "plotly",
  "DT",
  "shinyWidgets",
  "fs",
  "glue",
  "stringr",
  "data.table",
  "MASS",
  "dplyr",
  "mgcv",
  "purrr",
  "arrow",
  "lightgbm",
  "ggplot2",
  "tibble",
  "readr",
  "flexdashboard"
))
```

We have used all the datasets under the `individual_book_train` folder. In the file `shinyapp.R`, please replace all occurences of:
`YOUR_PATH_TO/individual_book_train` to your filepath of the datasets. There are 10 such occurences of `YOUR_PATH_TO/individual_book_train` which need to be changed in lines 515, 537, 556, 591, 608, 619, 801, 973, 1153 & 1468 of `shinyapp.R`. 

Now, save use `Ctrl + S` or `CMD + S` and run the file.
