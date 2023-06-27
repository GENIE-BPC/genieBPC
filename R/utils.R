#' Reformat appendix tables to alphabetized two column pages
#'
#' @param rows_per_page number of rows per page desired
#' @param data an appendix data frame to alphabetize
#'
#' @return a list of data frames (one for every page) where appendix
#'         tables start on the left column, reach the end of the page,
#'         and pick up again on the right column in alphabetical order
#' @keywords internal
#' @export

.appen_abc <- function(rows_per_page, data){
  # each page can hold 30 rows total, so loop through dataset to create one
  # with four columns instead of 2
  columns <- nrow(data)/rows_per_page
  pages <- ceiling(nrow(data)/(rows_per_page*2)) # round up to whole page

  # create ID
  new_data <- data

  # split dataset into subsets of rows_per_page
  # this won't work unless its a perfect split to check here
  extra_rows <- rows_per_page - (nrow(data) %% rows_per_page)

  if (extra_rows == 0){
    list <- split(new_data,rep(1:columns,each=rows_per_page))
  } else {
    extra_df <- data.frame(matrix(ncol = ncol(data), nrow = extra_rows))

    colnames(extra_df) <- colnames(data)

    new_data <- rbind(new_data, extra_df)

    columns <- nrow(new_data)/rows_per_page


    list <- split(new_data,rep(1:columns,each=rows_per_page))
  }


  list <- lapply(list, as.data.frame)

  # create empty list with index of pages
  list2by2 <- vector("list", pages)

  # set counter for columns
  i = 1

  # set counter for pages
  a = 1

  # need to figure out when there are odd columns
  while (i <= columns) {
    while (a <= pages){
      # this will be the column to the right index
      j = i + 1

      # if an odd number of columns and this is the last column,
      # fill the right hand side with empty cells
      if (i %% 2 != 0 & i == columns){
        list[[j]] <- data.frame(col1 = c(rep("", rows_per_page)),
                                col2 = c(rep("", rows_per_page)))
      }

      # can't have the same name twice in flextable, so rename
      colnames(list[[j]]) <- c(paste0(colnames(list[[j]]), "1"))

      # bind datasets for the two columns on a page together
      page <- as.data.frame(cbind(list[[i]], list[[j]]))

      # store the output
      list2by2[[a]] <- page

      # increase counters
      a = a + 1

      # skip j and move to next column number
      i = i + 2
    }

  }

  # name the loop output with page numbers
  names(list2by2) <- c(paste0("page", 1:pages))

  return(list2by2)
}



