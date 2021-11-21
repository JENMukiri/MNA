#" COUNTNA Function
#'
#' This function goes through the whole data set, counts NA's and #'returns back a visualization as a bar plot if NA's are present. #'This is very useful #' when working with an untidy data frame to #'first see how many NAs and which variables have the missing data
#'
#' @param  data tibble or data frame that you want use to test for missing NA's
#' @return a bar graph of {data} will plotted NA's or a message indicating no NA's
#' @examples
#' \dontrun{countna(datateachr::vancouver_trees)}
#' \dontrun{.countna(datateachr::apt_buildings)}
#' @export


countna <-  function(data){
  data1 <- dplyr::summarise(data, dplyr::across(dplyr::everything(),
                                  ~sum(is.na(.))))

  if(rowSums(data1) == 0) {
    stop("This data set has no missing values")
  }

  data1 %>%
    dplyr::select(where(~ sum(.) != 0)) %>%
    tidyr::pivot_longer(cols= dplyr::everything(), names_to = "variable",     values_to = "count")%>%  #transpose  data so it easier to plot w
    ggplot2::ggplot(ggplot2::aes(forcats::fct_reorder(variable),count)) + #bar graph are a good way to visualize this data
    ggplot2::geom_bar(stat = "identity")+
    ggplot2::labs(title="Variables with missing NA values",
         y="Number of NAs",
         x="Variable name") +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()
}
