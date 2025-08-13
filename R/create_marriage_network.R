#' Create a Marriage Network Graph
#' 
#' This function creates an undirected graph from marriage data within a specified time window
#' before a given election year.
#'
#' @param marriage_data A data frame containing marriage records with columns:
#'   - year: The year of the marriage
#'   - husband_familyname_std: Standardized husband's family name
#'   - wife_familyname_std: Standardized wife's family name
#' @param election_year The target election year
#' @param years_before Number of years before the election to include (default: 20)
#' 
#' @return An igraph graph object representing the marriage network
#' @importFrom igraph graph_from_data_frame
#' @importFrom dplyr filter
#' @export
create_marriage_network <- function(marriage_data, election_year, years_before = 20) {
  # Filter marriages within the specified time window
  marriages_window <- marriage_data %>%
    filter(year <= election_year & year >= election_year - years_before)
  
  # Create the graph
  marriage_network <- graph_from_data_frame(
    marriages_window,
    directed = FALSE,
    vertices = unique(c(marriages_window$husband_familyname_std, 
                       marriages_window$wife_familyname_std))
  )
  
  return(marriage_network)
}
