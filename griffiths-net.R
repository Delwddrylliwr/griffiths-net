#' Calculates the fractal or topological dimension of weighted and unweighted networks
#'
#' Following the analysis by MOretti and Munoz (https://www.nature.com/articles/ncomms3521#Sec10), and the weighted box covering algorithm of Wei et al (https://www.nature.com/articles/srep03049#Sec7), but not initially using renormalisation of box sizes.
#' @name griffiths-net

require(tidygraph)
require(ggplot2)
require(poweRlaw)

#' Public functions...
#' Makes a least squares estimate of the average number of neighbours at each path length
#' @noRd
#' @export
topological_dimension <- function(network, weights = NA) {
  #Differentiate weighted from unweighted network by whether a weights vector has been provided, corresponding to the edges of the network
  #Get the average size of neighbourhoods at each distance
  nbrhoods <- avg_nbrhoods(network, weights)
  
  #Transform so that a linear regression coefficient will represent an exponent for path length
  #Regress the size of neighbourhoods against the distance 
  model <- lm(formula = log(nbrhood_size) ~ log(distance), data = nbrhoods, na.action=na.exclude)
  ggplot(nbrhoods, aes(x = log(distance), y = log(nbrhood_size))) +
    geom_point() +
    geom_smooth(method='lm', se=FALSE, color='turquoise4') +
    theme_minimal() +
    labs(x='log(path length)', y='log(neighbourhood reached)', title='Network topological dimension') +
    theme(plot.title = element_text(hjust=0.5, size=20, face='bold'))
  
  #Confirm that a straight line is the best fit in the log-log plot (a power law), using Vuong's Kullbeck-Leibler-based test
  
  #Output the topological dimension
  coef(model)[2]
}

#' Calculates the spectral gap of a network, and other characteristics that might suggest a Griffiths phase 
#' @noRd
#' @export
spectral_gap <- function(network, weights = NA) {
  # Get the adjacency matrix of the network
  adjacency <- as_adj(network, sparse=FALSE)
  
  # Get the eigenvalues, sort and plot as a histogram for comparison of scale and the spectral edge/gap
  eigen()$vectors[,1]
  
  # Calculate the inverse participation ratios for each eigenvector in the spectral edge, to show localisation and rare regions
  
  # Visualise the spectral edge eigenvectors across the nodes, to illustrate rare regions 
  
  # Switch to laplacian and calculate lower spectral edge, to check for lifschitz tail
}

#' Identifies weak ties in fractal networks, so that these can be removed 
#' @noRd
#' @export
weak_ties <- function(network, weights = NA) {
  # Simulate a percolation process on the network, and graph the jumps in size of the largest component to identify the first major jump in connectivity, which threshold will be the basis for analysis
  
  # Identify modules using Girvan-Newman Q-modularity
  
  # Apply renormalised box-covering, to recover fractal scaling exponent of submodules
  
  # Compare submodules between GN and box renormalisation 
  
  # Fit degree distribution to power law, to differentiate from a Euclidean Lattice that might also have the same fractal scaling
  
  # Simulate a percolation process, and systematically reduce activation thresholds beyond the critical value to identify the threshold where small worlds are introduced
}
  
#' Private functions...
#' #' Counts the average number of neighbours for each weighted path length across a network
#' @noRd
avg_nbrhoods <- function(network, weights){
  #For weighted, get the table of distances between all nodes
  distance_table <- network %>% 
    distances(weights = weights)
  
  # extract an ordered vector of the distances
  distances <- distance_table %>% 
    as.vector() %>% 
    unique() %>% 
    sort() 
  
  distances <- distances[distances != 0]
    
  #For each node, count the number of other nodes within each distance of it
  sapply(distances, function (distance) {  
    as.numeric(mean(sapply(V(network), function(node) { #Average across nodes and return a dataframe
      length(
        distance_table[node, distance_table[node,] <= distance]
        )
      })))
    }) %>%
    cbind(distance = distances, nbrhood_size = .) %>%
    data.frame()
}


