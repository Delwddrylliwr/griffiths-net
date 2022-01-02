#' Calculates the fractal or topological dimension of weighted and unweighted networks
#'
#' Following the analysis by MOretti and Munoz (https://www.nature.com/articles/ncomms3521#Sec10), and the weighted box covering algorithm of Wei et al (https://www.nature.com/articles/srep03049#Sec7), but not initially using renormalisation of box sizes.
#' @name griffiths-net

require(tidygraph)
require(ggplot2)
require(poweRlaw)

#' Global variables
dist_res = 100

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
  
  #Output the topological dimension, bundled with the plots and test result
  coef(model)[2]
}

#' Calculates the spectral gap of a network, and other characteristics that might suggest a Griffiths phase 
#' @noRd
#' @export
spectral_gap <- function(network, weights = NULL) {
  # Get the adjacency matrix of the network
  #adjacency <- as_adj(network, sparse = FALSE, attr = weights)
  adjacency <- as_adj(network, sparse = FALSE)
  
  # Get the eigenvalues, sort and plot as a histogram for comparison of scale and the spectral edge/gap
  eigens <- eigen(adjacency)
  ggplot(data = data.frame(eigens$values), aes(abs(eigens$values))) + 
    geom_histogram(binwidth = diff(range(abs(eigens$values)))/dist_res) +
    scale_fill_hue(c = 40) +
    theme(legend.position="none")
  
  # Plot components of the leading eigenvectors
  # Calculate the inverse participation ratios for each eigenvector in the spectral edge, to show localisation and rare regions
  ipr
  
  # Visualise the spectral edge eigenvectors across the nodes, to illustrate rare regions 
  
  # Switch to laplacian and calculate lower spectral edge, to check for lifschitz tail
}

#' Identifies weak ties in fractal networks, so that these can be removed, as per Gallos et al.: https://www.pnas.org/content/109/8/2825 
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
  
#' Simulates a deterministic Hierarchical Modular Network, as per Moretti et al.: https://www.nature.com/articles/ncomms3521
#' @noRd
#' @export
make_hierarchical_modular_network <- function(num_nodes, exponent) {
  #Create a network of the relevant size
  
  #Group nodes into initial modules and add densest links
  #"Initially, nodes are grouped into fully connected modules of size M0;" 
  
  #Recursively pair (super-)modules and add progressively less dense links between them
  #"Then nodes in different modules are clustered recursively into sets of b higher level blocks 
  #(for example, in pairs, b=2) linking their respective nodes with hierarchical level-dependent 
  #wiring probabilities (HMN-1): pl=??pl with 0<p<1 and ?? a constant. 
  #At level l, each of the existing N/2l pairs is connected on average by nl=4l(M0/2)2 × ??pl links. 
  #The resulting networks are always connected; with total number of N=M0 × bs nodes, and average connectivity .
  
  #HMN-1: At each hierarchical level l=1,2,.s, pairs of blocks are selected, each block of size 2i???1M0. All possible 4i???1M02 undirected eventual connections between the two blocks are evaluated, and established with probability ??pl, avoiding repetitions. With our choice of parameters, we stay away from regions of the parameter space for which the average number of connections between blocks nl is less than one, as this would lead inevitably to disconnected networks (as rare-region effects would be a trivial consequence of disconnectedness, we work exclusively on connected networks, that is, networks with no isolated components). As links are established stochastically, there is always a chance that, after spanning all possible connections between two blocks, no link is actually assigned. In such a case, the process is repeated until, at the end of it, at least one link is established. This procedure enforces the connectedness of the network and its hierarchical structure, introducing a cutoff for the minimum number of block-block connections at 1. Observe also that for M0=2 and p=1/4, ?? is the target average number of block-block connections and 1+?? the target average degree. However, by applying the above procedure to enforce connectedness, both the number of connections and the degree are eventually slightly larger than these expected, unconstrained, values.
  #For the HMN-2, the number of connections between blocks at every level is a priori set to a constant value ??. Undirected connections are assigned choosing random pairs of nodes from the two blocks under examination, avoiding repetitions. Choosing ?????1 ensures that the network is hierarchical and connected. This method is also stochastic in assigning connections, although the number of them (as well as the degree of the network) is fixed deterministically. In both cases, the resulting networks exhibit a degree distribution characterized by a fast exponential tail, as shown in Supplementary Fig. S1.
}

#' Generates an Hierarchical Modular Network with weak ties across it, as per Gallos et al.:https://www.pnas.org/content/109/8/2825 
#' @noRd
#' @export
play_weak_tied_hmn <- function(num_nodes, exponent, weak_tie_prob, weak_tie_strength) {
  #Create the underlying HMN
  network <- make_hierarchical_modular_network(num_nodes, exponent)
  
  #Randomly add weaker ties between pairs of nodes to shortcut the structure
  
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


