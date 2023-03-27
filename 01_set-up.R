


# this .r script creates adjacency graphs that represent phone calls, meetings, and mafia membership



# import mafia attributes
montagna <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_attributes.csv")
montagna # print


# recode levels of attributes ... 

  # criminal organization membership
  table(montagna$family)
  # merge "purple" and "green"... this step seems to be what Breuer & Varese (2022) did
  # see Breuer, N., & Varese, F. (2022). The Structure of Trade-type and Governance-type Organized Crime Groups: A Network Study. The British Journal of Criminology.
  
  # don't run
  # montagna$family[montagna$family == ""]          <- "1_none"      # no mafia membership
  # montagna$family[montagna$family == "batanesi"]  <- "2_batanesi"
  # montagna$family[montagna$family == "mistretta"] <- "3_mistretta"
  # montagna$family[montagna$family == "green" ]    <- "4_verde"     # 'verde' is green in Italian
  # montagna$family[montagna$family == "purple"]    <- "5_viola"     # 'viola' is purple in Italian
  
  # recode levels of criminal organization membership
  montagna$family[montagna$family == ""]          <- "1_none"        # no mafia membership
  montagna$family[montagna$family == "batanesi"]  <- "2_batanesi"
  montagna$family[montagna$family == "mistretta"] <- "3_mistretta"
  montagna$family[montagna$family == "green"]     <- "4_sconosciuto" # sconosciuto means 'unknown' in Italian
  montagna$family[montagna$family == "purple"]    <- "4_sconosciuto" # sconosciuto means 'unknown' in Italian
  
  # rank or title inside the mafia
  table(montagna$rank)
  # merge boss and soldier ... Breuer & Varese (2022) seem to merge associate and soldier into one category ...
  # ... but I code mafia members and non-members instead
  
  # don't run
  # montagna$rank[montagna$rank == ""]        <- "1_associate"
  # montagna$rank[montagna$rank == "soldier"] <- "2_soldier"
  # montagna$rank[montagna$rank == "boss"]    <- "3_boss"
  
  # recode levles of rank inside the mafia
  montagna$rank[montagna$rank == ""]        <- "1_associate" # no mafia rank or title = associates (non-members)
  montagna$rank[montagna$rank == "soldier"] <- "2_mafia"     # membership in mafia-type organization
  montagna$rank[montagna$rank == "boss"]    <- "2_mafia"     # membership in mafia-type organization 
  


# import edges -----------------------------------------------------------------
phone <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_phone.csv")    # phone calls by wire taps
meets <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_meetings.csv") # mafia summits or meeting co-attendance

# vertex list
v <- function(v){
  v <- v[, 1:2] # select columns
  v <- stack(v) # stack the vertex IDs
  v <- dplyr::select(v, values) # select IDs
  v$values <- sort(v$values)    # sort IDs 
  v <- dplyr::rename(v, node = values) # rename column
  v <- unique(v) # drop duplicates
  return(v) # return vertex list
}
v_meets <- v(meets)
v_phone <- v(phone)

# complete vertex list i.e., all mafioso that police detect through surveillance
v <- rbind(v_meets, v_phone); v <- unique(v)

# drop isolates in attributes
dataset <- function(x, v){
  x <- merge(x = x, y = v, by = "node", all.x = FALSE, all.y = TRUE); x$node <- sort(x$node)
  return(x)
}
montagna <- dataset(x = montagna, v = v)



# graph objects ----------------------------------------------------------------
graph <- function(m, v){ # create statnet objects
  # inputs:
  # ... m = edgelist
  # ... v = vertex list
  g <- igraph::graph_from_data_frame(m, directed = FALSE, v = v)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE) # remove edgeweights, loops, etc.
  g <- intergraph::asNetwork(g) # statnet object
  return(g)
}
phone <- graph(m = phone, v = v)
meets <- graph(m = meets, v = v)

# load attributes onto phone-tap graph
require(statnet)
phone %v% 'mob'   <- montagna$family
phone %v% 'title' <- montagna$rank

# ... mob membership bipartite graph
mobs <- dplyr::select(montagna, family, node) # mob membership bipartite

# I use the gambit of group strategy to operationalize mafia membership ...
# ... i.e., do mobsters belong the same criminal organization?
bipartiate <- function(m){ # bipartite projection
  
  # transfrom two-mode EDGELIST into graph object
  g = igraph::graph_from_data_frame(d = m, directed = FALSE)
  cat("\n ... bipartite graph? ")
  cat(igraph::is.bipartite(g)) # check bipartite graph: Should read FALSE
  cat("\n")
  
  # coerce to bipartite graph
  igraph::V(g)$type <- igraph::V(g)$name %in% m[, 1] # assign 'type' ...
  # ... bipartite graphs have 'types' i.e., mafia membership ties
  cat("\n ... bipartite graph? ")
  cat(igraph::is.bipartite(g)) # recheck bipartite graph: Should now read TRUE
  cat("\n")
  # bipartite projection
  g = igraph::bipartite.projection(graph = g, igraph::V(g)$type, multiplicity = TRUE) # multiplicity counts edges
  
  # return one-mode EDGELIST from the two-mode projection
  g = igraph::get.data.frame(g$proj1) # or get.adjacency() ...
  g = dplyr::rename(g, i = from, j = to) # rename columns i,j
  return(g)
} 
mobs <- bipartiate(m = mobs)



# ... convert graphs into matrix -----------------------------------------------
mat <- function(m, v){
  # inputs:
  # ... m = edgelist
  # ... v = vertex list
  
  # igraph object
  g = igraph::graph_from_data_frame(d = m, directed = FALSE, v = v)
  # ensure the graph is binary... remove possible loops, edge weights, etc. in the graph
  g = igraph::simplify(graph = g, remove.loops = TRUE, remove.multiple = TRUE)
  # print graph dimensions
  n <- igraph::vcount(g)
  m <- igraph::ecount(g)
  cat("\n G(n, m): "); cat("[", n, ",", m, "]"); cat("\n")
  # ... into graph object
  g <- intergraph::asNetwork(g) # statnet object
  return(g) # return graph
}
mobs   = mat(m = mobs, v = v)



# ... close .r script
