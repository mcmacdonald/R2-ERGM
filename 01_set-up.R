

# this .r script creates adjacency graphs that represent phone calls, meetings, mafia membership, and kinship relations



# import mafia attributes
montagna <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_attributes.csv")
montagna # print


# recode levels of attributes ... 

  # criminal organization membership
  table(montagna$familia)
  
  # recode levels of criminal organization membership
  montagna$familia[montagna$familia == ""]                          <- "01_none"        # no mafia membership
  montagna$familia[montagna$familia == "barcellona pozzo di gotto"] <- "02_barcellona"
  montagna$familia[montagna$familia == "batanesi"]                  <- "03_batanesi"
  montagna$familia[montagna$familia == "brancaccio"]                <- "04_brancaccio"
  montagna$familia[montagna$familia == "caltagirone"]               <- "05_caltagirone"
  montagna$familia[montagna$familia == "mazzaroti"]                 <- "06_mazzaroti"
  montagna$familia[montagna$familia == "mistretta"]                 <- "07_mistretta"
  montagna$familia[montagna$familia == "san mauro castelverde"]     <- "08_castelverde"
  montagna$familia[montagna$familia == "tortorici"]                 <- "09_tortorici"
  montagna$familia[montagna$familia == "-77"]                       <- "10_sconosciuto" # sconosciuto means 'unknown' in Italian
  
  # rank or title inside the mafia
  table(montagna$title)
  
  # recode levels of rank inside the mafia
  montagna$title[montagna$title == ""]          <- "01_none" # no rank or title = associates (non-members)
  montagna$title[montagna$title == "soldier"]   <- "02_soldier" # soldier in the mafia-type organization
  montagna$title[montagna$title == "boss"]      <- "03_capo"    # capo or boss mafia-type organization 
  montagna$title[montagna$title == "executive"] <- "03_capo"    # capo or boss mafia-type organization 
  montagna$title[montagna$title == "underboss"] <- "03_capo"    # capop or boss mafia-type organization 

  

# import edges -----------------------------------------------------------------
phones <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_phone.csv")     # phone calls by wire taps
summit <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_meetings.csv")  # mafia summits or meeting co-attendance
family <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_kinship.csv")   # kinship relations 
sights <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_sightings.csv") # sightings during surveillance 

# vertex list
v <- function(v){
  v <- v[, 1:2]
  v <- stack(v)
  v <- dplyr::select(v, values)
  v$values <- sort(v$values)
  v <- dplyr::rename(v, node = values)
  v <- unique(v)
  return(v)
}
v_phones <- v(phones)
v_summit <- v(summit)

# complete vertex list i.e., all mafioso that police detect through surveillance
v <- rbind(v_phones, v_summit); v <- unique(v)

# drop isolates in attributes
dataset <- function(x, v){
  x <- merge(x = x, y = v, by = "node", all.x = FALSE, all.y = TRUE); x$node <- sort(x$node)
  return(x)
}
montagna <- dataset(x = montagna, v = v)



# graph objects ----------------------------------------------------------------

# the mafia summit co-participation or meetings is weighted, but simple ergms do ...
# ... not handle multilayer graphs, so create graph that indicates suspects that ...
# ... meet more than once
summit <- igraph::graph_from_data_frame(summit, directed = FALSE, v = v)
summit <- igraph::get.adjacency(summit, type = "both")
summit <- as.matrix(summit)

# multiple summits
summit_m <- summit
summit_m[summit_m == 1] <- 0 # code single summit co-participation = 0
summit_m[summit_m == 2] <- 1 # code dual summit co-participation = 1
# dual <- igraph::graph_from_adjacency_matrix(dual, mode = "undirected", diag = FALSE, add.colnames = TRUE, add.rownames = TRUE)

# single summits
summit[summit == 2] <- 0 # code multiple summit co-participation = 0

# function to generate directed graphs
digraph <- function(m, v){ # create statnet objects
  # inputs:
  # ... m = edgelist
  # ... v = vertex list
  g <- igraph::graph_from_data_frame(m, directed = TRUE, v = v)
# g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE) # remove edgeweights, loops, etc.
  g <- intergraph::asNetwork(g)
  return(g)
}
phones <- digraph(m = phones, v = v)

# function to generate undirected graphs
bigraph <- function(m, v){ # create statnet objects
  # inputs:
  # ... m = edgelist
  # ... v = vertex list
  g <- igraph::graph_from_data_frame(m, directed = FALSE, v = v)
  g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE) # remove edgeweights, loops, etc.
  g <- intergraph::asNetwork(g)
  return(g)
}
family <- bigraph(m = family, v = v)
sights <- bigraph(m = sights, v = v)



# load attributes onto phone-tap graph
require(statnet)
phones %v% 'mafia'     <- montagna$familia
phones %v% 'title'     <- montagna$title
phones %v% 'padrino'   <- montagna$padrino
phones %v% 'arrest'    <- montagna$arrest
phones %v% 'informant' <- montagna$informant

# ... mob membership bipartite graph
mafias <- dplyr::select(montagna, familia, node) # mob membership bipartite

# I use the gambit of group strategy to operationalize mafia membership ...
# ... i.e., do mobsters belong the same criminal organization?
bipartiate <- function(m){ # bipartite projection
  
  # project bipartite edgelist into graph object
  g = igraph::graph_from_data_frame(d = m, directed = FALSE)
  cat("\n ... bipartite graph? ")
  cat(igraph::is.bipartite(g)) # check bipartite graph: Should read FALSE
  cat("\n")
  
  # coerce into bipartite graph
  igraph::V(g)$type <- igraph::V(g)$name %in% m[, 1] # assign 'type' ...
  # ... bipartite graphs have 'types' i.e., mafia membership ties
  cat("\n ... bipartite graph? ")
  cat(igraph::is.bipartite(g)) # recheck bipartite graph: Should now read TRUE
  cat("\n")
  # bipartite projection
  g = igraph::bipartite.projection(graph = g, igraph::V(g)$type, multiplicity = TRUE) # multiplicity counts edges
  
  # return unipartite edgelist
  g = igraph::get.data.frame(g$proj1) # or get.adjacency() ...
  g = dplyr::rename(g, i = from, j = to) # rename columns i,j
  return(g)
} 
mafias <- bipartiate(m = mafias)



# ... convert graphs into matrix -----------------------------------------------
mat <- function(m, v){
  # inputs:
  # ... m = edgelist
  # ... v = vertex list
  
  # igraph object
  g = igraph::graph_from_data_frame(d = m, directed = FALSE, v = v)
  # simplify the graph - remove possible loops, or edge weights
  g = igraph::simplify(graph = g, remove.loops = TRUE, remove.multiple = TRUE)
  # print graph dimensions
  n <- igraph::vcount(g)
  m <- igraph::ecount(g)
  cat("\n G(n, m): "); cat("[", n, ",", m, "]"); cat("\n")
  # ... into graph object
  g <- intergraph::asNetwork(g) # statnet object
  return(g) # return graph
}
mafias   = mat(m = mafias, v = v)



# close .r script --------------------------------------------------------------



# ... this code outputs the attribute data for the nodes in the different data sources that I use to create the graph illustrations

# don't run

# attributes for nodes only in the phone tap records or the surveillance records on summit participation 
# attributes <- merge(montagna, v_phones, by = "node", all.x = FALSE, all.y = TRUE)
# attributes <- merge(montagna, v_summit, by = "node", all.x = FALSE, all.y = TRUE)


