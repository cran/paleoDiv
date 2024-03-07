#' tree_archosauria
#'
#' A time-calibrated phylogenetic tree of Archosauria.
#'
#' @format An object of class==phylo with 13 tips and 12 internal nodes.

"tree_archosauria"


#' ages_archosauria
#'
#' A dataset containing earliest and latest occurrence dates for clades shown in the example phylogeny.
#'
#' @format A matrix with 13 rows and 2 collumns containing:
#' \describe{
#'   \item{FAD}{Earliest occurrence age}
#'   \item{LAD}{Latest occurrence age}
#' }
#' …for each taxon

"ages_archosauria"

#' archosauria
#'
#' A dataset of stratigraphic ranges of species within the clades in tree_archosauria.
#'
#' @format A list() object containing 15 species tables (data.frames) with the following data in each:
#' \describe{
#'   \item{tna}{taxon names (species names)}
#'   \item{max}{maximum ages}
#'   \item{min}{minimum ages}
#'   \item{ma}{mean ages}
#' }
#' @source Generated from data downloaded from the paleobiology database <https://paleobiodb.org> using the functions pdb(), occ.cleanup() and mk.sptab()

"archosauria"

#' diversity_table
#'
#' A dataset of diversity by stage, exemplifying the output produced by the divDyn-package.
#'
#' @format A data.frame() containing mean ages and diversity figures by stage.
#' \describe{
#'   \item{x_orig}{ages for each stage in the phanerozoic}
#'   \item{x}{ages converted for plotting on tree_archosauria, using the tsconv()-function}
#'   \item{Sauroporomorpha}{diversity by stage for Sauropodomorpha}
#'   \item{etc}{diversity by stage for each of the taxa represented in tree_archosauria}
#'   ...
#' }

"diversity_table"
