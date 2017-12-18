#### Bird Families Listing Tool       ###
#### Gates Dupont, GLD44@cornell.edu  ###
#### Created: December 8th, 2017      ###
#### Version 1.0.121117               ###
#########################################

############################################################################
"DESCRIPTION: Creates and counts your family lifelist
and compares it to eBird data in a region to find targets 
for family listing"

"FUTURE PRIORITIES: 1) Remove sp. from eBird tax. input, making it more
straightforward to calculate proprtion of families and species observed.
Try scraping from Explore Region > World. Resolve with spreadsheet tax."
############################################################################

############################################################################
"INPUT"
regionInput = 'CG' # Can be found using Explore Region url
############################################################################

############################################################################
"PART 0"
### Setup
## Loading packages
library("XML") # For htmlParse()
library(httr) # For GET()
library(rvest) # For html_text()

## Load a function for later use
eBirdListCleaner <- function(thelist){
  listall = thelist # Carried over from other uses of this function
  locSpeciesWoSp = listall[!grepl(' sp.', listall)] # Remove spuhs
  locSpeciesWoSlsh = locSpeciesWoSp[!grepl("[/]", locSpeciesWoSp)] # Remove slashes
  listwohyb = locSpeciesWoSlsh[!grepl("hybrid", locSpeciesWoSlsh)] # Remove hybrids
  listwodom = listwohyb[!grepl("Domestic", listwohyb)] # Remove domestic
  listwopar = listwodom[!grepl(")", listwodom)] # Removing any others using parantheses
  locSpecies = as.character(listwopar) # Converting to characted list
  return(locSpecies)
}
############################################################################

############################################################################
"PART I"
### Loading data
## Importing data from downloaded files
lifelist = read.csv('ebird_world_life_list.csv') # A lifelist (Common Name, English)
erdtax = read.csv('eBird_Taxonomy_v2017_18Aug2017.csv') # eBird Taxonomy: http://www.birds.cornell.edu/clementschecklist/download/

# Scraping data from eBird site for specified regionInput
url = paste('http://ebird.org/ebird/country/',regionInput, "?yr=all&m=&rank=mrec&hs_sortBy=taxon_order&hs_o=asc", sep = '') # Concatenating url
doc = htmlParse(rawToChar(GET(url)$content)) # Scraping text from url
string = as(doc, "character") # Converting scraped text to character string
taxa = read_html(url) %>% # Selecting all species names from scraped text
  html_nodes('td.species-name') %>%
  html_text

## Converting input data to vectors
myLLcomName = as.vector(lifelist$Species) # Creating a vector of input lifelist by common names.
erdsppcomName = as.vector(erdtax$PRIMARY_COM_NAME) # Creating a vector of species from eBird tax. by common names.
erdfams = as.vector(erdtax$FAMILY) # Creating a vector of assosciated family names from eBird tax.
regionSpp = eBirdListCleaner(taxa) # Using above function to clean up scraped text

############################################################################

############################################################################
"PART II"
### World family lifelist
## Creating a data frame with input lifelist and assosciating families.
myLLfams = c() # Creating an empty list for associated families.
for(i in 1:length(myLLcomName)){ # Initializing a for-loop through the input lifelist.
  z = myLLcomName[i] # Indexing the common names from input lifelist.
  x = match(z, erdsppcomName) # Indexing each species from input lifelist in the eBird tax.
  y = erdfams[x] # Pulling the family name associated with the species from the index above.
  myLLfams[i] = y # Appending the previously indexed family to a family lifelist assosciated with the input lifelist.
}
myLLwFams = data.frame(myLLcomName, myLLfams) # Collecting output into a single dataframe.

## Output: world family lifelist
myFamilyLL = unique(myLLwFams$myLLfams) # Creating a separate list of just families of input lifelist (a family lifelist).
View(myFamilyLL)

## Quick numbers based on output
myFamilyLLcount = length(myFamilyLL) # Counting number of families on new family lifelist.
eBirdFamiliesCount = length(unique(erdfams))-1 # Counting number of families in the eBird taxonomy, and accoutning for empty family due to order-level ID fields, see line 12.
prFamiliesObserved = myFamilyLLcount/eBirdFamiliesCount # Calcluating proportion of observed families to total families in eBird tax.
paste("You have observed", myFamilyLLcount, "familes on world_family_lifelist,",
      "which is", round(100*prFamiliesObserved, 2), "percent of all bird families in the world")
LLfamilyNeeds = setdiff(unique(erdfams), myLLfams) # Creating a list of all families needed for family lifelist.
############################################################################

############################################################################
"PART III"
### Target families in a given region
## Creating a data frame with region list and assosciated families.
regionfams = c() # Creating an empty list to add associated families of input region list.
for(i in 1:length(regionSpp)){ # Initializing a for-loop through the input region list.
  z = regionSpp[i] # Indexing the common names from input region list.
  x = match(z, erdsppcomName) # Indexing each species from input region list in the eBird tax.
  y = erdfams[x] # Pulling the family name associated with the species from the index above.
  regionfams[i] = y # Appending the previously indexed family to a family region list assosciated with input region list.
}
regionListSppFamm = data.frame(regionSpp, regionfams) # Bringing output into a single dataframe.
############################################################################

############################################################################
"FINAL PRODUCT"
# Output: regional needs for family lifelist
regionFamNeeds = setdiff(regionfams,myLLfams) # A list of all birds needed for a family lifelist in a given region.
regionFamNeeds = data.frame(regionFamNeeds) # Preparing to remove issue, see next line comment.
regionFamNeeds = regionFamNeeds[regionFamNeeds$regionFamNeeds!= "",] # Removing blank row caused by sp. in eBird tax., see line 12.
View(regionFamNeeds) # Viewing output for ease of user.

print("Scroll up to see your family lifelist stats!")
