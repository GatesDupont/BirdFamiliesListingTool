# BirdFamiliesListingTool
This is a tool that allows eBird users to list all of the families of birds they have observed. It also tells the user what birds they need for their family lifelist in a region. 

Just a few things to get started:
1) Log in to eBird 
2) Make sure your preferences are set to: English -- Common Name translated to English (United States). You can do this by navigating to this page: http://ebird.org/ebird/prefs
3) Navigate to 'my eBird' and download your lifelist. A link titled 'download (csv)' can be found at this page: http://ebird.org/ebird/MyEBird?cmd=lifeList&listType=world&listCategory=default&time=life
4) After downloading, drag your data into the same folder (directory) as the R file.
5) Navigate to the following url to download the eBird taxonomy, which you will need for the R code. Download a CSV of the eBird Taxonomy: http://www.birds.cornell.edu/clementschecklist/download/
6) Open the R file and set your working directory (Session > Choose Working Directory).
7) Set your region of interest by typing in its region code. You can find this by searching for the region on Explore a Region. For example, Costa Rica's url is: http://ebird.org/ebird/country/CR?yr=all , so its county code is CR.

I have added detailed comments to almost every line of the code in case anyone is confused or wants to play around with it.
I hope you enjoy it!

Gates Dupont
GLD44@cornell.edu
