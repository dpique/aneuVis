## README


- can add validate(need) statements to output (not just input, like in the examples!!)


# 2018-04-25
#got the ginkgo interface working, but now having issue with ternary plot for FISH data. - 
the plot is really small, probably because file name is too long (where does this come from?)

The code below from 2 days ago seems to work - will try to revert to this tomorrow:
# maybe an issue with plotting
https://github.com/dpique/aneuVis/blob/11efc4af18c867ac138dc4e999c27f8820668bdf/app.R

# 2018-04-26

Committed changes to git repo (app.R) that included a working version of the aneuvis app with both FISH and single cell sequencing data.

Calculated heterogeneity and aneuploidy scores for ginkgo data. Will double check correctness of these stats for FISH data.