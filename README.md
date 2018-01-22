# Karl Broman's Socks 
An interactive exploration of Bayesian statistics. 

Problem inspired by Karl Broman, who once [tweeted](https://twitter.com/kwbroman/status/523221976001679360?lang=en), "That the 1st 11 socks in the laundry are each distinct suggests there are a lot more socks." 

Analysis inspired by [Rasmus Bååth](http://www.sumsar.net/blog/2014/10/tiny-data-and-the-socks-of-karl-broman/).

## Background
When Karl Broman, Professor of Biostatistics at UW-Madison, tweeted his laundry observation, the response of the statistics community on Twitter was to take it as a challenge in probabalistic modeling - based on Karl's sample, how many total socks are there in his laundry, and what proportion are in pairs versus left as singles? 

## Analysis
The problem put forth by Broman is peculiar in that it does not lend itself to Big Data analysis, or even provide a "normal" amount of data to work with. We have only witnessed 11 data points (11 socks), so we must instead account for our current knowledge with a Bayesian prior and then simulate via a generative model. We use the method of Approximate Bayesian Computation (ABC), which Bååth describes on his blog.

## Results
To demonstrate the outcome, as well as some of the intricacies of Bayesian statistics, the results are presented in a Shiny app. This allows the user to interact with the analysis, specifying various priors and parameterizations and watching as the analysis dynamically changes. I invite you to take some time to play with the distributions! Broman later revealed that there were "21 pairs and 3 singletons" - how close can you get to the true results?

