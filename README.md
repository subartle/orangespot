# blackspot
Shiny app exploring code violation data in Syracuse. View at [blackspot.org.uk](http://blackspot.org.uk) or grab this repository and run locally with: 

```
install.packages("dplyr")
install.packages("DT")
install.packages("ggplot2")
install.packages("htmltools")
install.packages("leaflet")
install.packages("rcharts")
install.packages("shiny")

shiny::runGitHub("subartle/orangespot")
```

Note `analytics.js` contains my Google analytics tracking code, you'll want to comment this out (i.e. `includeScript("analytics.js")` in `ui.R`) or swap in your own code if you're deploying this app!

## Screenshots
### v0.4
![screenshot of shiny app](http://blm.io/images/blackspot_v04.png)

### v0.3
![screenshot of shiny app](http://blm.io/images/blackspot_v03.png)

## Credits

* Code: some shiny code (especially UI) adapted from [SuperZip](https://github.com/jcheng5/superzip).
* Data: Edinburgh open data [vehicle collisions](http://www.edinburghopendata.info/dataset/vehicle-collisions).
