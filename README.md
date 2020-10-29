# gtfs_gmaps_comp
Comparing GTFS time savings estimates with Google Maps Distance Matrix API time savings estimates to evaluate potential of bus preferential treatments.

GTFS time savings estimates were determined by identifying the shortest and longest amounts of time allowed between each stop pair across all trips in the schedule. It is assumed the difference between these times reflects delays due to congestion and therefore approximate the time savings potential of bus preferential treatments on each route segment. To validate this methodology, the Google Distance Matrix API was used to compare rush-hour and free-flow travel time estimates. The resulting map can be found at http://rpubs.com/karagtodd/gtfs_gmaps_comp .
