from pystac_client import Client
import planetary_computer as pc

# Search against the Planetary Computer STAC API
catalog = Client.open(
  "https://planetarycomputer.microsoft.com/api/stac/v1"
)

# Define your area of interest
aoi = {
  "type": "Polygon",
  "coordinates": [
    [
      [-15.789786479486366, 49.185349185490026],
      [10.572303657957661, 49.185349185490026],
      [10.572303657957661, 59.46809389471022],
      [-15.789786479486366, 59.46809389471022],
      [-15.789786479486366, 49.185349185490026]
    ]
  ]
}

# Define your search with CQL2 syntax
search = catalog.search(filter_lang="cql2-json", filter={
  "op": "and",
  "args": [
    {"op": "s_intersects", "args": [{"property": "geometry"}, aoi]},
    {"op": "=", "args": [{"property": "collection"}, "sentinel-2-l2a"]}
  ]
})

# Grab the first item from the search results and sign the assets
first_item = next(search.get_items())
pc.sign_item(first_item).assets




