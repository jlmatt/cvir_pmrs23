Data descriptions:

date: date oysters collected
zone: internal code for categorizing sites by region
bay: bay of site oysters were collected
sample_site: code of site oysters collected
salinity: salinity at site [ask JLM for clarification of how this was collected]
temp: temperature at site [ask JLM as above]
do: dissolved oxygen at site [ask JLM as above]
ph: pH [ask JLM as above]
tray_num: code on tray while measuring 
animal_id: id of oyster within group/site/date
total_weight: live weight of oyster after removing biofouling (g)
shell_length: max dimension hinge to bill (mm)
shell_width: max dimesnion perpendicular to shell length (mm)
shell height: max dimension of cup (mm)
plate: deep well plate id swab or tissue went into
well_id: well id within deep well plate for swab
box: box id for tube with swab
tube_id: label on tube containing swab or tissue
shell_weight_and_meat_weight: combined weight of shell and meat weight, oyster drained of liquor and both top shell and bottom shell (containing meat) weighed (g)
shell_weight: weight of shells after shucking out meat and letting shells desicate on counter overnight (g)
sex: F = female; M = male; U = undifferentiated
egg_count: number of eggs estimating by counting three independent samples of eggs stripped into beakers and cleaned through 63/20 mesh
sperm_rating: 0 is no activity, 5 is dense with lots of activity. always measured by Michaela Rust
notes: notes
site_name: more descriptive name of site of sampling
city_town_region: metadata related to city/town/region of site
water_body: bay, lagoon, or river where site is located
water_body_code: internal code for distinguishing water bodies
latitude: latitude coordinates of site
longitude: longitude coordinates of site
site_notes: site notes
meat_weight: meat weight (g) calculating by subtracting shell_weight from shell_weight_and_meat_weight
wt_adj_fec: weight adjusted fecundity, egg count divided by meat weight to give eggs per gram of soft tissue






Shiny app:

On 10/26/2026 re-deployed (published) the SHINY app.  I did this by making a new conda environment on 
eddie called shiny_clean and then running the following code:

> rsconnect::deployApp(
  appDir = ".",
  appPrimaryDoc = "app.R",
  appName = "cvir_pmrs23",
  account = "jlmatt"
)

Note I exported a .yml and a .txt of this environment in the shiny directory.  