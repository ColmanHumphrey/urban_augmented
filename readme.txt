This second project combines with the first to form the
relevant data sets and concepts, refer to UrbanAnalytics for the introduction.
Here, we introduce more sophisticating matching techniques.

1. We further the process of acquiring and cleaning data.
2. We apply matching techniques to various related datasets.

------------------------------------

The data:

- Street data
  - these form our intersections, i.e. the literal streets
  - also know if there are stop signs / traffic signals / nothing etc
  https://www.opendataphilly.org/dataset/street-centerlines
  https://www.opendataphilly.org/dataset/intersection-controls
- Property data
  - many properties of every listed property,
    including age, value, height, floorsize, presence of garage etc
  https://www.opendataphilly.org/dataset/opa-property-assessments
- Transit Routes / stops
  - subways, buses, trams; we generally use their station / stop locations
  http://www3.septa.org/hackathon/
- School data
  - location, size, years taught;
    we also have number of various types of incidents, but we don't use these
  https://www.opendataphilly.org/dataset/schools

Resuing all our old data:
- demographic,
- crime,
- business
- zoning / landuse, but to a lesser extent

------------------------------------

Note: data from the first project is stored
in data/old_data/(landuse; business_frame; block_and_group).rdata, and is pulled as needed.

There are no APIs to use this time (minor exception with transit),
we just have to pull everything from raw files. These
files are to be found in data/raw/.

code/get_extra_data is the first code folder to be used.
The folder data_functions contains any functions used in these files.
We use:
get_streets_and_intersections_data.R
 - pulls in streets, combines to form intersections
get_property_data.R
 - pulls in property data and cleans it
get_school_data.R
 - pulls in school data and cleans it
get_transit_data.R
 - pulls in transit data and cleans it;
get_traffic_control.R
 - pulls in traffic control data:
  stop signs, traffic signals etc
OUTPUT:
data/(streets, inter_points, prop_data, transit_stops, inter_controls).rdata

We actually re-pull crime, even though it happened previously.
Basically because we use different time, updated data:
get_prior_data/redo_crime_data.R
- It's cleaner now!
OUTPUT:
data/crime.rdata

Note that business_frame will actually get pulled and redone gently later,
but only to add what intersection contains each business (if any)
Clearly this could also be done on the spot, but either works

----------------

Data then needs to be combined / aggregated to intersections.

code/combine_to_inter_frame contains the files to do this.
Note that these are long, but the logic is somewhat consistent throughout,
hence they're not separated. 

combine_new.R
- aggregates the new data sets (property, school, transit, traffic)
- easy enough
combine_old.R
- aggregates the old data sets (crime, landuse, demographic data, business gets edited...)
- landuse and demographic data are a bit messy, they require the
  creation of intersections.
- crime isn't too bad
OUTPUT:
data/summary_lists/(summary_list_new, summary_list_old).rdata
--- edits business_frame, puts it in data/business_frame.rdata

THEN AFTER:
create_inter_frame.R
- pulls everything together, and chooses the (potentially) new names,
  and adds missing indicators, and replaces NA values
- does NOT contain landuse, or crime
create_crime_and_landuse_frames.R
- basically does the above, but
  for landuse and crime, and creates separate frames
- why?
  -- landuse is generally not included in our analyses (for the second half)
  -- crime is often the outcome (or input)
  -- and/or business: but then that's separate too
OUTPUT:
data/(inter_frame, crime_inter_frame, landuse_inter_frame).rdata

----------------
----------------

Plotting.

The folder code/plots contains functions
to plot the new data - pre-analyses, for descriptive purposes.

code/plots/plot_maps contains the files for each data type,
code/plots/plot_functions contains the functions used.

plot_example_intersection.R
 - plots one of our intersections, used to explain the calcs
plot_inter_controls.R
 - plots the traffic controls
plot_intersections.R
 - plots all our inters, over the whole city, and just center city
plot_property.R
 - plots average value of properties over the city
plot_schools.R
 - plots location, size and type of schools in the city
plot_transit.R
 - plots subway, bus and trolley lines

----------------
