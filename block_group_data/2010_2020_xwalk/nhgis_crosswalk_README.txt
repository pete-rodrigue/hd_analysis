----------------------------------------------------------------------------------------------------
Documentation for NHGIS geographic crosswalk files
----------------------------------------------------------------------------------------------------

Contents
    - Data Summary
    - Zone Identifiers
    - Citation and Use

Additional documentation on NHGIS crosswalks is available at:
    https://www.nhgis.org/user-resources/geographic-crosswalks


----------------------------------------------------------------------------------------------------
Data Summary
----------------------------------------------------------------------------------------------------
 
Each NHGIS crosswalk file provides interpolation weights for allocating census counts from a 
specified set of source zones to a specified set of target zones.

The crosswalk file name indicates which geographic levels, years, and geographic extent are covered 
in the file:

    nhgis_[source level][source year]_[target level][target year]{_state FIPS code}.csv

Geographic level codes:

    blk - Block
    bgp - Block group part (intersections between block groups, places, county subdivisions, etc.)
            - 1990 NHGIS level ID: blck_grp_598
            - 2000 NHGIS level ID: blck_grp_090
    bg  - Block group
    tr  - Census tract
    co  - County

State FIPS codes:

    If the file name includes a state code, it indicates that the file covers target zones within 
    the specified state or state equivalent. Such files may contain some source zones from 
    neighboring states in cases where the Census Bureau adjusted their state boundary lines between
    censuses. Files with no state code in their names cover the entire nation and, if the source 
    year is later than 1990, Puerto Rico.

    FIPS STATE/EQUIVALENT    FIPS STATE/EQUIVALENT   FIPS STATE/EQUIVALENT
    01   Alabama             22   Louisiana          40   Oklahoma
    02   Alaska              23   Maine              41   Oregon
    04   Arizona             24   Maryland           42   Pennsylvania
    05   Arkansas            25   Massachusetts      44   Rhode Island
    06   California          26   Michigan           45   South Carolina
    08   Colorado            27   Minnesota          46   South Dakota
    09   Connecticut         28   Mississippi        47   Tennessee
    10   Delaware            29   Missouri           48   Texas
    11   Dist. of Columbia   30   Montana            49   Utah
    12   Florida             31   Nebraska           50   Vermont
    13   Georgia             32   Nevada             51   Virginia
    15   Hawaii              33   New Hampshire      53   Washington
    16   Idaho               34   New Jersey         54   West Virginia
    17   Illinois            35   New Mexico         55   Wisconsin
    18   Indiana             36   New York           56   Wyoming
    19   Iowa                37   North Carolina     72   Puerto Rico
    20   Kansas              38   North Dakota       
    21   Kentucky            39   Ohio

Crosswalk file content:

    - The first row is a header row

    - Each subsequent row represents a spatial intersection between a source zone and target zone

    - Fields:
 
        - Zone identifiers (see specifications below):

            [source level][source year]gj:    NHGIS GISJOIN identifier for the source zone
            [source level][source year]ge:*   Census Bureau GEOID identifier for the source zone
                                              *Field not included in crosswalks from block group 
                                               parts
            [target level][target year]gj:    NHGIS GISJOIN identifier for the target zone
            [target level][target year]ge:    Census Bureau GEOID identifier for the target zone

        - Spatial relationship
        
            parea:     Proportion of source zone's land* area lying in target zone
                       - *If a source zone's area is entirely water, then this value is based on 
                         water area
                       - In crosswalks from 1990 zones, parea is based on "indirect overlay" through
                         2000 blocks. We begin by determining the expected proportion of each 1990 
                         source zone's area in each 2000 block's intersection with each target 2010 
                         zone, which is the product of two proportions:
                           - Proportion of 1990 zone in 2000 block in 2000 TIGER/Line-based file 
                           - Proportion of 2000 block in 2010 target zone in 2010 TIGER/Line-based
                             file
                         We then sum these products for each intersecting pair of source zone and
                         target zone.

        - Interpolation weights

		  ... in crosswalks from blocks:
        
            weight:    Interpolation weight, all characteristics
                       = Expected proportion of source block's population and housing located in 
                         target zone

          ... in crosswalks from higher levels:

            wt_pop:    Expected proportion of source zone's POPULATION in target zone
            wt_adult:  Expected proportion of source zone's ADULT POPULATION (18 years and over) in
                       target zone
            wt_fam:    Expected proportion of source zone's FAMILIES in target zone
            wt_hh:     Expected proportion of source zone's HOUSEHOLDS in target zone
                           - The household count is equal to the count of OCCUPIED HOUSING UNITS
                             and the count of HOUSEHOLDERS, so this weight applies equally to any
                             of these.
            wt_hu:     Expected proportion of source zone's HOUSING UNITS in target zone
            wt_ownhu:  Expected proportion of source zone's OWNER-OCCUPIED HOUSING UNITS in target
                       zone
            wt_renthu: Expected proportion of source zone's RENTER-OCCUPIED HOUSING UNITS in target
                       zone


----------------------------------------------------------------------------------------------------
Zone Identifiers
----------------------------------------------------------------------------------------------------

For a general explanation of the two types of identifiers in crosswalks (GISJOIN and GEOID), see:

    https://www.nhgis.org/user-resources/geographic-crosswalks#geog-ids

Zone identifier specifications:
    - Blocks
        - 1990
            - GISJOIN: 15 to 18 characters:
                - "G"                 1 character
                - State NHGIS code:   3 digits (FIPS + "0")
                - County NHGIS code:  4 digits (FIPS + "0")
                - Census tract code:  4 or 6 digits
                - Block code:         3 or 4 digits
            - GEOID: 14 or 15 characters:
                - State FIPS code:    2 digits
                - County FIPS code:   3 digits
                - Census tract code:  6 digits
                    - Tract codes that were originally 4 digits (as in NHGIS files) are extended to
                      6 with an appended "00" (as in Census Relationship Files)
                - Block code:         3 or 4 digits
        - After 1990
            - GISJOIN: 18 characters:
                - "G"                 1 character
                - State NHGIS code:   3 digits (FIPS + "0")
                - County NHGIS code:  4 digits (FIPS + "0")
                - Census tract code:  6 digits
                - Block code:         4 digits
            - GEOID: 15 characters:
                - State FIPS code:    2 digits
                - County FIPS code:   3 digits
                - Census tract code:  6 digits
                - Block code:         4 digits

    - Block group parts
        - GISJOIN, 1990: 37 or 39 characters:
            - "G"                                                1 character
            - State NHGIS code:                                  3 digits (FIPS + "0")
            - County NHGIS code:                                 4 digits (FIPS + "0")
            - County subdivision code:                           5 digits
            - Place/remainder code:                              5 digits
            - Census tract code:                                 4 or 6 digits
            - Congressional District (1987-1993, 100th-102nd 
                Congress) code:                                  2 digits
            - American Indian/Alaska Native area/remainder code: 4 digits
            - Reservation/trust lands/remainder code:            1 digit
            - Alaska Native regional corporation/remainder code: 2 digits
            - Urbanized area/remainder code:                     4 digits
            - Urban/Rural code:                                  1 digit
            - Census block group code:                           1 digit
        - GISJOIN, 2000: 26 characters:
            - "G"                       1 character
            - State NHGIS code:         3 digits (FIPS + "0")
            - County NHGIS code:        4 digits (FIPS + "0")
            - County subdivision code:  5 digits
            - Place/remainder code:     5 digits
            - Census tract code:        6 digits
            - Urban/rural code:         1 character ("U" for urban, "R" for rural)
            - Block group code:         1 digit

    - Block groups
        - GISJOIN: 15 characters:
            - "G"                 1 character
            - State NHGIS code:   3 digits (FIPS + "0")
            - County NHGIS code:  4 digits (FIPS + "0")
            - Census tract code:  6 digits
            - Block group code:   1 digit
        - GEOID: 12 characters:
            - State FIPS code:    2 digits
            - County FIPS code:   3 digits
            - Census tract code:  6 digits
            - Block group code:   1 digit

    - Census tracts
        - GISJOIN: 14 characters:
            - "G"                 1 character
            - State NHGIS code:   3 digits (FIPS + "0")
            - County NHGIS code:  4 digits (FIPS + "0")
            - Census tract code:  6 digits
        - GEOID: 11 characters:
            - State FIPS code:    2 digits
            - County FIPS code:   3 digits
            - Census tract code:  6 digits

    - Counties
        - GISJOIN: 8 characters:
            - "G"                 1 character
            - State NHGIS code:   3 digits (FIPS + "0")
            - County NHGIS code:  4 digits (FIPS + "0")
        - GEOID: 5 characters:
            - State FIPS code:    2 digits
            - County FIPS code:   3 digits


----------------------------------------------------------------------------------------------------
Citation and Use
----------------------------------------------------------------------------------------------------
 
Use of NHGIS crosswalks is subject to the same conditions as for all NHGIS data. 
See https://www.nhgis.org/citation-and-use-nhgis-data.

