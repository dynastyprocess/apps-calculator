# DPTC 3.1.4

-   Revert changes from DPTC 3.1.2 

# DPTC 3.1.3

-   Save trade logs as CSVs 

# DPTC 3.1.2

-   Load data from bucket inside each session rather than globals. Reduces need to restart?

# DPTC 3.1.1

-   Fix bugs related to Startup Mode.
-   Add version tag to about page
-   Maybe fixup metatag?

# DPTC 3.1.0

-   Uses Docker + rhub/r-minimal
-   Rewrites logic to data.table
-   Uses DigitalOcean as storage platform and aws.s3 to interact with it
-   Uses DigitalOcean App Platform now
-   Environment variables are required to run the app and interact with the production bucket.
    -   If attempting to replicate on your own, must have values/ folder with various things in it as well as a trades/ folder as a data dump

# DynastyProcess Trade Calculator 3.0.0

Changes

-   Colours [redder!], logos [calculator specific], and fonts [IBM Plex Sans] updated
-   Get rid of trade details since data quality shitty
-   Automatically toggle superflex on when 16 or more teams are selected
-   Add search to values table
-   eliminate DT usage in favour of f7Table
-   align with shinyMobile v1.0.0 changes
-   add donation link
-   Add cookie to save/auto-load the last saved valuation/scoring settings
