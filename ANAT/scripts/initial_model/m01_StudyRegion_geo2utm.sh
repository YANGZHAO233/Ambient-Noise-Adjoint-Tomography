#!/bin/bash
utm_zone=48
perl ../LonLatAndUtm/convert_lonlat2utm_2.pl 01_simulation_region_geo.txt $utm_zone > 01_simulation_region_utm.txt
