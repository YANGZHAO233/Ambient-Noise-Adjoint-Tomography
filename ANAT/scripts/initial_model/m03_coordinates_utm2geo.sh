#!/bin/bash
utm_zone=48

perl ../LonLatAndUtm/convert_utm2lonlat_3.pl 02_coordinate_utm.dat $utm_zone > 02_coordinates_geo.txt
