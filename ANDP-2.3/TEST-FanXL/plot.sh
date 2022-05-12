#!/bin/bash
srcfolder="../Results/FINAL"

##################################################################
rm -rf plot_disps
mkdir plot_disps

##################################################################
R="-R0/45/2/5"
J="-JX4i/3i"
X="-K -O"
gmt gmtset FONT_ANNOT_PRIMARY +10p
gmt gmtset FONT_LABEL +12p
gmt gmtset FONT_TITLE +14p

##################################################################
for station in `ls $srcfolder/LINEAR`
do
    for item in `ls $srcfolder/LINEAR/$station`
    do
        path1=$srcfolder/LINEAR/$station/$item
        path2=$srcfolder/PWS/$station/$item
        name=`echo $path1 | gawk -F'/' '{print $NF}' | sed 's/.dat//g'`
        ##################################################################
        PS="plot_disps/plot_${name}.ps"
        echo "plot ${PS} ..."
        ##################################################################
        gmt psxy $R $J -K -T -Yc > $PS
        ##################################################################
        number=`sed -n '3p' $path1 | wc -w`
        if [ "$number" == "8" ]
        then
            gmt psxy $R $J $X $path1 -h3 -i0,1 -W1p,blue -Bxa5+l"Period (s)" -Bya0.5+l"velocity (km/s)" -BWS+t"Linear stacking result" >> $PS
            gmt psxy $R $J $X $path1 -h3 -i0,1,3 -Ey -Ben -B0 >> $PS
            gmt psxy $R $J $X $path1 -h3 -i0,4 -W1p,red >> $PS
            gmt psxy $R $J $X $path1 -h3 -i0,4,6 -Ey >> $PS
            if [ -f $path2 ]
            then
                gmt psxy $R $J $X $path2 -h3 -i0,1 -W1p,blue -Bxa5+l"Period (s)" -Bya0.5+l"velocity (km/s)" -BWS+t"Phase weighted stacking result" -X5i >> $PS
                gmt psxy $R $J $X $path2 -h3 -i0,2 -W1p,red -Ben -B0 >> $PS
            fi
        else
            gmt psxy $R $J $X $path1 -h3 -i0,1 -W1p,blue -Bxa5+l"Period (s)" -Bya0.5+l"velocity (km/s)" -BWS+t"Linear stacking result" >> $PS
            gmt psxy $R $J $X $path1 -h3 -i0,2 -W1p,red -Ben -B0 >> $PS
            if [ -f $path2 ]
            then
                gmt psxy $R $J $X $path2 -h3 -i0,1 -W1p,blue -Bxa5+l"Period (s)" -Bya0.5+l"velocity (km/s)" -BWS+t"Phase weighted stacking result" -X5i >> $PS
                gmt psxy $R $J $X $path2 -h3 -i0,2 -W1p,red -Ben -B0 >> $PS
            fi
        fi
        ##################################################################
        gmt psxy $R $J -O -T >> $PS
        gmt psconvert $PS -E300 -A -Tg -P

        rm -rf  ${PS}
    done
done

rm -rf gmt.*
