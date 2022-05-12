#!/usr/bin/env python3

from math import pi, atan, tan, atan2, sin, cos, acos, asin, sqrt, pow
from math import copysign as sign
radius=6378.2064

######################################
def azbaz(alat, alon, blat, blon, deg=True):
    alat = float(alat)
    alon = float(alon)
    blat = float(blat)
    blon = float(blon)

    if alat==blat and alon==blon:
        return (0, 0, 0)
    if deg:
        alat=torad(alat)
        alon=torad(alon)
        blat=torad(blat)
        blon=torad(blon)

    acolat=colat(alat)
    bcolat=colat(blat)
    ac1=sin(acolat)
    ac2=cos(acolat)
    ac3=sin(alon)
    ac4=cos(alon)
    bc1=sin(bcolat)
    bc2=cos(bcolat)
    bc3=sin(blon)
    bc4=cos(blon)

    ae=bc1*bc4
    be=bc1*bc3
    azi1=pow(ae-ac3,2)+pow(be+ac4,2)+bc2*bc2-2.
    azi2=pow(ae-ac2*ac4,2)+pow(be-ac2*ac3,2)+pow(bc2+ac1,2)-2.

    if azi2==0:
        atob=pi-sign(pi/2,azi1)
    else:
        atob=atan2(azi1,azi2)

    codelb=ac1*(ae*ac4+be*ac3)+ac2*bc2
    del1=acos(codelb)
    ass=ac1*ac4
    bs=ac1*ac3
    azi1=pow(ass-bc3,2)+pow(bs+bc4,2)+ac2*ac2-2.
    azi2=pow(ass-bc2*bc4,2)+pow(bs-bc2*bc3,2)+pow(ac2+bc1,2)-2.

    if azi2==0:
        btoa=pi-sign(pi/2,azi1)
    else:
        btoa=atan2(azi1,azi2)
    acolat=pi/2-alat
    bcolat=pi/2-blat

    ac1=sin(acolat)
    ac2=cos(acolat)
    bc1=sin(bcolat)
    bc2=cos(bcolat)
    ae=bc1*bc4
    be=bc1*bc3
    delbod=acos(ac1*(ae*ac4+be*ac3)+ac2*bc2)
    cosdel=cos(delbod)
    x1=(pow(ac2+bc2,2))/(1.+cosdel)
    x2=(pow(ac2-bc2,2))/(1.-cosdel)
    x=x1+x2
    y=x1-x2
    cotdel=1./tan(delbod)
    sindel=sin(delbod)
    del2=delbod*delbod
    A=64.*delbod+16.*del2*cotdel
    D=48.*sindel+8.*del2/cosdel
    B=-(D+D)
    E=30.*sin(delbod+delbod)
    C=-30.*delbod-8.*del2*cotdel-E/2.
    dels=radius*(delbod-.000847518825*(x*delbod-3.*y*sindel)+.0897860195E-6*(x*(A+C*x+D*y)+y*(B+E*y)))
    if deg:
        atob=todeg(atob)
        btoa=todeg(btoa)
    return (atob, btoa, dels)


######################################
def azdiff(az1, az2, deg=False):
    if deg:
        az1=torad(az1)
        az2=torad(az2)
    azdif = abs(az1-az2)
    if azdif>pi:
        azdif=2*pi-azdif
    if azdif<0:
        azdif=-1
    if deg:
        azdif=todeg(azdif)
    return azdif


######################################
def torad(a):
    if a.__class__ is list:
        return [b*pi/180 for b in a]
    else:
        return a*pi/180


######################################
def todeg(a):
    if a.__class__ is list:
        return [b*180/pi for b in a]
    else:
        return a*180/pi


######################################
def colat(a):
    if a.__class__ is list:
        return [pi/2-atan(0.993277*tan(b)) for b in a]
    else:
        return pi/2-atan(0.993277*tan(a))

######################################
def hanning(x, r, p):
    if x >= 0.0 and x <= r:
        return pow((1+cos(pi*x/r))/2, p)
    else:
        print('Error: x must satisfy 0<=x<=r')
        return 0.0
