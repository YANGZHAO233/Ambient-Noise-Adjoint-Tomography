#define MAIN
#include <unistd.h>
#include<stdio.h>
#include<stdlib.h>
#include <math.h>
#include <string.h>
#include "mysac.h"
SAC_HD *read_sac(char *fname, float *sig, SAC_HD *SHD, long nmax)
{
    FILE *fsac;
        fsac = fopen(fname, "rb");
        if ( !fsac )
        {
                printf("the %s dosen't exit!\n",fname);
                exit(0);
        }

        if ( !SHD ) SHD = &SAC_HEADER;
        fread(SHD,sizeof(SAC_HD),1,fsac);
       if ( SHD->npts > nmax )
        {
                SHD->npts = nmax;
        }

        fread(sig,sizeof(float),(int)(SHD->npts),fsac);
        fclose (fsac);
        return SHD;
}
void write_sac(char *fname, float *sig, SAC_HD *SHD)
{
 FILE *fsac;
 //FILE *fp;
 int i,n;

        fsac = fopen(fname, "wb");
        if((fsac = fopen(fname, "wb"))==NULL)
        {
                 printf("could not open sac file to write\n");
                 exit(1);
         }


        if ( !SHD )
        {
                SHD = &SAC_HEADER;
        }


        SHD->iftype = (long)ITIME;
        SHD->leven = (long)TRUE;

        SHD->lovrok = (long)TRUE;
        SHD->internal4 = 6L;




     SHD->depmin = sig[0];
     SHD->depmax = sig[0];
   //fp=fopen("TEST.dat","w");
   for ( i = 0; i < SHD->npts ; i++ )
   {
    if ( SHD->depmin > sig[i] ) SHD->depmin = sig[i];
    if ( SHD->depmax < sig[i] ) SHD->depmax = sig[i];
    //fprintf(fp,"%f   %f\n",i*0.5+250, sig[i]);
   }

         fwrite(SHD,sizeof(SAC_HD),1,fsac);

         n=fwrite(sig,sizeof(float),(int)(SHD->npts),fsac);
	 if(n!=SHD->npts)
	 {
		printf("Error occur when writing the SAC!");
	 	printf("n:%d   SHD->npts:%d\n",n,(int)(SHD->npts));
	 }

        fclose (fsac);
//	fclose(fp);
}
