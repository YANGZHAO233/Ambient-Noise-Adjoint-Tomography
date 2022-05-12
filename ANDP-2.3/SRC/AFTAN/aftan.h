#ifndef AFTAN_H
#define AFTAN_H 1

/* Finction prorotypes */

void aftanipg_(double *piover4,int *n,float *sei,double *t0,double *dt,
           double *delta,double *vmin,double *vmax,double *tmin,double *tmax,
           double *tresh,double *ffact,double *perc,int *npoints,
           double *taperl,int *nfin,double *snr,double *fmatch,
           int *npred, double pred[2][300],
           int *nprpv,double prpvper[300],double prpvvel[300],
           int *nfout1,double arr1[100][8],int *nfout2,double arr2[100][7],
           double *tamp, int *nrow,int *ncol,double ampo[100][32768],int *ierr);
void aftanpg_(double *piover4,int *n,float *sei,double *t0,double *dt,
           double *delta,double *vmin,double *vmax,double *tmin,double *tmax,
           double *tresh,double *ffact,double *perc,int *npoints,
           double *taperl,int *nfin,double *snr,
           int *nprpv,double prpvper[300],double prpvvel[300],
           int *nfout1,double arr1[100][8],int *nfout2,double arr2[100][7],
           double *tamp, int *nrow,int *ncol, double ampo[100][32768],int *ierr);
void printres_(double *dt,double *delta,int *nfout1,double arr1[100][8],
           int *nfout2,double arr2[100][7],double *tamp, int *nrow,int *ncol,
           double ampo[100][32768],int *ierr, char *name,char *pref);
void readdata_(int *sac,char *name,int *n,double *dt,double *delta,
              double *t0,float sei[32768], int *ierr);
void swapn(unsigned char *b, int N, int nn);

#endif /* !AFTAN_H */
