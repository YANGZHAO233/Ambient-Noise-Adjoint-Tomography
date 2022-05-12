#include<stdio.h>
#include<math.h>
#include <time.h>
#include<stdlib.h>
#include<string.h>
#include<fftw3.h>
#include"mysac.h"
#include<omp.h>
#include <unistd.h>
#define E 1.0e-4
#define MAINE
#define Nmax 32768
#define USAGE "[-B/-E: minimum or maximum period for group or phase velocity] \
[-F: input file] [-O: output file] [-W: the exponential term of the weight]\n [-I: 1 or 2 inverse algorithm]"
#define WP 1
#define SP 1

// Compile: gcc tf-PWS.c S-transform.c sacrw.c -lfftw3 -lm -o tf-PWS

/*
 * 	This code is written for tf-PWS based on Stockwell et al. 1996
 * 	USAGE:
 * 	  Default paramenters:
 * 	        A.   Normalization and smooth of the weight funciton is turned off, if you want to turn on them,
 *               you can appendix -N1 and -S10 in the tail tf-PWS;
 *          B.   The default weight is set to 1, if you want use other value(for example n), you can appendix
 *               -Wn in the tail of tf-PWS;
 *          C.   The default inverse algorithm is defined by stockwell et al. 1996, and if you want to use the
 *               inverse algorithm proposed by Schimmel and Gallart 2005, you can appendix -I2 in the tail of
 *               tf-PWS.
 *          D.   The default frequent band is set from 0-Nyquist Hz, if you want to sepcifiy in some frequency
 *               band (for example from 10s to 100s), you can appendix -B10 -E100s in the tail.
 *          E.   This code support the parallel computing, and the default CPU number is set to 4, you can change
 *               that value too.
 *
 *        Examples:
 * 	        (1) ls * | ./tf-PWS
 * 	       	        to perform the line and tf-PWS of all the input files, and all other paramenter
 * 	                are set as default values;
 * 	        (2) ls * > input.dat
 * 	                ./tf-PWS -Finput.dat -B10  -E100  -N1 -S10 -I1 -W1
 *	                 In this case, the interested band is 10~100s but not 0-Nyquist. By this mean, you can reduce the
 *	                 calcaulation time, and filter the data in band 10~100s.
 *
 * 	Written by Guoliang Li, guoleonlee@gmail.com
 * 	Finish data: 2016.01.10
 * 	 recitified 2016.08.09  Adding parallel computing part, and the number of parallel CPU is 4.
 * 	 recitified 2016.09.09  Fix the bug of storage overflowing.
 * 	 modified on 2016.09.20 adding the second inverse algorithm proposed by Schimmel and Gallart 2005;
 */

int getopt();
extern      char *optarg;
extern      int optind;

int main(int arg, char *arv[])
{
    int i,n,m,freq,nfft,npts,tag;
    int nstack,flag=0,I;
    int smooth, normalize, user1;
    int lo,hi,lag,N,npow=1;
    float *sigz,**real,**img;
    float MinP,MaxP,weight;
    float b;
    double *sig,*trans,*stack;
    double pi,df,dt;
    double fmax,fa,phase,f1,f2,f3,f4;
    double noreal,noimg;
    char infn[200],sacfn[200],outfn[200],outfile[200];
    FILE *fp,*fp1,*fp2,*fpp;
    SAC_HD shdz;
    int ispws;

    pi=atan(1.0)*4.0;

    if(arg<1)
    {
        printf(USAGE);
        exit(0);
    }

    /* Setting the default parameters*/
    strcpy(outfile,"untitled");
    weight=1.0;
    smooth=0;
    normalize=0;
    MinP=0.0;
    MaxP=0.0;
    f1=0;
    f4=0;
    I=1;
    tag=0;
    ispws=0;

    while((lag=getopt(arg, arv, "B:E:F:O:W:S:N:I:P:h" )) != (-1) )
    {
        switch(lag)
        {
                case 'B':
                        MinP=atof(optarg);
                        break;
                case 'E':
                        MaxP=atof(optarg);
                        break;
                case 'F':
                        strcpy(infn,optarg);
			            tag=1;
                        break;
                case 'O':
                        strcpy(outfile,optarg);
                        break;
                case 'W':
                        weight=atof(optarg);
                        break;
		        case 'S':
                        smooth=atoi(optarg);
                        break;
		        case 'N':
                        normalize=atoi(optarg);
                        break;
		        case 'I':
			            I=atoi(optarg);
                        break;
                case 'P':
                        ispws=atoi(optarg);
                        break;
                //default:
                case 'h':
                        printf(USAGE);
                        exit(0);
        }
    }


    // printf("normalize=%d\n",normalize);
    // printf("smooth=%d\n",smooth);

    if(MaxP<MinP)
    {
        printf("The input paraments are wrong:MaxP < MinP\n");
        exit(2);
    }

    if( MaxP-MinP>10e-5 )
    {
    	f1=1.0/MaxP;
    	f4=1.0/MinP;
    }

    sigz=(float *)malloc(Nmax*sizeof(float));

    //Open the input file
    if(tag==1)
    {
    	if((fp=fopen(infn,"r"))==NULL)
    	{
       	 	printf("The file %s cannot be read!\n",infn);
        	exit(1);
    	}
    }
    else
    {
        fp=stdin;
    }

    //Start: calculate the weights and the line stack//
    nstack=0;
    user1=0;

    if (ispws < 1)
    {
        for (i=0;;i++)
        {
            if(fscanf(fp,"%s",sacfn)==EOF)
        	{
        		// printf("read over\n");
        		break;
            }

            read_sac(sacfn, sigz, &shdz, Nmax);

            if(flag!=1)
            {
                npts=shdz.npts;
                dt=shdz.delta;
                b=shdz.b;

                stack=(double *)malloc(npts*sizeof(double));
                memset(stack,0,sizeof(double)*npts);

        		flag=1;
            }

        	for(n=0;n<npts;n++)
            {
                stack[n]=stack[n]+(double)sigz[n];
            }

            nstack = nstack + 1;
        }

        for(n=0;n<npts;n++)
        {
            sigz[n]=(float)stack[n];
        }

        shdz.user0=nstack;

        sprintf(outfn,"%s%s",outfile,"_ls.SAC");
        write_sac(outfn,sigz,&shdz);

        return;
    }

    for(i=0;;i++)
    {

    	if(fscanf(fp,"%s",sacfn)==EOF)
    	{
    		// printf("read over\n");
    		break;
        }

        if(access(sacfn,F_OK)!=0) continue;
        // printf("%s\n",sacfn);

    	read_sac(sacfn, sigz, &shdz, Nmax);

        //Distribute the storage space and parameters according to the npts of input series//
        if(flag!=1)
        {
            npts=shdz.npts;
            dt=shdz.delta;
            b=shdz.b;

            //calculate the points for fft//
            nfft=1;
            while(nfft<npts)
    		{
        		nfft=nfft*2;
    		}
    		df=1.0/(nfft*dt);

            // printf("npts:%d   nfft:%d\n",npts,nfft);

            if(nfft>Nmax)
            {
    			// printf("The nfft:%d is greater than Nmax:%d\n",nfft,Nmax);
    			exit(2);
            }

            //set lo: low frequency and hi:high frequency for ST and IST//
            lo=(int)(f1/df);
            hi=(int)(f4/df+0.5);

            if(lo==0 && hi==0)
            {
            	lo=0;
                hi=(int)(1.0/(2.0*dt*df));  // 1.0/(2.0*dt) is the Nyquist frequency
            	f4=hi*df;
            }

            // printf("f1=%f  f4=%f  lo=%d  hi=%d\n",f1,f4,lo,hi);

    		N=(hi-lo+1)*nfft;
    		// printf("N=%d\n",N);

            //strore the line_stack parts, double
            stack=(double *)malloc(nfft*sizeof(double));
            memset(stack,0,sizeof(double)*nfft);

            //To transfer input data to ST and IST, double
            sig=(double *)malloc(nfft*sizeof(double));
            trans=(double *)calloc(N*2,sizeof(double));
            memset(trans,0,sizeof(double)*2*N);

            //To store the weights
            real=(float **)malloc((hi-lo+1)*sizeof(float *));
    		img=(float **)malloc((hi-lo+1)*sizeof(float *));
    		for(n=0;n<hi-lo+1;n++)
    		{
        		real[n]=(float *)malloc(nfft*sizeof(float));
        		img[n]=(float *)malloc(nfft*sizeof(float));
        		memset(real[n],0,sizeof(float)*nfft);
        		memset(img[n],0,sizeof(float)*nfft);
    		}

    		flag=1;
    		// printf("Set storage successfully!\n");
        }

    	npts=(npts==shdz.npts?npts:0);
    	if(npts==0)
    	{
    		// printf("There some thing wrong with the npst for:%s!\n",sacfn);
    		// printf("NPTS:%d  %d\n",shdz.npts,npts);
    		exit(3);
    	}

    	//zero fill//
    	for(n=npts;n<nfft;n++)
    		sigz[n]=0.0;

    	//pre-filter//
        //filter4_(&f1,&f2,&f3,&f4,&npow,&dt,&nfft,sigz,sigz);

    	//float transfer to double//
    	for(n=0;n<nfft;n++)
        {
            sig[n]=sigz[n];
        }

    	//st(int len, int lo, int hi, double *data, double *result)
    	//./Stack_before0920/ZZ.tf_PWS_stack_4.SAC
    	//printf("sig:%d %d, trans:%d %d\n",&sig[0],&sig[Nmax],&trans[0],&trans[N]);
    	Strans(nfft, lo, hi, df, &sig[0], &trans[0]);

        //	fpp=fopen("test.dat","w");

    	#pragma omp parallel for private(m,n,fa,noreal,noimg) num_threads(4)
    	for(m=0;m<hi-lo+1;m++)
    	{
    		for(n=0;n<nfft;n++)
    		{
    			fa=sqrt(pow(trans[2*m*nfft+2*n],2.0)+pow(trans[2*m*nfft+2*n+1],2.0));
    			noreal=trans[2*m*nfft+2*n]/fa;
    			noimg=trans[2*m*nfft+2*n+1]/fa;

    			//To prevent the emergence of the 0/0=Nan
    			if(fabs(noreal)>1.0 || fabs(noimg)>1.0)
    			{
    				real[m][n]=real[m][n]+0;
    				img[m][n]=img[m][n]+0;
    				// printf("n=%d\n",n);
    				// getchar();
    			}
    			else
    			{
    				real[m][n]=real[m][n]+noreal;
    				img[m][n]=img[m][n]+noimg;
    			}
    			/*
    			noreal=trans[2*m*nfft+2*n];
    			noimg=trans[2*m*nfft+2*n+1];
    			real[m][n]=real[m][n]+noreal;
    			img[m][n]=img[m][n]+noimg;
    			*/
                //	fprintf(fpp,"m=%d n=%d  num=%d, fa=%e,%e %e\n",m,n,2*m*nfft+2*n,fa,trans[2*m*nfft+2*n]/fa,trans[2*m*nfft+2*n+1]/fa);
    		}
    		//printf("f=%7.5lf T=%7.5lf, i=%d\n",(m+lo)*df,1.0/((m+lo)*df),m);
    		//getchar();
    	}
    	//	fclose(fpp);

    	//The line stack part, from float to double//
    	for(n=0;n<nfft;n++)
        {
            stack[n]=stack[n]+sig[n];
        }

    	// The variable user1 is used to store the stack times of tf-PWS, and finally are written in the SAC head as shdz.user1//
    	if( shdz.user1 <= 0 )
        {
            user1=user1+1;
        }
    	else
        {
            user1=user1+shdz.user1;
        }

        nstack++;
    }

    fclose(fp);

    if(nstack==0)
    {
        return 0;
    }


    //calculate the spectrum of the line_stack part//
    Strans(nfft,lo,hi,df,stack,&trans[0]);

    //calculate the weights//
    #pragma omp parallel for private(i,n,m,fmax) num_threads(4)
    for(i=0;i<hi-lo+1;i++)
    {
        fmax=0.0;
        for(n=0;n<nfft;n++)
        {
            //real[i][n]=pow((pow(real[i][n],2.0)+pow(img[i][n],2.0)),weight)/nstack;
            real[i][n]=real[i][n]/nstack;
            img[i][n] = img[i][n]/nstack;
            real[i][n]=pow((pow(real[i][n],2.0)+pow(img[i][n],2.0)),weight/2.0);
            if(real[i][n]>fmax)
            {
                fmax=real[i][n];
            }
        }

    	// do normalize along time axis//
    	if(normalize==1)
    	{
    		for(n=0;n<nfft;n++)
    		{
    			real[i][n]=real[i][n]/fmax;
    		}
    	}
     }

    //do smoothing along the time axis//
    if(smooth>1)
    {
        smooth=5;
        // printf("Begin smoothing\n");

        #pragma omp parallel for private(i,n,m,fa,lag,fmax) num_threads(4)
        for(n=0;n<hi-lo+1;n++)
        {
            for(i=0;i<nfft;i++)
            {
                fa=0.0;
                lag=0;
                for(m=-smooth;m<=smooth;m++)
                {
                    if( i+m>0 && i+m < nfft)
                    {
                        fa=fa+real[n][i+m];
                        lag++;
                    }
                }

                if(lag>0)
                {
                    img[n][i]=fa/lag;
                }

                if(fmax<img[n][i])
                {
                    fmax=img[n][i];
                }
            }
        }

        //real=img;
        for(n=0;n<hi-lo+1;n++)
        {
            for(i=0;i<nfft;i++)
            {
                real[n][i]=img[n][i];
            }
        }
     }

    #ifdef WP
    // if((fp1=fopen("weight.dat","w"))==NULL)
    // {
    //     printf("The file: weight.dat cannot be created!\n");
    //     exit(3);
    // }
    //
    // for(i=0;i<hi-lo+1;i++)
    // {
    //     for(n=0;n<nfft;n++)
    //     fprintf(fp1,"%10.7f %10.7f  %10.7f\n",b+n*dt, f1+i*df ,real[i][n]);
    // }
    //
    // fclose(fp1);
    #endif

    //revise the spectrum of the line_stack part//
    #ifdef SP
    // fp=fopen("Before_Filter.dat","w");
    // fp2=fopen("After_Filter.dat","w");
    #endif

    //#pragma omp parallel for private(i,n,fa) num_threads(4)
    for(i=0;i<hi-lo+1;i++)
    {
        for(n=0;n<nfft;n++)
        {
            #ifdef SP
            fa=sqrt(pow(trans[2*i*nfft+2*n],2)+pow(trans[2*i*nfft+2*n+1],2));
            phase=atan2(trans[2*i*nfft+2*n],trans[2*i*nfft+2*n+1]);
            // fprintf(fp,"%10.7f %10.7f %10.7lf %10.7lf\n",b+n*dt, f1+i*df, fa, phase);
            #endif

            // Add the weight to the spectrum of the linear stacking result
            trans[2*i*nfft+2*n]=trans[2*i*nfft+2*n]*real[i][n];
            trans[2*i*nfft+2*n+1]=trans[2*i*nfft+2*n+1]*real[i][n];

            #ifdef SP
            fa=sqrt(pow(trans[2*i*nfft+2*n],2)+pow(trans[2*i*nfft+2*n+1],2));
            phase=atan2(trans[2*i*nfft+2*n],trans[2*i*nfft+2*n+1]);
            // fprintf(fp2,"%10.7f %10.7f %10.7lf %10.7lf\n",b+n*dt, f1+i*df,fa,phase);
            #endif

            //	if(n<1200 && n>800)
            //	printf("i=%d, n=%d, %7.5lf  %7.5lf  %7.5lf\n",i,n,trans[2*i*nfft+2*n],trans[2*i*nfft+2*n+1],real[i][n]);
        }
    //printf("fa=%7.5lf\n",fa);
    //printf("f=%7.5lf T=%7.5lf, i=%d\n",(i+lo)*df,1.0/((i+lo)*df),i);
    //getchar();
    }

    for(i=0;i<nfft;i++)
    {
        sig[i]=0.0;
    }

    shdz.user0=nstack;
    //shdz.npts=11000;

    for(i=0;i<nfft;i++)
    {
        sigz[i]=stack[i];
    }

    sprintf(outfn,"%s%s",outfile,"_ls.SAC");
    write_sac(outfn,sigz,&shdz);


    /*Perform the inverse tf-PWS:The inverse algorithm of iStrans is based on Stockwell er al. 1996,
    while that of  iStrans2 is based on Schimmel et al. 2005, but we recommond the first inverse algorithm*/
    // printf("lo=%d, hi=%d, nfft=%d\n",lo, hi, nfft);


    if(I==1) iStrans(nfft, lo, hi, df, &trans[0], stack);

    if(I==2) iStrans2(nfft, lo, hi, df, &trans[0], stack);

    for(i=0;i<nfft;i++)
    {
        sigz[i]=stack[i];
        if(sigz[i]>10e8)
        {
            // printf("%d\n",i);
            // getchar();
        }
    }
    // filter4_(&f1,&f2,&f3,&f4,&npow,&dt,&nfft,sigz,sigz);

    // printf("npts=%d  shdz.npts:%d\n",npts,shdz.npts);
    // printf("b:%f   e:%f   delta:%f\n",shdz.b,shdz.e,shdz.delta);

    // fp=fopen("check.dat","w");
    // for(i=0;i<npts;i++)
    // {
    //     fprintf(fp,"%d   %d   %f\n",npts, i,sigz[i]);
    // }

    sprintf(outfn,"%s%s",outfile,"_pws.SAC");
    write_sac(outfn,sigz,&shdz);


    //Free storage space//
    free(sigz);
    free(sig);
    free(trans);
    free(stack);
    for(i=0;i<hi-lo+1;i++)
    {
    	free(real[i]);
    	free(img[i]);
    }
    free(real);
    free(img);

    return 0;

}
