
#include <stdio.h>
#include <signal.h>
#include "crinkle.h"
#include "paint.h"
#include "patchlevel.h"
#include "copyright.h"

#define VERSION 2
#define SIDE 1.0

char scroll_Id[]="$Id: xmountains.c,v 1.1 2003/04/08 23:17:26 dustin Exp $";
extern Graph g;
Parm fold_param;
char *display;
extern char *geom;

/* {{{ my version on getopt*/
int optind=1;
char *optarg;
int opterr=1;

int my_getopt (argc, argv, pat)
int argc;
char **argv;
char *pat;
{
  char *flag;

  if((optind >= argc) || (argv[optind][0] != '-'))
  {
    return -1;
  }
  if( argv[optind][1] == '-' )
  {
    optind++;
    return -1;
  }
  if( argv[optind][1] == ':' )
  {
    if( opterr )
    {
      fprintf(stderr,"getopt: found \":\" in optstring\n");
    }
    return '?';
  }
  for(flag=pat;*flag;flag++)
  {
    if( *flag == argv[optind][1] )
    {
      optind++;
      if( *(flag+1) == ':' )
      {
        if(optind >= argc )
        {
          if( opterr )
          {
            fprintf(stderr,"getopt: no option for flag %c\n",*flag);
          }
          return '?';
        }
        optarg = argv[optind];
        optind++;
      }
      return *flag;
    }

  }
  if( opterr )
  {
    fprintf(stderr,"getopt: flag %s not recognized\n",argv[optind]);
  }
  optind++;
  return '?';
}
/* }}} */

double atof();
#ifdef ANSI
void init_graphics (int, int, int *, int *, int, Gun *, Gun *, Gun *);
void clear_col( int );
void finish_graphics();
void plot_pixel (int, int, unsigned char);
void scroll_screen ( int );
void zap_events();
#else
void init_graphics ();
void clear_col();
void finish_graphics();
void plot_pixel ();
void scroll_screen ();
void zap_events();
#endif

void finish_prog();

int s_height=768, s_width=1024;
int mapwid;
#if 0
main (argc,argv)
int argc;
char **argv;
{
  int i,p;
  int e_events=FALSE;
  int request_clear=FALSE;
  int smooth;
  int snooze=10;
  int root= 0;
  int seed=0;

  int c, errflg=0;
  extern char *optarg;
  extern int optind;
  char *mesg[2];
  Gun *clut[3];
  FILE *pidfile;

  init_parameters();
  /* {{{ handle command line flags*/

  mesg[0]="false";
  mesg[1]="true";
  while((c = my_getopt(argc,argv,"bxmqMEHl:r:f:t:I:A:S:T:W:C:a:p:B:n:R:g:d:c:e:v:Z:s:X:Y:P:F:G:"))!= -1)
  {
    switch(c){
      case 'b':
        root = 1- root;
        break;                      /* run on root window */
      case 'x':
        fold_param.cross = 1- fold_param.cross;
        break;                      /* use cross updates */
      case 'E':
        e_events = 1 - e_events;
        break;
      case 'q':
        request_clear = 1 - request_clear;
        break;
      case 'm':                     /* Map view only */
        g.map = 1 - g.map;
        break;
      case 'M':                     /* put in reflections */
        g.reflec = 1 - g.reflec;
        break;
      case 'l':                     /* Set # levels of recursion */
         g.levels = atoi( optarg );
         if( g.levels < 2 )
         {
           g.levels = 2;
         }
         break;
      case 'F':                     /* Set # levels to force front to mean */
         fold_param.force_front = atoi( optarg );
         break;
      case 's':                     /* Set smoothing parameter */
         smooth = atoi( optarg );
         fold_param.rg1 = smooth & 1;
         fold_param.rg2 = smooth & 2;
         fold_param.rg3 = smooth & 4;
         break;
      case 't':                     /* Set width of lowest level */
         g.stop = atoi( optarg );
         if( g.stop < 0 )
         {
           g.stop = 0;
         }
         break;
      case 'r':
         g.repeat = atoi( optarg );
         if( g.repeat < 0 )
         {
           g.repeat = -g.repeat;
           i= -1;
         }else{
           i=1;
         }
         /* we want repeat to be a multiple of 2 as we are using
          * a textured field for the sky.
          */
         g.repeat = i*(2 * ((g.repeat +1)/2));
         break;
      case 'B':                     /* set band_size */
         g.band_size = atoi( optarg );
         if( g.band_size < 2 )
         {
           g.band_size=2;
         }
         g.n_col = (BAND_BASE + (N_BANDS * g.band_size));
         break;
      case 'n':                     /* set max number of colours */
         g.n_col = atoi( optarg );
         if( g.n_col < MIN_COL )
         {
           g.n_col = MIN_COL;
         }
         g.band_size = (g.n_col - BAND_BASE)/N_BANDS;
         g.n_col = (BAND_BASE + (N_BANDS * g.band_size));
         break;
      case 'R':                     /* set seed, read clock if 0 */
         seed = atoi( optarg );
         break;
      case 'Z':                     /* put sleep into wait events */
         snooze = atoi( optarg );
         if( snooze < 0 )
         {
           snooze = 0;
         }
         break;
      case 'P':
         pidfile = fopen(optarg,"w");
         if( pidfile )
         {
           fprintf(pidfile,"%d\n",getpid());
           fclose(pidfile);
         }else{
           perror(optarg);
         }
         break;
      case 'f':                     /* set fractal dimension */
         fold_param.fdim = atof( optarg );
         if( fold_param.fdim < 0.5 )
         {
          fold_param.fdim=0.5;
         }
         if( fold_param.fdim > 1.0 )
         {
          fold_param.fdim=1.0;
         }
         break;
      case 'I':                     /* set Illumination angle */
         g.phi = ((PI * atof( optarg ))/180.0);
         if ( g.phi < 0.0 )
         {
           g.phi=0.0;
         }
         if( g.phi > PI/2.0 )
         {
           g.phi = PI/2.0;
         }
         break;
      case 'A':                     /* set Illumination angle (horizontal)*/
         g.alpha = ((PI * atof( optarg ))/180.0);
         if( g.alpha < -PI/3.0 )
         {
           g.alpha = -PI/3.0;
         }
         if( g.alpha > PI/3.0 )
         {
           g.alpha = PI/3.0;
         }
         break;
      case 'X':                     /* set mix */
         fold_param.mix = atof( optarg );
         break;
      case 'Y':                     /* set midmix */
         fold_param.midmix = atof( optarg );
         break;
      case 'S':                     /* set stretch */
         g.stretch = atof( optarg );
         break;
      case 'W':                     /* set sealevel */
         g.sealevel = atof( optarg );
         break;
      case 'G':                     /* set forceheight */
         fold_param.forceval = atof( optarg );
         break;
      case 'T':                     /* set shift */
         g.base_shift = atof( optarg );
         break;
      case 'C':
         g.contour = atof( optarg );
         break;
      case 'a':                     /* set altitude */
         g.altitude = atof( optarg );
         break;
      case 'p':                     /* set distance */
         g.distance = atof( optarg );
         break;
      case 'c':
         g.contrast = atof( optarg );
         if( g.contrast < 0.0 )
         {
          g.contrast=0.0;
         }
         break;
      case 'e':
         g.ambient = atof( optarg );
         if( g.ambient < 0.0 )
         {
          g.ambient = 0.0;
         }
         if( g.ambient > 1.0 )
         {
          g.ambient=1.0;
         }
         break;
      case 'v':
         g.vfract = atof( optarg );
         if( g.vfract < 0.0 )
         {
          g.vfract = 0.0;
         }
         break;
      case 'g':
         geom = optarg;
         break;
      case 'd':
         display = optarg;
         break;
      case 'H':
         print_algorithm();
         errflg++;
         break;
      case '?':
         errflg++;
    }
  }
  if( errflg )
  {
    fprintf(stderr,"%s: version %d.%d\n",argv[0],VERSION,PATCHLEVEL);
    fprintf(stderr,"usage: %s -[bqgdPEmMrBZIASFTCapcevfRltxsXYH]\n",argv[0]);
    fprintf(stderr," -b       [%s] use root window \n",mesg[root]);
    fprintf(stderr," -q       [%s] reset root window on exit\n",mesg[request_clear]);
    fprintf(stderr," -g string     window geometry\n");
    fprintf(stderr," -d string     display\n");
    fprintf(stderr," -P filename   write PID to file\n");
    fprintf(stderr," -E       [%s] toggle explicit expose events \n",mesg[e_events]);
    fprintf(stderr," -m       [%s] print map \n",mesg[g.map]);
    fprintf(stderr," -M       [%s] implement reflections \n",mesg[g.reflec]);
    fprintf(stderr," -r int   [%d] # columns before scrolling \n",g.repeat);
    fprintf(stderr," -B int   [%d] # shades in a colour band\n",g.band_size);
    fprintf(stderr," -n int   [%d] # number of colours\n",g.n_col);
    fprintf(stderr," -Z int   [%d] time to sleep before scrolling\n",snooze);
    fprintf(stderr," -I float [%f] vertical angle of light \n",(g.phi*180.0)/PI);
    fprintf(stderr," -A float [%f] horizontal angle of light \n",(g.alpha*180.0)/PI);
    fprintf(stderr," -S float [%f] vertical stretch \n",g.stretch);
    fprintf(stderr," -T float [%f] vertical shift \n",g.base_shift);
    fprintf(stderr," -W float [%f] sealevel \n",g.sealevel);
    fprintf(stderr," -F int   [%d] reduce variation in the foreground \n",fold_param.force_front);
    fprintf(stderr," -G float [%f] average foreground height \n",fold_param.forceval);
    fprintf(stderr," -C float [%f] contour parameter \n",g.contour);
    fprintf(stderr," -a float [%f] altitude of viewpoint \n",g.altitude);
    fprintf(stderr," -p float [%f] distance of viewpoint \n",g.distance);
    fprintf(stderr," -c float [%f] contrast\n",g.contrast);
    fprintf(stderr," -e float [%f] ambient light level\n",g.ambient);
    fprintf(stderr," -v float [%f] vertical light level\n",g.vfract);
    fprintf(stderr,"Fractal options:\n");
    fprintf(stderr," -f float [%f] fractal dimension \n",fold_param.fdim);
    fprintf(stderr," -R int   [%d] rng seed, read clock if 0 \n",seed);
    fprintf(stderr," -l int   [%d] # levels of recursion \n",g.levels);
    fprintf(stderr," -t int   [%d] # non fractal iterations \n",g.stop);
    fprintf(stderr," -x       [%s] cross update \n",mesg[fold_param.cross]);
    fprintf(stderr," -s       [%x] smoothing (0-7)\n",smooth);
    fprintf(stderr," -X float [%f] fraction of old value for rg2 & rg3\n",fold_param.mix);
    fprintf(stderr," -Y float [%f] fraction of old value for rg1\n",fold_param.midmix);
    fprintf(stderr," -H            print short description of algorithm.\n");
    exit(1);
  }

  /* }}} */
  for(i=0 ;i<3 ;i++)
  {
    clut[i] = (Gun *) malloc(g.n_col * sizeof(Gun));
    if( ! clut[i] )
    {
      fprintf(stderr,"malloc failed for clut\n");
      exit(1);
    }
  }
  set_clut(g.n_col,clut[0], clut[1], clut[2]);
  init_graphics(root,(! e_events),request_clear,&g.graph_width,&g.graph_height,g.n_col,clut[0],clut[1],clut[2]);
  for(i=0;i<3;i++)
  {
    free(clut[i]);
  }


  seed_uni(seed);

  init_artist_variables();
  if( -1 == (int) signal(SIGINT, finish_prog ))
  {
    perror(argv[0]);
    exit(1);
  }
  if( -1 == (int) signal(SIGTERM, finish_prog ))
  {
    perror(argv[0]);
    exit(1);
  }
  if( -1 == (int) signal(SIGHUP, finish_prog ))
  {
    perror(argv[0]);
    exit(1);
  }
  if( -1 == (int) signal(SIGQUIT, finish_prog ))
  {
    perror(argv[0]);
    exit(1);
  }


  /* This is a stand in for the event loop in a Widget set implementation
   * where we would call plot_column at regular intervals using
   * XTtimeout. However xmountains is an Xlib program
   * so I do the following.
   */
  while( TRUE )
  {
    plot_column(&g);
    zap_events();
#ifndef NO_SLEEP
    if( g.scroll ){
    /* sleep if we are due a scroll next time */
    /* sleeping is very bad because it will prevent
     * events being processed but I suppose it is better
     * than being a CPU hog, as a compremise always check for
     * events at least once a second, looping for longer sleep times.
     * process the events before a sleep to make sure the screen is up to date.
     * the events must always be processed at least once.
     */
      for(i=0;i<snooze;i++){
	sleep(1);
	zap_events();
      }
#endif
    }
  }
}
#endif

extern int quit_xmount;

void finish_prog()
{
  /* The next time zap_events is called the program will quit */
  quit_xmount=TRUE;
}

