/*
 * Recursive update procedure for fractal landscapes
 *
 * The only procedures needed outside this file are
 *   make_fold, called once to initialise the data structs.
 *   next_strip, each call returns a new strip off the side of the surface
 *                you can keep calling this as often as you want.
 *   free_strip, get rid of the strip when finished with it.
 *   free_fold,  get rid of the data structs when finished with this surface.
 *
 * Apart from make_fold all these routines get their parameters from their
 * local Fold struct, make_fold initialises all these values and has to
 * get it right for the fractal to work. If you want to change the fractal
 * dim in mid run you will have to change values at every level.
 * each recursive level only calls the level below once for every two times it
 * is called itself so it will take a number of iterations for any changes to
 * be notices by the bottom (long length scale) level.
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "crinkle.h"

char calcalt_Id[] = "$Id: calcalt.c,v 1.1 2003/04/08 08:09:36 dustin Exp $";

#ifdef DEBUG
#define DB(A,B) dump_pipeline(A,B)
#else
#define DB(A,B)
#endif


/* {{{   Strip *make_strip(Fold *f) */
#ifdef ANSI
  Strip *make_strip(Fold *f)
#else
Strip *make_strip (f)
Fold *f;
#endif
{
  Strip *p;
  int n;

  p = (Strip *) malloc( sizeof(Strip) );
  if( p == NULL )
  {
    fprintf(stderr,"make_strip: malloc failed\n");
    exit(1);
  }
  p->f = f;
  n = f->count;
  p->d = (Height *)malloc( n * sizeof(Height) );
  if( p->d == NULL )
  {
    fprintf(stderr,"make_strip: malloc failed\n");
    exit(1);
  }
  return(p);
}

/* }}} */
/* {{{   void free_strip(Strip *p) */
#ifdef ANSI
void free_strip(Strip *p)
#else
void free_strip (p)
Strip *p;
#endif
{
  if( p->d )
  {
    free(p->d);
    p->d = NULL;
  }
  free(p);
}
/* }}} */
/* {{{   Strip *double_strip(Strip *s) */
#ifdef ANSI
Strip *double_strip(Strip *s)
#else
Strip *double_strip (s)
Strip *s;
#endif
{
  Strip *p;
  Height *a, *b;
  int i;
  int count;

  p = make_strip(s->f->parent);
  a = s->d;
  b = p->d;
  count = s->f->count;
  for(i=0; i < count-1; i++)
  {
    *b = *a;
    a++;
    b++;
    *b = 0.0;
    b++;
  }
  *b = *a;
  return(p);
}
/* }}} */
/* {{{   Strip *set_strip(Fold *f, Height value)*/
#ifdef ANSI
Strip *set_strip(Fold *f, Height value)
#else
Strip *set_strip (f,value)
Fold *f;
Height value;
#endif
{
  int i;
  Strip *s;
  Height *h;
  int count;

  s = make_strip(f);
  h = s->d;
  count = f->count;
  for( i=0 ; i < count ; i++)
  {
    *h = value;
    h++;
  }
  return(s);
}

/* }}} */
/* {{{   Strip *random_strip(Fold *f)*/
#ifdef ANSI
Strip *random_strip(Fold *f)
#else
Strip *random_strip(f)
Fold *f;
#endif
{
  Strip *result;
  int i, count;

  result=make_strip(f);
  count = f->count;
  for( i=0 ; i < count ; i++)
  {
    result->d[i] = f->p->mean + (f->scale * gaussian());
  }
  return(result);
}
/* }}} */

/* {{{ void reset_fold(Fold *f) */
#ifdef ANSI
void reset_fold(Fold *f)
#else
void reset_fold(f)
Fold *f;
#endif
{
  /*
   * resets any cached values within the fold structure
   * this should be called if the param struct is changed.
   */
  Length scale, midscale;
  double root2;

  root2=sqrt((double) 2.0 );
  scale = pow((double) f->length, (double) (2.0 * f->p->fdim));
  midscale = pow((((double) f->length)*root2), (double) (2.0 * f->p->fdim));
  f->scale = scale;
  f->midscale = midscale;

  if( f->next ){
    reset_fold(f->next);
  }
}

/* }}} */
/* {{{   Fold *make_fold(Fold *parent,Parm *param, int levels, int stop, Length len) */
/*
 * Initialise the fold structures.
 * As everything else reads the parameters from their fold
 * structs we need to set these here,
 * p  is the parameter struct common to all update levels.
 * levels is the number of levels of recursion below this one.
 *    Number of points = 2^levels+1
 * stop is the number of levels that are generated as random offsets from a
 *    constant rather than from an average.
 * fractal_start, if true we start in the middle of a mountain range
 *    if false we build up mountains from the start height.
 * length is the length of the side of the square at this level.
 *   N.B this means the update square NOT the width of the fractal.
 *   len gets smaller as the level increases.
 * start, the starting height for a non-fractal start.
 */
#ifdef ANSI
Fold *make_fold(Fold *parent,Parm *param, int levels, int stop, Length length)
#else
Fold *make_fold (parent,param,levels,stop,length)
struct fold *parent;
struct parm *param;
int levels;
int stop;
Length length;
#endif
{
  Fold *p;
  int i;

  if( (levels < stop) || (stop<0) )
  {
    fprintf(stderr,"make_fold: invalid parameters\n");
    fprintf(stderr,"make_fold: levels = %d , stop = %d \n",levels,stop);
    exit(1);
  }
  p = (Fold *)malloc(sizeof(Fold));
  if( p == NULL )
  {
    fprintf(stderr,"make_fold: malloc failed\n");
    exit(1);
  }
  p->length=length;
  p->level = levels;
  p->count = (1 << levels) +1;
  p->stop = stop;
  p->state = START;
  p->save =NULL;
  p->p = param;
  for(i=0;i<NSTRIP;i++)
  {
    p->s[i] = NULL;
  }
  p->parent=parent;

  p->next = NULL; /* truncate recursion in reset */
  reset_fold(p);

  if( levels > stop )
  {
    p->next = make_fold(p,param,(levels-1),stop,(2.0*length));
  }else{
    p->next = NULL;
  }
  return( p );
}
/* }}} */
/* {{{   void free_fold(Fold *f) */
#ifdef ANSI
void free_fold(Fold *f)
#else
void free_fold (f)
Fold *f;
#endif
{
  int i;
  if( f->next ){
    free_fold(f->next);
    f->next=NULL;
  }
  if( f->save ){
    free_strip(f->save);
    f->save=NULL;
  }
  for(i=0;i<NSTRIP;i++)
  {
    if( f->s[i] != NULL )
    {
      free_strip(f->s[i]);
      f->s[i] = NULL;
    }
  }
  free(f);
  return;
}
/* }}} */

/* {{{   Strip *next_strip(Fold *fold) */
#ifdef ANSI
Strip *next_strip(Fold *fold)
#else
Strip *next_strip (fold)
Fold *fold;
#endif
{
  Strip *result=NULL;
  Strip *tmp;
  Strip **t;
  int i, iter;
  int count=fold->count;

  if( fold->level == fold->stop)
  {
    /* {{{   generate values from scratch */
    result=random_strip(fold);
    /* }}} */
  }else{
  /*
   * There are two types of strip,
   *  A strips - generated by the lower recursion layers.
   *             these contain the corner points and half the side points
   *  B strips - added by this layer, this contains the mid points and
   *             half the side points.
   *
   * The various update routines test for NULL pointer arguments so
   * that this routine will not fail while filling the pipeline.
   */
    while( result == NULL )
    {
      /* {{{ iterate*/
      switch(fold->state)
      {
        case START:
          /* {{{   perform an update. return first result*/
          DB("S1",fold);
          t=fold->s;
          /* read in a new A strip at the start of the pipeline */
          tmp =next_strip(fold->next);
          t[0] = double_strip(tmp);
          free_strip(tmp);
          /* make the new B strip */
          t[1]=set_strip(fold,0.0);
          if( ! t[2] )
          {
            /* we want to have an A B A pattern of strips at the
             * start of the pipeline.
             * force this when starting the pipe
             */
            t[2]=t[0];
            tmp =next_strip(fold->next);
            t[0] = double_strip(tmp);
            free_strip(tmp);
          }
          DB("E1",fold);

          /*
           * create the mid point
           * t := A B A
           */
          DB("S2",fold);
          x_update(fold,fold->midscale,0.0,t[0],t[1],t[2]);
          DB("E2",fold);

          if(fold->p->rg1)
          {
            DB("S3",fold);
            /*
             * first possible regeneration step
             * use the midpoints to regenerate the corner values
             * increment t by 2 so we still have and A B A pattern
             */
            if( t[3] == NULL )
            {
              /* rather than do no update add offset to old value */
              v_update(fold,fold->midscale,1.0,t[1],t[2],t[1]);
            }else{
              v_update(fold,fold->midscale,fold->p->midmix,t[1],t[2],t[3]);
            }
            t+=2;
            DB("E3",fold);

          }

          /*
           * fill in the edge points
           * increment t by 2 to preserve the A B A pattern
           */
          DB("S4",fold);

          if( fold->p->cross )
          {
            t_update(fold,fold->scale,0.0,t[0],t[1],t[2]);
            p_update(fold,fold->scale,0.0,t[1],t[2],t[3]);
            t+=2;
          }else{
            hside_update(fold,fold->scale,0.0,t[0],t[1],t[2]);
            vside_update(fold,fold->scale,0.0,t[2]);
            t+=2;
          }
          DB("E4",fold);

          if(fold->p->rg2)
          {
            DB("S5",fold);
            /*
             * second regeneration step update midpoint
             * from the new edge values
             */
            if( fold->p->cross )
            {
              if( t[2] == NULL )
              {
                /* add random offset to old rather than skip update */
                p_update(fold,fold->scale,fold->p->mix,t[0],t[1],t[0]);
              }else{
                p_update(fold,fold->scale,fold->p->mix,t[0],t[1],t[2]);
              }
            }else{
              vside_update(fold,fold->scale,fold->p->mix,t[1]);
            }
            DB("E5",fold);

          }
          /* increment t by 1
           * this gives a B A B pattern to regen-3
           * if regen 3 is not being used it leaves t pointing to the
           * 2 new result strips
           */
          t++;
          if(fold->p->rg3)
          {
            DB("S6",fold);

            /* final regenration step
             * regenerate the corner points from the new edge values
             * this needs a B A B pattern
             * leave t pointing to the 2 new result strips
             *
             * this has to be a t_update
             */
            if( t[2] == NULL )
            {
              /* add random offset to old rather than skip update */
              t_update(fold,fold->scale,1.0,t[0],t[1],t[0]);
            }else{
              t_update(fold,fold->scale,fold->p->mix,t[0],t[1],t[2]);
            }
            t++;
            DB("E6",fold);

          }
          result=t[1];
          fold->save=t[0];
          t[0]=t[1]=NULL;
          fold->state = STORE;
          break;
          /* }}} */
        case STORE:
          /* {{{   return second value from previous update. */
          result = fold->save;
          fold->save=NULL;
          for(i=NSTRIP-1;i>1;i--)
          {
            fold->s[i] =fold->s[i-2];
          }
          fold->s[0] = fold->s[1]=NULL;
          fold->state = START;
          break;
          /* }}} */
        default:
          fprintf(stderr,"next_strip: invalid state level %d state %d\n",
               fold->level,fold->state);
          exit(3);
      }
      /* }}} */
    }
  }
  iter = fold->level - fold->stop;
  if( fold->p->force_front > iter){
   result->d[0] = fold->p->forceval;
  }
  if( fold->p->force_back > iter){
    result->d[count-1] = fold->p->forceval;
  }
  return(result);
}
/* }}} */

/* {{{ void x_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)*/
#ifdef ANSI
void x_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)
#else
void x_update(fold, scale, mix, a, b, c)
Fold *fold;
float scale;
float mix;
Strip *a;
Strip *b;
Strip *c;
#endif
{
  int i;
  int count=fold->count;
  float w;
  Height *mp, *lp, *rp;

  /* don't run unless we have all the parameters */
  if( !a || !c ) return;
  if( !b )
  {
    fprintf(stderr,"x_update: attempt to update NULL strip\n");
    exit(1);
  }

  w = (1.0 - mix)*0.25;
  mp=b->d;
  lp=a->d;
  rp=c->d;

  if( mix <= 0.0 ){
    /* {{{ random offset to average of new points*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = 0.25 * ( lp[0] + rp[0] + lp[2] + rp[2])
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else if( mix >= 1.0 ){
    /* {{{ random offset to old value*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = mp[1]
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else{
    /* {{{ mixed update*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = (mix * mp[1]) + w * ( lp[0] + rp[0] + lp[2] + rp[2])
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }
}
/* }}} */
/* {{{ void p_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)*/
#ifdef ANSI
void p_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)
#else
void p_update(fold, scale, mix, a, b, c)
Fold *fold;
float scale;
float mix;
Strip *a;
Strip *b;
Strip *c;
#endif
{
  int i;
  int count=fold->count;
  float w;
  Height *mp, *lp, *rp;

  /* don't run if we have no parameters */
  if( !a || !b ) return;

  /* if c is missing we can do a vside update instead
   * should really be a sideways t but what the heck we only
   * need this at the start
   */
  if( !c )
  {
    vside_update(fold,scale,mix,b);
    return;
  }

  w = (1.0 - mix)*0.25;
  mp=b->d;
  lp=a->d;
  rp=c->d;

  if( mix <= 0.0 ){
    /* {{{ random offset to average of new points*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = 0.25 * ( lp[1] + rp[1] + mp[0] + mp[2] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else if(mix >= 1.0){
    /* {{{ random offset to old values*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = mp[1]
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else{
    /* {{{ mixed update*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = (mix * mp[1]) + w * ( lp[1] + rp[1] + mp[0] + mp[2] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }
}

/* }}} */
/* {{{ void t_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)*/
#ifdef ANSI
void t_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)
#else
void t_update(fold, scale, mix, a, b, c)
Fold *fold;
float scale;
float mix;
Strip *a;
Strip *b;
Strip *c;
#endif
{
  int i;
  int count=fold->count;
  float w, we;
  Height *mp, *lp, *rp;
  float third=(1.0/3.0);

  /* don't run unless we have all the parameters */
  if( !a || !c ) return;
  if( !b )
  {
    fprintf(stderr,"t_update: attempt to update NULL strip\n");
    exit(1);
  }

  w = (1.0 - mix)*0.25;
  we = (1.0 - mix)*third;
  mp=b->d;
  lp=a->d;
  rp=c->d;

  if( mix <= 0.0){
    /* {{{ random offset to average of new points*/
    mp[0] = third * ( lp[0] + rp[0] + mp[1] )
            + (scale * gaussian());
    mp++;
    lp++;
    rp++;
    for(i=1; i<count-3; i+=2)
    {
      mp[1] = 0.25 * ( lp[1] + rp[1] + mp[0] + mp[2] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    mp[1] = third * ( lp[1] + rp[1] + mp[0] )
          + (scale * gaussian());
    /* }}} */
  }else if(mix >= 1.0){
    /* {{{ random offset to old values*/
    for(i=0; i<count; i+=2)
    {
      mp[0] = mp[0]
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else{
    /* {{{ mixed update*/
    mp[0] = (mix * mp[0]) + we * ( lp[0] + rp[0] + mp[1] )
            + (scale * gaussian());
    mp++;
    lp++;
    rp++;
    for(i=1; i<count-3; i+=2)
    {
      mp[1] = (mix * mp[1]) + w * ( lp[1] + rp[1] + mp[0] + mp[2] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    mp[1] = (mix * mp[1]) + we * ( lp[1] + rp[1] + mp[0] )
          + (scale * gaussian());
    /* }}} */
  }
}


/* }}} */
/* {{{ void v_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)*/
#ifdef ANSI
void v_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)
#else
void v_update(fold, scale, mix, a, b, c)
Fold *fold;
float scale;
float mix;
Strip *a;
Strip *b;
Strip *c;
#endif
{
  int i;
  int count=fold->count;
  float w, we;
  Height *mp, *lp, *rp;

  /* don't run unless we have all the parameters */
  if( !a || !c ) return;
  if( !b )
  {
    fprintf(stderr,"v_update: attempt to update NULL strip\n");
    exit(1);
  }

  w = (1.0 - mix)*0.25;
  we = (1.0 - mix)*0.5;
  mp=b->d;
  lp=a->d;
  rp=c->d;

  if( mix <= 0.0){
    /* {{{ random offset of average of new points*/
    mp[0] = 0.5 * ( lp[1] + rp[1] )
            + (scale * gaussian());
    mp++;
    lp++;
    rp++;
    for(i=1; i<count-3; i+=2)
    {
      mp[1] = 0.25 * ( lp[0] + rp[0] + lp[2] + rp[2] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    mp[1] = 0.5 * ( lp[0] + rp[0] )
            + (scale * gaussian());
    /* }}} */
  }else if(mix >= 1.0){
    /* {{{ random offset to old values*/
    for(i=0; i<count; i+=2)
    {
      mp[0] = mp[0]
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else{
    /* {{{ mixed update*/
    mp[0] = (mix * mp[0]) + we * ( lp[1] + rp[1] )
            + (scale * gaussian());
    mp++;
    lp++;
    rp++;
    for(i=1; i<count-3; i+=2)
    {
      mp[1] = (mix * mp[1]) + w * ( lp[0] + rp[0] + lp[2] + rp[2] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    mp[1] = (mix * mp[1]) + we * ( lp[0] + rp[0] )
            + (scale * gaussian());
    /* }}} */
  }
}

/* }}} */
/* {{{ void vside_update(Fold *fold,float scale, float mix, Strip *a)*/
#ifdef ANSI
void vside_update(Fold *fold,float scale, float mix, Strip *a)
#else
void vside_update(fold, scale, mix, a)
Fold *fold;
float scale;
float mix;
Strip *a;
#endif
{
  int i;
  int count=fold->count;
  float w;
  Height *mp;

  /* don't run unless we have all the parameters */
  if( !a ) return;


  w = (1.0 - mix)*0.5;
  mp=a->d;

  if( mix <= 0.0){
    /* {{{ random offset to average of new points*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = 0.5 * ( mp[0] + mp[2] )
            + (scale * gaussian());
      mp+=2;
    }
    /* }}} */
  }else if(mix >= 1.0){
    /* {{{ random offset to old values*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = mp[1]
            + (scale * gaussian());
      mp+=2;
    }
    /* }}} */
  }else{
    /* {{{ mixed update*/
    for(i=0; i<count-2; i+=2)
    {
      mp[1] = (mix * mp[1]) + w * ( mp[0] + mp[2] )
            + (scale * gaussian());
      mp+=2;
    }
    /* }}} */
  }
}


/* }}} */
/* {{{ void hside_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)*/
#ifdef ANSI
void hside_update(Fold *fold,float scale, float mix, Strip *a, Strip *b, Strip *c)
#else
void hside_update(fold, scale, mix, a, b, c)
Fold *fold;
float scale;
float mix;
Strip *a;
Strip *b;
Strip *c;
#endif
{
  int i;
  int count=fold->count;
  float w;
  Height *mp, *lp, *rp;

  /* don't run unless we have all the parameters */
  if( !a || !c ) return;
  if( !b )
  {
    fprintf(stderr,"x_update: attempt to update NULL strip\n");
    exit(1);
  }

  w = (1.0 - mix)*0.5;
  mp=b->d;
  lp=a->d;
  rp=c->d;

  if( mix <= 0.0 ){
    /* {{{ random offset to average of new points*/
    for(i=0; i<count; i+=2)
    {
      mp[0] = 0.5 * ( lp[0] + rp[0] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else if(mix >= 1.0){
    /* {{{ random offset to old points*/
    for(i=0; i<count; i+=2)
    {
      mp[0] = mp[0]
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }else{
    /* {{{ mixed update*/
    for(i=0; i<count; i+=2)
    {
      mp[0] = (mix * mp[0]) + w * ( lp[0] + rp[0] )
            + (scale * gaussian());
      mp+=2;
      lp+=2;
      rp+=2;
    }
    /* }}} */
  }
}



/* }}} */


#ifdef DEBUG
dump_pipeline(s, f)
char *s;
Fold *f;
{
  printf("%s[%d]: %0xd %0xd %0xd %0xd %0xd %0xd %0xd %0xd\n",s,f->level,
     f->s[0],f->s[1],f->s[2],f->s[3],f->s[4],f->s[5],f->s[6],f->s[7]);
}
#endif
