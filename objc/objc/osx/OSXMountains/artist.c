/*
 *   routines to render a fractal landscape as an image
 */
#include <math.h>
#include <stdio.h>
#include "paint.h"
#include "crinkle.h"

char artist_Id[] = "$Id: artist.c,v 1.1 2003/04/08 08:09:36 dustin Exp $";
#define SIDE 1.0
#ifndef PI
#define PI 3.14159265
#endif



int base=0;      /* parity flag for mirror routine */

extern Parm fold_param;
extern Graph g;

Fold *top;


Height varience;
Height delta_shadow;
Height shift;
double shadow_slip;
double shadow_register;
double cos_phi;
double sin_phi;
double tan_phi;
double x_fact;
double y_fact;
double vangle;
double vscale;
double tan_vangle;
float viewpos;        /* position of viewpoint */
float viewheight;      /* height of viewpoint */
float focal;
float vstrength; /* strength of vertical light source */
float lstrength; /* strength of vertical light source */



Height *shadow;               /* height of the shadows */
Height *a_strip, *b_strip;    /* the two most recent strips */


float uni();
/* {{{   void set_clut(int max_col, Gun *red, Gun *green, Gun *blue)*/
/*
 * setup the colour lookup table
 */
void set_clut (max_col,red,green,blue)
int max_col;
Gun *red;
Gun *green;
Gun *blue;
{
  int band,shade;
  float top, bot;
  float intensity;
  int tmp;
/*
*  float rb[N_BANDS] = { 0.167,0.200,0.333,0.450,0.600,1.000 };
*  float gb[N_BANDS] = { 0.667,0.667,0.500,0.500,0.600,1.000 };
*  float bb[N_BANDS] = { 0.500,0.450,0.333,0.200,0.000,1.000 };
*/

  float rb[N_BANDS];
  float gb[N_BANDS];
  float bb[N_BANDS];

  /* band base colours as RGB fractions */
  rb[0] = 0.450; rb[1] = 0.600; rb[2] = 1.000;
  gb[0] = 0.500; gb[1] = 0.600; gb[2] = 1.000;
  bb[0] = 0.333; bb[1] = 0.000; bb[2] = 1.000;

  /* {{{   black */
  red[BLACK]       = 0;
  green[BLACK]     = 0;
  blue[BLACK]      = 0;
  /* }}} */
  /* {{{   white */
  red[WHITE]       = COL_RANGE;
  green[WHITE]     = COL_RANGE;
  blue[WHITE]      = COL_RANGE;
  /* }}} */
  /* {{{   sky*/
  red[SKY]         = 0.404*COL_RANGE;
  green[SKY]       = 0.588*COL_RANGE;
  blue[SKY]        = COL_RANGE;
  /* }}} */
  /* {{{   sea (lit) */
  red[SEA_LIT]     = 0;
  green[SEA_LIT]   = 0.500*COL_RANGE;
  blue[SEA_LIT]    = 0.700*COL_RANGE;
  /* }}} */
  /* {{{   sea (unlit)*/
  red[SEA_UNLIT]   = 0;
  green[SEA_UNLIT] = ((g.ambient+(g.vfract/(1.0+g.vfract)))*0.500)*COL_RANGE;
  blue[SEA_UNLIT]  = ((g.ambient+(g.vfract/(1.0+g.vfract)))*0.700)*COL_RANGE;
  /* }}} */

  if( MIN_COL > max_col )
  {
    fprintf(stderr,"set_clut: less than the minimum number of colours available\n");
    exit(1);
  }
  /* max_col can over-rule band_size */
  while( (BAND_BASE +g.band_size*N_BANDS) > max_col )
  {
    g.band_size--;
  }

  for( band=0 ; band<N_BANDS; band++)
  {
    for(shade=0 ; shade < g.band_size ; shade++)
    {
      if( (BAND_BASE + (band*g.band_size) + shade) >= max_col )
      {
        fprintf(stderr,"INTERNAL ERROR, overflowed clut\n");
        exit(1);
      }
      /* {{{   set red */
      top = rb[band];
      bot = g.ambient * top;
      intensity = bot + ((shade * (top - bot))/(g.band_size-1));
      tmp = COL_RANGE * intensity;
      if (tmp < 0)
      {
        fprintf(stderr,"set_clut: internal error: invalid code %d\n",tmp);
        exit(2);
      }
      if( tmp > COL_RANGE )
      {
        tmp = COL_RANGE;
      }
      red[BAND_BASE + (band*g.band_size) + shade] = tmp;
      /* }}} */
      /* {{{   set green */
      top = gb[band];
      bot = g.ambient * top;
      intensity = bot + ((shade * (top - bot))/(g.band_size-1));
      tmp = COL_RANGE * intensity;
      if (tmp < 0)
      {
        fprintf(stderr,"set_clut: internal error: invalid code %d\n",tmp);
        exit(2);
      }
      if( tmp > COL_RANGE )
      {
        tmp = COL_RANGE;
      }
      green[BAND_BASE + (band*g.band_size) + shade] = tmp;
      /* }}} */
      /* {{{   set blue */
      top = bb[band];
      bot = g.ambient * top;
      intensity = bot + ((shade * (top - bot))/(g.band_size-1));
      tmp = COL_RANGE * intensity;
      if (tmp < 0)
      {
        fprintf(stderr,"set_clut: internal error: invalid code %d\n",tmp);
        exit(2);
      }
      if( tmp > COL_RANGE )
      {
        tmp = COL_RANGE;
      }
      blue[BAND_BASE + (band*g.band_size) + shade] = tmp;
      /* }}} */
    }
  }
}
/* }}} */
/* {{{   Height *extract(Strip *s) */
/*
 * extract the table of heights from the Strip struct
 * and discard the rest of the struct.
 */
Height *extract (s)
Strip *s;
{
  int i;

  Height *p;
  p = s->d;
  free(s);
  for(i=0 ; i<g.width; i++ )
  {
    p[i] = shift + (vscale * p[i]);
  }
  return(p);
}
/* }}} */
/* {{{   void init_artist_variables() */
/*
 * initialise the variables for the artist routines.
 */
void init_artist_variables()
{
  float dh, dd;
  int pwidth;  /* longest lengthscale for update */

  g.width= (1 << g.levels)+1;
  pwidth= (1 << (g.levels - g.stop))+1;

  /* make the fractal SIDE wide, this makes it easy to predict the
   * average height returned by calcalt. If we have stop != 0 then
   * make the largest update length = SIDE
   */
  cos_phi = cos( g.phi );
  sin_phi = sin( g.phi );
  tan_phi = tan( g.phi );

  x_fact = cos_phi* cos(g.alpha);
  y_fact = cos_phi* sin(g.alpha);
  vscale = g.stretch * pwidth;  /* have approx same height as fractal width
                               * this makes each pixel SIDE=1.0 wide.
                               * c.f. get_col
                               */

  delta_shadow = tan_phi /cos(g.alpha);
  shadow_slip = tan(g.alpha);
  /* guess the average height of the fractal */
  varience = pow( SIDE ,(2.0 * fold_param.fdim));
  varience = vscale * varience ;
  shift = g.base_shift * varience;
  varience = varience + shift;


  /* set the position of the view point */
  viewheight = g.altitude * g.width;
  viewpos = - g.distance * g.width;

  /* set viewing angle and focal length (vertical-magnification)
   * try mapping the bottom of the fractal to the bottom of the
   * screen. Try to get points in the middle of the fractal
   * to be 1 pixel high
   */
  dh = viewheight;
  dd = (g.width / 2.0) - viewpos;
  focal = sqrt( (dd*dd) + (dh*dh) );
#ifndef SLOPPY
  tan_vangle = (double) ((double)(viewheight-g.sealevel)/(double) - viewpos);
  vangle = atan ( tan_vangle );
  vangle -= atan( (double) (g.graph_height/2) / focal );
#else
  /* we are making some horrible approximations to avoid trig funtions */
  tan_vangle = (double) ((double)(viewheight-sealevel)/(double) - viewpos);
  tan_vangle = tan_vangle - ( (double) (height/2) / focal );
#endif

  top=make_fold(NULL, &fold_param, g.levels,g.stop,(SIDE / pwidth));

  /* use first set of heights to set shadow value */
  shadow = extract(next_strip(top));
  a_strip = extract( next_strip(top) );
  b_strip = extract( next_strip(top) );

  /* initialise the light strengths */
  vstrength = g.vfract * g.contrast /( 1.0 + g.vfract );
  lstrength = g.contrast /( 1.0 + g.vfract );
  if( g.repeat >= 0 ){
    g.pos=0;
  }else{
    g.pos=g.graph_width-1;
  }
}
/* }}} */
/* {{{   Col get_col(Height p, Height p_minus_x, Height p_minus_y, Height shadow) */
/*
 * calculate the colour of a point.
 */
Col get_col (p,p_minus_x,p_minus_y,shadow)
Height p;
Height p_minus_x;
Height p_minus_y;
Height shadow;
{
  Height delta_x, delta_y;
  Height delta_x_sqr, delta_y_sqr;
  Height hypot_sqr;

  double norm, dshade;
  Height effective;
  Col result;
  int band, shade;
  /* {{{   if underwater*/
  if ( p < g.sealevel )
  {
    if( shadow > g.sealevel )
    {
      return( SEA_UNLIT );
    }else{
      return( SEA_LIT );
    }
  }
  /* }}} */
  /*
   * We have three light sources, one slanting in from the left
   * one directly from above and an ambient light.
   * For the directional sources illumination is proportional to the
   * cosine between the normal to the surface and the light.
   *
   * The surface contains two vectors
   * ( 1, 0, delta_x )
   * ( 0, 1, delta_y )
   *
   * The normal therefore is parallel to
   * (  -delta_x, -delta_y, 1)/sqrt( 1 + delta_x^2 + delta_y^2)
   *
   * For light parallel to ( cos_phi, 0, -sin_phi) the cosine is
   *        (cos_phi*delta_x + sin_phi)/sqrt( 1 + delta_x^2 + delta_y^2)
   *
   * For light parallel to ( cos_phi*cos_alpha, cos_phi*sin_alpha, -sin_phi)
   * the cosine is
   * (cos_phi*cos_alpha*delta_x + cos_phi*sin_alpha*delta_y+ sin_phi)/sqrt( 1 + delta_x^2 + delta_y^2)
   *
   * For vertical light the cosine is
   *        1 / sqrt( 1 + delta_x^2 + delta_y^2)
   */

  delta_x = p - p_minus_x;
  delta_y = p - p_minus_y;
  delta_x_sqr = delta_x * delta_x;
  delta_y_sqr = delta_y * delta_y;
  hypot_sqr = delta_x_sqr + delta_y_sqr;
  norm = sqrt( 1.0 + hypot_sqr );

  /* {{{   calculate effective height */
  effective = (p + (varience * g.contour *
          (1.0/ ( 1.0 + hypot_sqr))));
  /* }}} */
  /* {{{   calculate colour band. */
  band = ( effective / varience) * N_BANDS;
  if ( band < 0 )
  {
    band = 0;
  }
  if( band > (N_BANDS - 1))
  {
    band = (N_BANDS -1);
  }
  result = (BAND_BASE + (band * g.band_size));
  /* }}} */

  /* {{{ calculate the illumination stength*/
  /*
   * add in a contribution for the vertical light. The normalisation factor
   * is applied later
   *
   */
  dshade = vstrength;

  if( p >= shadow )
  {
    /*
     * add in contribution from the main light source
     */
    /* dshade += ((double) lstrength * ((delta_x * cos_phi) + sin_phi));*/
    dshade += ((double) lstrength *
               ((delta_x * x_fact) + (delta_y * y_fact) + sin_phi));
  }
  /* divide by the normalisation factor (the same for both light sources) */
  dshade /= norm;
  /* }}} */
  /* {{{   calculate shading */
  /* dshade should be in the range 0.0 -> 1.0
   * if the light intensities add to 1.0
   * now convert to an integer
   */
  shade = dshade * (double) g.band_size;
  if( shade > (g.band_size-1))
  {
    shade = (g.band_size-1);
  }
  /* {{{   if shade is negative then point is really in deep shadow */
  if( shade < 0 )
  {
      shade = 0;
  }
  /* }}} */
  /* }}} */
  result += shade;
  if( (result >= g.n_col) || (result < 0) )
  {
    fprintf(stderr,"INTERNAL ERROR colour out of range %d max %d\n",result,g.n_col);
    exit(1);
  }
  return(result);
}
/* }}} */
/* {{{   Col *makemap(Height *a, Height *b, Height *shadow) */
Col *makemap (a,b,shadow)
Height *a;
Height *b;
Height *shadow;
{
Col *res;
int i;

  /* This routine returns a plan view of the surface */
  res = (Col *) malloc(g.width * sizeof(Col) );
  if (res == NULL)
  {
    fprintf(stderr,"malloc failed for colour strip\n");
    exit(1);
  }
  res[0] = BLACK;
  for(i=1 ; i<g.width ; i++)
  {
    res[i] = get_col(b[i],a[i],b[i-1],shadow[i]);
  }
  return(res);
}

/* }}} */
/* {{{   Col *camera(Height *a, Height *b, Height *shadow) */
Col *camera(a,b,shadow)
Height *a;
Height *b;
Height *shadow;
{
  int i, j, coord, last;
  Col *res, col;

  /* this routine returns a perspective view of the surface */
  res = (Col *) malloc( g.graph_height * sizeof(Col) );
  if( res == NULL )
  {
    fprintf(stderr,"malloc failed for picture strip\n");
    exit(1);
  }
  /*
   * optimised painters algorithm
   *
   * scan from front to back, we can avoid calculating the
   * colour if the point is not visable.
   */
  for( i=0, last=0 ; (i < g.width)&&(last < g.graph_height) ; i++ )
  {
    if( a[i] < g.sealevel )
    {
      a[i] = g.sealevel;
    }
    coord = 1 + project( i, a[i] );
    if( coord > last )
    {
      /* get the colour of this point, the front strip should be black */
      if( i==0 )
      {
        col = BLACK;
      }else{
        col = get_col(b[i],a[i],b[i-1],shadow[i]);
      }
      if( coord > g.graph_height )
      {
        coord = g.graph_height;
      }
      for(;last<coord;last++)
      {
        res[last]=col;
      }
    }
  }
  for(;last<g.graph_height;last++)
  {
    res[last]=SKY;
  }
  return(res);
}
/* }}} */
/* {{{   Col *mirror(Height *a, Height *b, Height *shadow)*/
Col *mirror(a,b,shadow)
Height *a;
Height *b;
Height *shadow;
{
  Col *res, *map;
  Col last_col;
  int i,j, top, bottom, coord;
  int last_top, last_bottom;
  Height pivot;
  /* this routine returns a perspective view of the surface
   * with reflections in the water
   *
   */
  res = (Col *) malloc( g.graph_height * sizeof(Col) );
  if( res == NULL )
  {
    fprintf(stderr,"malloc failed for picture strip\n");
    exit(1);
  }
  last_col=SKY;
  last_top=g.graph_height-1;
  last_bottom=0;
  /*
   * many of the optimisation in the camera routine are
   * hard to implement in this case so we revert to the
   * simple painters algorithm modified to produce reflections
   * scan from back to front drawing strips between the
   * projected position of height and -height.
   * for water stipple the colour so the reflection is still visable
   */
  map=makemap(a,b,shadow);
  pivot=2.0*g.sealevel;
  for(i=g.width-1;i>0;i--)
  {
    if(map[i] < BAND_BASE)
    {
      /* {{{ stipple water values*/
      for(j=last_bottom;j<=last_top;j++)
      {
        res[j]=last_col;
      }
      last_col=map[i];
      /* invalidate strip so last stip does not exist */
      last_bottom=g.graph_height;
      last_top= -1;
      /* fill in water values */
      coord=1+project(i,g.sealevel);
      for(j=0;j<coord;j++)
      {
        /* do not print on every other point
         * if the current value is a land value
         */
        if( (j+base)%2 || (res[j]<BAND_BASE) )
        {
          res[j]=map[i];
        }
      }
      /* skip any adjacent bits of water with the same colour */
      while(map[i]==last_col)
      {
        i--;
      }
      i++;  /* the end of the for loop will decrement as well */
      /* }}} */
    }else{
      /* {{{ draw land values*/
      top = project(i,a[i]);
      bottom=project(i,pivot-a[i]);
      if(last_col == map[i])
      {
        if( top > last_top)
        {
          last_top=top;
        }
        if( bottom < last_bottom)
        {
          last_bottom=bottom;
        }
      }else{
        if(top < last_top)
        {
          for(j=top+1;j<=last_top;j++)
          {
            res[j]=last_col;
          }
        }
        if(bottom > last_bottom)
        {
          for(j=last_bottom;j<bottom;j++)
          {
            res[j]=last_col;
          }
        }
        last_top=top;
        last_bottom=bottom;
        last_col=map[i];
      }
      /* }}} */
    }
  }
  /* {{{ draw in front face*/
  for(j=last_bottom;j<=last_top;j++)
  {
    res[j]=last_col;
  }
  if( a[0] < g.sealevel )
  {
    coord=1+project(0,g.sealevel);
  }else{
    coord=1+project(0,a[0]);
  }
  for(j=0;j<coord;j++)
  {
    res[j] = map[0];
  }
  /* }}} */
  base=1-base;
  free(map);
  return(res);
}
/* }}} */
/* {{{   int project( int x , Height y ) */
/*
 *  project a point onto the screen position
 */
int project (x,y)
int x;
Height y;
{
  int pos;
#ifndef SLOPPY
  double theta;

  theta = atan( (double) ((viewheight - y)/( x - viewpos)) );
  theta = theta - vangle;
  pos = (g.graph_height/2) - (focal * tan( theta));
#else
  float tan_theta;

  /* nast approx to avoid trig functions */
  tan_theta = (viewheight -y)/(x-viewpos) - tan_vangle;
  pos = (height/2) - (focal * tan_theta);
#endif
  if( pos > (g.graph_height-1))
  {
    pos = g.graph_height-1;
  }
  else if( pos < 0 )
  {
    pos = 0;
  }
  return( pos );
}
/* }}} */
/* {{{   void finish_artist() */
/*
 * Tidy up and free everything.
 */
void finish_artist()
{
  free(a_strip);
  free(b_strip);
  free(shadow);
  free_fold(top);
}
/* }}} */
/* {{{  void init_parameters() */

void init_parameters()
{
  g.graph_height=768;
  g.graph_width=1024;
  g.levels = 10;
  g.stop=2;
  g.n_col=DEF_COL;
  g.band_size=BAND_SIZE;
  g.ambient=0.3;
  g.contrast=1.0;
  g.contour=0.3;
  g.vfract=0.6;
  g.altitude=2.5;
  g.distance=4.0;
  g.phi=(40.0 * PI)/180.0;
  g.alpha=0.0;
  g.base_shift=0.5;
  g.sealevel=0.0;
  g.stretch=0.6;
  g.map=FALSE;
  g.reflec=TRUE;
  g.repeat=20;
  g.pos=0;
  g.scroll=0;

  fold_param.mean=0;
  fold_param.rg1=FALSE;
  fold_param.rg2=FALSE;
  fold_param.rg3=TRUE;
  fold_param.cross=TRUE;
  fold_param.force_front=TRUE;
  fold_param.force_back=FALSE;
  fold_param.forceval=-1.0;
  fold_param.fdim = 0.65;
  fold_param.mix   =0.0;
  fold_param.midmix=0.0;

}

/* }}} */
/* {{{   Col *next_col(int paint, int reflec) */

Col *next_col (paint, reflec)
int paint;
int reflec;
{
  Col *res;
  int i,offset=0;

  /* {{{    update strips */
  if(paint)
  {
    if(reflec)
    {
      res = mirror( a_strip,b_strip,shadow);
    }else{
      res = camera( a_strip,b_strip,shadow);
    }
  }else{
    res = makemap(a_strip,b_strip,shadow);
  }
  free(a_strip);
  a_strip=b_strip;
  b_strip = extract( next_strip(top) );
  /* }}} */

  /* {{{  update the shadows*/

  /* shadow_slip is the Y component of the light vector.
   * The shadows can only step an integer number of points in the Y
   * direction so we maintain shadow_register as the deviation between
   * where the shadows are and where they should be. When the magnitude of
   * this gets larger then 1 the shadows are slipped by the required number of
   * points.
   * This will not work for very oblique angles so the horizontal angle
   * of illumination should be constrained.
   */
  shadow_register += shadow_slip;
  if( shadow_register >= 1.0 )
  {
    /* {{{  negative offset*/

    while( shadow_register >= 1.0 )
    {
      shadow_register -= 1.0;
      offset++;
    }
    for(i=g.width-1 ; i>=offset ; i--)
    {
      shadow[i] = shadow[i-offset]-delta_shadow;
      if( shadow[i] < b_strip[i] )
      {
        shadow[i] = b_strip[i];
      }
      /* {{{    stop shadow at sea level */

      if( shadow[i] < g.sealevel )
      {
        shadow[i] = g.sealevel;
      }



      /* }}} */
    }
    for(i=0;i<offset;i++)
    {
      shadow[i] = b_strip[i];
      /* {{{    stop shadow at sea level*/
      if( shadow[i] < g.sealevel )
      {
        shadow[i] = g.sealevel;
      }
      /* }}} */
    }

    /* }}} */
  }else if( shadow_register <= -1.0 ){
    /* {{{  positive offset*/
    while( shadow_register <= -1.0 )
    {
      shadow_register += 1.0;
      offset++;
    }
    for(i=0 ; i<g.width-offset ; i++)
    {
      shadow[i] = shadow[i+offset]-delta_shadow;
      if( shadow[i] < b_strip[i] )
      {
        shadow[i] = b_strip[i];
      }
      /* {{{    stop shadow at sea level */
      if( shadow[i] < g.sealevel )
      {
        shadow[i] = g.sealevel;
      }
      /* }}} */
    }
    for(;i<g.width;i++)
    {
      shadow[i] = b_strip[i];
      /* {{{    stop shadow at sea level*/
      if( shadow[i] < g.sealevel )
      {
        shadow[i] = g.sealevel;
      }
      /* }}} */
    }
    /* }}} */
  }else{
    /* {{{  no offset*/
    for(i=0 ; i<g.width ; i++)
    {
      shadow[i] -= delta_shadow;
      if( shadow[i] < b_strip[i] )
      {
        shadow[i] = b_strip[i];
      }
      /* {{{    stop shadow at sea level */
      if( shadow[i] < g.sealevel )
      {
        shadow[i] = g.sealevel;
      }
      /* }}} */
    }
    /* }}} */
  }

  /* }}} */

  return(res);
}

/* }}} */
/* {{{  void plot_column(g)*/
void plot_column(g)
Graph *g;
{
  Col *l;
  int j;
  int mapwid;

  /* blank if we are doing the full window */
  if( g->repeat >= 0){
    if(g->pos == 0){
      blank_region(0,0,g->graph_width,g->graph_height);
      flush_region(0,0,g->graph_width,g->graph_height);
    }
  }else{
    if( g->pos == g->graph_width-1){
      blank_region(0,0,g->graph_width,g->graph_height);
      flush_region(0,0,g->graph_width,g->graph_height);
    }
  }
  if( g->scroll ){
    scroll_screen(g->scroll);
  }

  l = next_col(1-g->map,g->reflec);
  if( g->map )
  {
    if( g->graph_height > g->width ){
      mapwid=g->width;
    }else{
      mapwid=g->graph_height;
    }
    for( j=0 ;j<(g->graph_height-mapwid); j++)
    {
      plot_pixel(g->pos,((g->graph_height-1)-j),BLACK);
    }
    for(j=0; j<mapwid ; j++)
    {
      plot_pixel(g->pos,((mapwid-1)-j),l[j]);
    }
  }else{
    for(j=0 ; j<g->graph_height ; j++)
    {
      /* we assume that the scroll routine fills the
       * new region with a SKY value. This allows us to
       * use a testured sky for B/W displays
       */
      if( l[j] != SKY )
      {
        plot_pixel(g->pos,((g->graph_height-1)-j),l[j]);
      }
    }
  }
  free(l);
  flush_region(g->pos,0,1,g->graph_height);
  g->scroll = 0;
  /* now update pos ready for next time */
  if( g->repeat >=0 ){
    g->pos++;
    if(g->pos >= g->graph_width)
    {
      g->pos -=  g->repeat;
      if( g->pos < 0 || g->pos > g->graph_width-1 )
      {
        g->pos=0;
      }else{
        g->scroll = g->repeat;
      }
    }
  }else{
    g->pos--;
    if( g->pos < 0 ){
      g->pos -=   g->repeat;
      if( g->pos < 0 || g->pos > (g->graph_width-1) ){
	g->pos=g->graph_width-1;
      }else{
	g->scroll = g->repeat;
      }
    }
  }

}
/* }}} */


