/* $Id: crinkle.h,v 1.1 2003/04/08 23:13:37 dustin Exp $ */
#ifndef CRINKLE
#define CRINKLE
/* {{{   typedefs */
typedef float Height;
typedef float Length;
/* }}} */
/* {{{   defines */
#ifndef NULL
#define NULL (void *) 0
#endif
#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#define START 0
#define STORE 1

#define NSTRIP 8
/* }}} */
/* {{{   structs */

/* parameters for the update */
typedef struct parm{
  Height mean;              /* mean altitude */
  int rg1;                  /* optional regeneration steps */
  int rg2;
  int rg3;
  int cross;                /* use four point average on edges rather than 2 */
  int force_front;          /* keep front edge low */
  int force_back;           /* keep back edge low */
  Height forceval;          /* value to force to */
  float mix;                /* fraction of old value to include in average */
  float midmix;             /* same but for cross updates */
  float fdim;
}Parm;

/* The parameter struct for the recursive procedure */
typedef struct fold{
  int level;                /* levels of recursion below us */
  int count;                /* number of points at this level */
  Length length;            /* width of the update square */
  Length scale;             /* scale factor for perturbations */
  Length midscale;          /* as above but for diagonal offsets */
  struct parm *p;           /* update parameters */
  struct strip *s[NSTRIP];  /* pointers to the pipeline strips */
  struct strip *save;       /* save position for STORE state */
  int stop;                 /* level to stop recursion */
  int state;                /* internal stat of algorithm */
  struct fold *next;        /* next iteration down */
  struct fold *parent;      /* next iteration up */
} Fold;

/* strip of altitudes */
typedef struct strip{
  struct fold *f;   /* parent fold structure */
  Height *d;        /* should have 2^level + 1 (f->count) points */
}Strip;


/* }}} */
/* {{{   prototypes */
#ifdef ANSI
Strip *make_strip (Fold *);
void free_strip (Strip *);
Strip *double_strip (Strip *);
Strip *set_strip(Fold *, Height );
Strip *random_strip (Fold *);
Strip *next_strip (Fold *);
void reset_fold(Fold *f);
Fold *make_fold (Fold *,Parm *,int, int, Length);
void free_fold (Fold *);
Length gaussian ();
void x_update(Fold *, float, float, Strip *, Strip *, Strip *);
void p_update(Fold *, float, float, Strip *, Strip *, Strip *);
void t_update(Fold *, float, float, Strip *, Strip *, Strip *);
void v_update(Fold *, float, float, Strip *, Strip *, Strip *);
void vside_update(Fold *, float, float, Strip *);
void hside_update(Fold *, float, float, Strip *, Strip *, Strip *);
#else
Strip *make_strip ();
void free_strip ();
Strip *double_strip ();
Strip *set_strip ();
Strip *random_strip ();
Strip *next_strip ();
void reset_fold();
Fold *make_fold ();
void free_fold ();
Length gaussian ();
void x_update();
void p_update();
void t_update();
void v_update();
void vside_update();
void hside_update();

#endif
/* }}} */
#endif
