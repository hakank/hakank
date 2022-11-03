include "grapdecl.pro"

constants
xlo = -2.2        /* Xlo, Ylo, Xhi och Yhi bildar tillsammans  */
xhi = 1.0        /* den "ruta" i det komplexa talplanet som   */
ylo = 1.2        /* motsvaras av sk„rmen.                     */
yhi = 1.2
maxIter = 16
maxZ = 10.0

predicates



clauses

/* Definiera ett komplext tal */

/************************************
 * Absolutv„rde av ett komplext tal *
 ************************************/
float AbsC(struct komplex z)
{
  return( sqrt( z.re*z.re + z.im*z.im) );


/* Andra ledet: GE X och Y v„rden*/

initiera :-
  Xres = getmaxx + 1,    /* Xres = antalet koordinater i x-led */
  Yres = getmaxy + 1,
  DX = (Xhi-Xlo)/Xres,  /* "Avst†nd" i x-led mellan tv† punkter p† sk„rmen */
  DY = (Yhi-Ylo)/Yres.

/* Tredje ledet:
   G”r en loop:
   F”r X = 0 till max_X
      F”r Y = 0 till Max_Y  */

loop :-
  X < Xres,
  Y < Yres.  


  for (x=0; x<Xres; x++)
  {
   for (y=0; y<Yres; y++)
   {
/* Fj„rde ledet
   Ge startv„rde*/

ge_start_v„rde :-
    z_re = z_im = 0,        /* Ge variablerna startv„rden:                  */
    c_re = x * DX + xlo,    /*   z = (0,0)                                  */
    c_im = y * DY + ylo.    /*   c = Koordinaten ”versatt till komplexa tal */

    /*************************************************
     * Iterera enligt formeln nedan tills ³z³>MaxZ   *
     * eller antalet iterationer ”verstiger MaxIter. *
     *************************************************/
 /* Femte ledet 
    Iterera tills Iter antingen „r st”rre „n 
    MaxIter 
    eller
    Absv„rdet av Z „r st”rre „n MaxZ */

start_Iterering :-
   Iter < MaxIter,
   abs(z) < maxZ,
   NewZre = z_re*z_re - z_im*z_im + c_re,    /******************/
     Z_im = 2*z_re*z_im + c_im,              /*  z <-- zý + c  */
     Z_re = NewZre.                          /******************/
    
    
    
kontroll_och_plott :-
    Iter > MaxIter,        /* Om z inte g†r mot o„ndligheten tillh”r   */
                           /* punkten c "Mandelbrotm„ngden" och f„rgas */
                           /* svart. Annars f„rgas punkten efter hur   */
                           /* fort z g†r mot o„ndligheten.             */
    putpixel(X,Y, Color).

/* ELSE */

kontroll_och_plott :-
    Iter < MaxIter,
    putpixel(X, Y, 1).

/* Fl”desschema:
1. G”r klar grafiken
2. Ge X och Y max v„rden
3. 





