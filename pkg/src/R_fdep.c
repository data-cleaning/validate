/*  Copyright (C) Mark van der Loo and Edwin de Jonge
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>. 
 *
 *  You can contact the author at: mark _dot_ vanderloo _at_ gmail _dot_ com
 */

#include <R.h>
#include <Rdefines.h>
#include <stdint.h>
#include <string.h>
#include "sfh.h"

/* 
 * Check functional dependencies x -> y
 *
 * The FD x -> y is two be interpreted as if two
 * elements of x have the same value, they shall have the
 * same value for y. In the comments below we refer to 'x' 
 * as the condition and to 'y' as the consequent.
 * 
 * INPUT x, y: character vectors.
 * OUTPUT integer vector. 
 * 
 * If all pairs in (x,y) obey x->y then the output is seq_along(x).
 * suppose that for some i < j we have x[i] == x[j] but y[i] != y[j].
 * The j'th value of the output is then equal to i. 
 *
 */

SEXP R_fdcheck(SEXP x, SEXP y){
  PROTECT(x);
  PROTECT(y);
  
  int k, hashfac = 11;
  
  R_xlen_t n = xlength(x)
    , nh = hashfac*n
    , j;
  
  SEXP out;
  PROTECT(out = allocVector(INTSXP,n));
  
  uint32_t *H = (uint32_t *) calloc(nh, sizeof(uint32_t));
  R_xlen_t *E = (R_xlen_t *) malloc(nh * sizeof(R_xlen_t));
  
  if (H == NULL | E == NULL){
    free(H); free(E);
    error("Could not allocate enough memory");
  }
  
  int *I = INTEGER(out);
  
  uint32_t a,b;
  
  for ( R_xlen_t i = 0; i<n; i++, I++){
    a = SuperFastHash(CHAR(STRING_ELT(x,i)), length(STRING_ELT(x,i)));
    b = SuperFastHash(CHAR(STRING_ELT(y,i)), length(STRING_ELT(y,i)));
    rehash:
    j = a % nh;
    if (H[j] == 0){ // new condition
      H[j] = b;
      E[j] = i;
      (*I) = i + 1; // R-indexing
     } else { // found similar condition
      // In case of collision in condition: hash the hash.
      if ( strcmp( CHAR(STRING_ELT(x,E[j])), CHAR(STRING_ELT(x,i)) ) != 0 ){
        a = SuperFastHash((const char *) &a, 4); 
        goto rehash;
      }
      if (H[j] == b && strcmp( CHAR(STRING_ELT(y,E[j])), CHAR(STRING_ELT(y,i)) ) == 0 ){ 
        // conseqent also similar, all good.
        (*I) = i + 1;
      } else { // consequent different, store conflicting index.
       (*I) = E[j] + 1;
      }
    }
  }
     
  free(H);
  free(E);
  UNPROTECT(3);
  return(out);
  
}

