*
**************************************************************
*
*  PHOENIX Version 2.1.4
*  Copyright (C) 2016 Wolfgang Sturhahn
*
*  This file is part of PHOENIX-2.1.4.
*
*  PHOENIX-2.1.4 is free software:
*  You can redistribute it and/or modify it under the terms
*  of the GNU General Public License as published by the
*  Free Software Foundation
*
*  PHOENIX-2.1.4 is distributed in the hope that it will be
*  useful, but WITHOUT ANY WARRANTY; without even the implied
*  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*  PURPOSE. See the GNU General Public License for details.
*
*  A copy of the GNU General Public License should have been
*  provided along with PHOENIX-2.1.4.
*  If not, see <http://www.gnu.org/licenses/>.
*
**************************************************************
*
      FUNCTION DNORM1(DA,N,NP,MODE)
*
* Purpose :  calculate normalization constant of one dimensional array;
*            normalize one dimensional array.
* Date    :  December 6, 1997
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DA(*)   = array to be normalized
*    N       = number of elements to be used
*    MODE    = control parameter, options are
*                0x00x1  normalize array
*                0x001x  peak normalization
*                0x010x  area normalization
*                0x011x  euclidean normalization
*
* export variables :
*    DA(*)   = normalized array
*    DNORM1  = normalization constant
*    NP      = index of peak if peak normalization requested
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION  DA(*)
*
*
      DNORM1 = 0.D0
      IF(N.LE.0) RETURN
*
*:  calculate normalization constant
*
      NP = -1
      IF(MODE/2.EQ.1) THEN
       DNORM1 = DA(1)
       NP     = 1
       DO 1  I = 2,N
        IF(DA(I).GT.DNORM1) THEN
         DNORM1 = DA(I)
         NP     = I
        ENDIF
    1  CONTINUE
       GOTO 100
      ENDIF
*
      IF(MODE/2.eq.2) THEN
       DNORM1 = 0.D0
       DO 2  I = 1,N
        DNORM1 = DNORM1 + DA(I)
    2  CONTINUE
       GOTO 100
      ENDIF
*
      IF(MODE/2.EQ.3) THEN
       DNORM1 = 0.D0
       DO 3  I = 1,N
        DNORM1 = DNORM1 + DA(I)**2
    3  CONTINUE
       DNORM1 = DSQRT(DNORM1)
       GOTO 100
      ENDIF
*
      DNORM1 = 0.D0
*
*:  normalize array
*
  100 CONTINUE
      IF(MOD(MODE,2).EQ.1 .AND. DNORM1.NE.0.D0) THEN
       DN = 1.D0/DNORM1
       DO 10  I = 1,N
        DA(I) = DA(I)*DN
   10  CONTINUE
      ENDIF
*
*
      END
