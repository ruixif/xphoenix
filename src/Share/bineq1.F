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
      SUBROUTINE BINEQ1(DXO,DYO,DXN,DYN,K,N,N0,MODE)
*
* Purpose :  produce equidistant binning for x-axis of a
*            set of (x,y) pairs; the y-values are interpolated
*            using forward and backward parabolas.
* Date    :  December 5, 1997
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DXO(*)   = x-values, ordered sequence
*    DYO(*)   = y-values
*    K        = number of (x,y) pairs
*    N        = number of (x,y) pairs in output arrays
*    MODE     = control parameter, options are
*                0xxxx1  produce increasing values of x
*                0xxx1x  provide x=0 in output x-array
*                0x00xx  quadratic interpolation
*                0x01xx  linear interpolation
*                0x10xx  binning, bin size given by input data
*
* export variables :
*    DXN(*)   = x-values, equidistant
*    DYN(*)   = interpolated or binned y-values
*    N0       = position of x=0 in output array DXN(*)
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION  DXO(*),DYO(*),DXN(*),DYN(*)
*
*
      IF(K.LE.1 .OR. N.LE.1) RETURN
*
      BS = MOD(MODE,2).EQ.1
      BZ = MOD(MODE/2,2).EQ.1
      BQ = MOD(MODE/4,2).EQ.0 .AND. MOD(MODE/8,2).EQ.0
      BB = MOD(MODE/8,2).EQ.1
*
      BRVS = BS .AND. DXO(K).LT.DXO(1)
*
*
*:  reverse x-scale
*
      IF(BRVS) THEN
       DO 1  I = 1,K/2
        II      = K-I+1
        DXX     = DXO(I)
        DYY     = DYO(I)
        DXO(I)  = DXO(II)
        DYO(I)  = DYO(II)
        DXO(II) = DXX
        DYO(II) = DYY
    1  CONTINUE
      ENDIF
*
*:  K0 = index for x appr. 0 on the old x-scale
*
      DX = (DXO(K) - DXO(1))/DFLOAT(N-1)
      K0 = 1
      IF(BZ) THEN
       DO 2  I = 2,K
        IF(DXO(I-1)*DXO(I).LE.0.D0) THEN
         K0 = I
         GOTO 200
        ENDIF
    2  CONTINUE    
  200  CONTINUE
       IF(DABS(DXO(K0-1)).LT.DABS(DXO(K0))) K0 = K0-1
       N0   = IDNINT((DXO(K0)-DXO(1))/DX) + 1
       DOFF = -DFLOAT(N0-1)*DX
      ELSE
       N0   = -1
       DOFF = DXO(1)
      ENDIF
*
*
      IF(BQ) THEN
*
*:  quadratic interpolation
*
       DO 3  I = 1,N
        DXN(I) = DOFF + DFLOAT(I-1)*DX
*
*:  evaluate index on DXO-scale corresponding to x-value on DXN-scale
*
        KE = 1
        DO 4  J = 1,K
         IF(DXO(J).GE.DXN(I)) THEN
          KE = J
          GOTO 400
         ENDIF
*
         IF(DXO(K-J+1).LE.DXN(I)) THEN
          KE = K - J + 2
          GOTO 400
         ENDIF
    4   CONTINUE
  400   CONTINUE
*
*:  exceptions, interpolate linearly
*
        IF(KE.LT.2) THEN
         DYN(I) = DSPLL(DXN(I),DXO(1),DYO(1),DXO(2),DYO(2))
        ELSEIF(KE.GT.K-1) THEN
          DYN(I) = DSPLL(DXN(I),DXO(K-1),DYO(K-1),DXO(K),DYO(K))
         ELSE
*
*:  quadratic interpolation, looking forward and backward
*
          DYN(I) = DSPLQ( DXN(I),DXO(KE-1),DYO(KE-1),
     +                           DXO(KE),  DYO(KE),
     +                           DXO(KE+1),DYO(KE+1) )
*
          IF(KE.GT.2) THEN
           DYN(I) = 0.5D0*(DYN(I) +
     +                    DSPLQ( DXN(I),DXO(KE-2),DYO(KE-2),
     +                                  DXO(KE-1),DYO(KE-1),
     +                                  DXO(KE),  DYO(KE) ))
          ENDIF
        ENDIF
    3  CONTINUE
      ELSEIF(BB) THEN
*
*:  binning
*
        DO 5  I = 1,N
         DXN(I) = DOFF + DFLOAT(I-1)*DX
*
*:  bins are symmetric around x-values on DXN-scale
*
         DYN(I) = 0.D0
         DO 6  J = 1,K
          DX1 = DXN(I) - 0.5D0*DABS(DX)
          DX2 = DXN(I) + 0.5D0*DABS(DX)
          IF(DXO(J).GE.DX1 .AND. DXO(J).LT.DX2) THEN
           DYN(I) = DYN(I) + DYO(J)
          ENDIF
    6    CONTINUE
    5   CONTINUE
       ELSE
*
*:  linear interpolation
*
        DO 7  I = 1,N
         DXN(I) = DOFF + DFLOAT(I-1)*DX
*
*:  evaluate index on DXO-scale corresponding to x-value on DXN-scale
*
         KE = 1
         DO 8  J = 1,K
          IF(DXO(J).GE.DXN(I)) THEN
           KE = J
           GOTO 800
          ENDIF
*
          IF(DXO(K-J+1).LE.DXN(I)) THEN
           KE = K - J + 2
           GOTO 800
          ENDIF
    8    CONTINUE
  800    CONTINUE
*
*:  interpolate
*
         IF(KE.LT.2) THEN
          DYN(I) = DSPLL(DXN(I),DXO(1),DYO(1),DXO(2),DYO(2))
         ELSE
          DYN(I) = DSPLL(DXN(I),DXO(KE-1),DYO(KE-1),DXO(KE),DYO(KE))
         ENDIF
    7   CONTINUE
      ENDIF
*
*:  undo reversal of original x-scale
*
      IF(BRVS) THEN
       DO 9  I = 1,K/2
        II      = K-I+1
        DXX     = DXO(I)
        DYY     = DYO(I)
        DXO(I)  = DXO(II)
        DYO(I)  = DYO(II)
        DXO(II) = DXX
        DYO(II) = DYY
    9  CONTINUE
      ENDIF
*
*
      END
