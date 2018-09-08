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
      FUNCTION DINVGM(DX)
*
* purpose :  provide value for inverse Gamma-function;
*            a series expansion for the inverse
*            Gamma-function is used, see
*            Abramowitz,Stegun,p.256,eq.6.1.34
* date    :  December 11, 1997
* author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION DC(0:19)
      DATA DC / 0.D0, 1.D0, 5.772156649015329D-01,
     +         -6.558780715202538D-01, -4.20026350340952D-02,
     +          1.665386113822915D-01, -4.21977345555443D-02,
     +         -9.6219715278770D-03,    7.2189432466630D-03,
     +         -1.1651675918591D-03,   -2.152416741149D-04,
     +          1.280502823882D-04,    -2.01348547807D-05,
     +         -1.2504934821D-06,       1.1330272320D-06,
     +         -2.056338417D-07,        6.1160950D-09,
     +          5.0020075D-09,         -1.1812746D-09,
     +          1.043427D-10 /
*
*
      N  = IDINT(DX)
      DR = DX - DFLOAT(N)
*
*:  integer valued arguments
*
      IF(DR.EQ.0.D0) THEN
       IF(N.LE.0) THEN
        DINVGM = 0.D0
       ELSE
        DINVGM = 1.D0
        DO 1  K = 2,N-1
         DINVGM = DINVGM/DFLOAT(K)
 0001   CONTINUE
       ENDIF
       RETURN
      ENDIF
*
*:  non integer valued arguments
*:  use recurrence formula to shift argument to [-0.5,0.5]
*
      DFAC = 1.D0
      IF(N.GT.0) THEN
       DO 2  K = 1,N
        DFAC = DFAC/(DX-DFLOAT(K))
 0002  CONTINUE
      ELSE
       DO 3  K = 0,-N-1
        DFAC = DFAC*(DX+DFLOAT(K))
 0003  CONTINUE
      ENDIF
*
*:  take care of the fractional part,
*:  employ polynomial approximation of inverse Gamma function
*
      DINVGM = 0.D0
      DO 4  K = 19,0,-1
       DINVGM = DINVGM*DR + DC(K)
 0004 CONTINUE
*
*:  combine results
*
      DINVGM = DFAC*DINVGM
*
*
      END
