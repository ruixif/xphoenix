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
      SUBROUTINE DRUNAV(DV,DVA,K,N)
*
* Purpose :  perform an average over N points in input vector
* Date    :  December 13, 2001
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DV(*)   = input vector
*    K       = number of elements in input vector
*    N       = average over N   elements if N uneven
*                           N+1 elements if N even
*
* export variables :
*    DVA(*)   = averaged input vector
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION  DV(*),DVA(*)
*
*
      IF(K.LE.1 .OR. N.LE.0) RETURN
*
*
      NH = N/2
      NA = 2*NH + 1
*
*:  main part of vector
*
      DO 1  I = 1+NH,K-NH
       DVA(I) = 0.D0
       DO 2  J = -NH,NH
        DVA(I) = DVA(I) + DV(I+J)
    2  CONTINUE
       DVA(I) = DVA(I)/DFLOAT(NA)
    1 CONTINUE
*
*:  head of vector
*
      DO 3  I = 1,MIN0(NH,K)
       DVA(I) = 0.D0
       NA     = 0
       DO 4  J = -NH,NH
        JI = I+J
        IF(JI.GT.0 .AND. JI.LE.K) THEN
         DVA(I) = DVA(I) + DV(JI)
         NA     = NA + 1
        ENDIF
    4  CONTINUE
       DVA(I) = DVA(I)/DFLOAT(NA)
    3 CONTINUE
*
*:  tail of vector
*
      DO 5  I = MAX0(1,K-NH+1),K
       DVA(I) = 0.D0
       NA     = 0
       DO 6  J = -NH,NH
        JI = I+J
        IF(JI.GT.0 .AND. JI.LE.K) THEN
         DVA(I) = DVA(I) + DV(JI)
         NA     = NA + 1
        ENDIF
    6  CONTINUE
       DVA(I) = DVA(I)/DFLOAT(NA)
    5 CONTINUE
*
*
      END
