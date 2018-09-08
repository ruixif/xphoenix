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
      SUBROUTINE MOMENT(M)
*
* purpose :  calculate the moments of data and res.fct.
* date    :  December 29, 1994
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    M       = number of moments to calculate
*    DSX(*)  = x-value of data
*    DSY(*)  = y-value of data, elastic peak removed
*    DMY(*)  = y-value of data including elastic peak
*
* export variables :
*    DMD(*)  = moments of the data
*    DMR(*)  = moments of resolution function
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<phox.h>
*
      DIMENSION DPP(PFIT),DTT(PDATA),DFAC(0:PMN)
*
*
*:  calculate moments of data
*
      DMD(0) = 0.0
      DO 1  K = 1,KD
       DMD(0) = DMD(0) + DMY(K)
    1 CONTINUE
*
      DO 2  J = 1,M
       DMD(J) = 0.0
       DO 3  K = 1,KD
        DMD(J) = DMD(J) + DMY(K)*DSX(K)**J
    3  CONTINUE
    2 CONTINUE
*
*:  moments of the resolution function
*
      DPP(1) = 1.0
      DPP(2) = DP(2)
      DPP(3) = 0.0
      DPP(6) = DP(6)
      DPP(7) = DP(7)
*
      DMR(0) = 0.0
      DO 4  K = 1,KD
       DTT(K) = DTH(-1,DSX(K),DBETA,DPP,DRRX,DRRY,KRD,BREXT)
       DMR(0) = DMR(0) + DTT(K)
    4 CONTINUE
*
      DFAC(0) = 1.0
      DO 5  J = 1,M
       DFAC(J) = DFLOAT(J)*DFAC(J-1)
       DMR(J)  = 0.0
       DO 6  K = 1,KD
        DMR(J) = DMR(J) + DTT(K)*DSX(K)**J
    6  CONTINUE
       DMR(J) = DMR(J)/(DMR(0)*DFAC(J))
    5 CONTINUE
*
      DMR(0) = 1.0
*
*:  moments of deconvoluted data
*
      DMS(0) = DMD(0)
*
*:  recursive formula
*
      DO 7  J = 1,M
       DMS(J) = DMD(J)
       DO 8  L = 1,J
        DMS(J) = DMS(J) - DFAC(J)/DFAC(J-L)*DMR(L)*DMS(J-L)
    8  CONTINUE
    7 CONTINUE
*
*:  replace 0th-moment, i.e., use data without elastic peak
*
      DMS(0) = 0.0
      DO 9  K = 1,KD
       DMS(0) = DMS(0) + DSY(K)
    9 CONTINUE
*
*
      END
