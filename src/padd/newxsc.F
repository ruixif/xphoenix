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
      Subroutine NEWXSC(MODE)
*
* Purpose :  update x-scale of the measured data
* Date    :  April 9, 1996
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DMX(*)   = recent x-scale
*    DSX(*)   = old x-scale
*    DSY(*)   = old y-scale
*    DSDY(*)  = old y-errors
*    KD       = number of data points, recent scale
*    KDS      = number of data points, old scales
*    MODE     = if 0, reduce x-scale to intersection
*                    otherwise expand x-scale to union
*
* export variables :
*    DSX(*)   = new x-scale
*    DSY(*)   = new y-scale
*    DSDY(*)  = new y-errors
*    KDS      = number of data points, new scales
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Dimension  DH1(PDATA),DH2(PDATA)
*
      DDX = DSX(2) - DSX(1)
*
*:  index of minimum x-value on old scale
*
      KMIN = Idint((DMX(1)-DSX(1))/DDX) + 1
*
*:  index of maximum x-value on old scale
*
      KMAX = Idint((DMX(KD)-DSX(1))/DDX) + 1
*
      If(MODE.eq.0) Then
*
*:  intersection strategy
*
       KMIN = Max0(1,KMIN)
       KMAX = Min0(KDS,KMAX)
       KDS  = KMAX - KMIN + 1
*
*:  create reduced scales
*
       If(KMIN.ne.1) Then
        Do  K = 1,KDS
         KK = K+KMIN-1
         DSX(K)  = DSX(KK)
         DSY(K)  = DSY(KK)
         DSDY(K) = DSDY(KK)
        EndDo
       Endif
      Else
*
*:  union strategy
*
       KMIN = Min0(1,KMIN-1)
       KMAX = Max0(KDS,KMAX+1)
       KDS9 = Min0(PDATA,KMAX-KMIN+1)
*
*:  create expanded scales
*
       Call DCOPY1(DSY,DH1,KDS)
       Call DCOPY1(DSDY,DH2,KDS)
*
       DOFF = DSX(1) + DFLOAT(KMIN-2)*DDX
       Do  K = 1,KDS9
        DSX(K)  = DOFF + DFLOAT(K)*DDX
        DSY(K)  = 0.D0
        DSDY(K) = 0.D0
       EndDo
*
       Do  K = 1,KDS
        KK = K-KMIN+1
        DSY(KK)  = DH1(K)
        DSDY(KK) = DH2(K)
       EndDo
*
       KDS = KDS9
      Endif
*
*
      End
