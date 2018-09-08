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
      Subroutine NRMDAT(IDS,ICN)
*
* Purpose :  normalize input data
* Date    :  October 28, 2016
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    IDS        = sequence number
*    ICN        = col. to normalize counts
*    DMN(*)     = normalization data, if ICN>0
*    DMZ(*)     = counts
*    DMDZ(*)    = error of counts
*    DMY(*)     = counts of elastic peak spectrum
*    DMDY(*)    = errors of counts of elastic peak spectrum
*    DBGRD      = background counts
*
* export variables :
*    DMZ(*)     = normalized counts
*    DMDZ(*)    = normalized errors
*    DMY(*)     = normalized counts of elastic peak spectrum
*    DMDY(*)    = normalized errors of elastic peak spectrum
*    DMN(*)     = normalization function, if ICN>0
*    DFLC(IDS)  = rms fluctuation of normalization function
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Dimension  DH(PDATA),KVLD(PDATA)
*
*
      DFLC(IDS) = 0.D0
      If(ICN.eq.0)  Return
*
*:  normalization of data
*
      D12 = Dabs(DMN(1)-DMN(2))
      D23 = Dabs(DMN(2)-DMN(3))
      D13 = Dabs(DMN(1)-DMN(3))
      If(D12.gt.1.D1*D23 .and. D13.gt.1.D1*D23) Then
       DMN(1) = 0.5D0*(DMN(2) + DMN(3))
      Endif
*
*:  sudden change detection
*
       KVLD(1) = 1
       KMARK   = 1
       KNORM   = 1
       DNORM   = DMN(1)
       Do  K = 2,KD
        If(Dabs(DMN(K)-DMN(KMARK)).gt.0.15D0*Dabs(DMN(KMARK))) Then
         KVLD(K) = 0
        Else
         KVLD(K) = 1
         KMARK   = K
         KNORM   = KNORM + 1
         DNORM   = DNORM + DMN(K)
        Endif
       EndDo
       DNORM = DNORM/Dfloat(KNORM)
*
*:  remove sudden changes of normalization function
*:  smooth normalization function
*
       Do  K = 2,KD-1
        KK = KVLD(K-1) + KVLD(K) + KVLD(K+1)
        If(KK.eq.0) Then
         DH(K) = DMN(K)
        Else
         DH(K) = (KVLD(K-1)*DMN(K-1) + KVLD(K)*DMN(K) +
     +               KVLD(K+1)*DMN(K+1))/Dfloat(KK)
        Endif
       EndDo
*
       KK = KVLD(1) + KVLD(2)
       If(KK.eq.0) Then
        DH(1) = DMN(1)
       Else
        DH(1) = (KVLD(1)*DMN(1) + KVLD(2)*DMN(2))/Dfloat(KK)
       Endif
*
       KK = KVLD(KD-1) + KVLD(KD)
       If(KK.eq.0) Then
        DH(KD) = DMN(KD)
       Else
        DH(KD) = (KVLD(KD-1)*DMN(KD-1) + KVLD(KD)*DMN(KD))/
     +               Dfloat(KK)
       Endif
*
*:  normalize data
*
       DSUM  = 0.D0
       KMARK = 1
       Do  K = 1,KD
        If(KVLD(K).EQ.0) Then
         DMN(K) = DNORM/DH(KMARK)
        Else
         KMARK  = K
         DMN(K) = DNORM/DH(K)
        Endif
        D1      = 1.D0 - DMN(K)
        DMY(K)  = DMY(K)*DMN(K) + DBGRD*D1
        DMZ(K)  = DMZ(K)*DMN(K) + DBGRD*D1
        DMDY(K) = DMDY(K)*DMN(K)**2
        DMDZ(K) = DMDZ(K)*DMN(K)**2
        DSUM    = DSUM + D1**2
       EndDo
       DFLC(IDS) = 1.D2*Dsqrt(DSUM/Dfloat(KD))
*
*
      End
