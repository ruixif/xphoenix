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
      Subroutine DTFLM(DX,DY,DDY,KD,DBETA,DREC,DZ,DDZ)
*
* Purpose :  calculate the Lamb-Moessbauer factor
* Date    :  February 18, 2011
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DX(*)   = energy in meV
*    DY(*)   = normalized phonon DOS; Sum DY = 1
*    DDY(*)  = errors^2 of DOS
*    KD      = number of data points
*    DBETA   = inverse temperature in 1/meV
*    DREC    = recoil energy in meV
*
* export variables :
*    DZ      = Lamb-Moessbauer factor
*    DDZ     = error of Lamb-Moessbauer factor
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Dimension  DX(*),DY(*),DDY(*)
*
*
      DZ  = 0.D0
      DDZ = 0.D0
      If(KD.le.0 .or. DBETA.le.0.D0 .or. DREC.le.0.D0) Return
*
*:  assume DX(1)=0
*
      DZ1  = 0.D0
      DDZ1 = 0.D0
      Do  K = 2,KD
       DXX = DBETA*DX(K)
       If(DXX.le.50.D0) Then
        DE = Dexp(-DXX)
        D1 = (1.D0 + DE)/(1.D0 - DE)/DX(K)
       Else
        D1 = 1.D0/DX(K)
       Endif
       DZ  = DZ  + DY(K)*D1
       DDZ = DDZ + DDY(K)*D1*D1
       If(K.eq.2) Then
        DZ1  = DY(K)*D1
        DDZ1 = DDY(K)*D1*D1
       Endif
      EndDo
*
*:  E=0 contribution to integral
*
      DZ  = DZ  + 0.50*DZ1
      DDZ = DDZ + 0.50*DDZ1
*
      DZ  = Dexp(-DZ*DREC)
      DDZ = DZ*Dsqrt(DDZ)*DREC
*
*
      End
