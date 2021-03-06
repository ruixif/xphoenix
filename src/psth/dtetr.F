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
      Subroutine DTETR(DX,DY,DDY,KD,DBETA,DZ,DDZ)
*
* Purpose :  calculate the vibrational entropy
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
*
* export variables :
*    DZ      = vibrational entropy in k_B/atom
*    DDZ     = error of vibrational entropy in k_B/atom
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION  DX(*),DY(*),DDY(*)
*
*
      DZ  = 0.D0
      DDZ = 0.D0
      If(KD.le.0 .or. DBETA.le.0.D0) Return
*
      Do 1  K = 1,KD
       DXX = 0.5D0*DBETA*DX(K)
       If(DXX.gt.50.D0 .or. DXX.le.1.D-6) GoTo 1
*
       DE  = Dexp(-DXX)
       DSH = 0.5D0*(1.D0/DE - DE)
       DCH = 0.5D0*(1.D0/DE + DE)
       D1  = DXX*DCH/DSH - Dlog(2.D0*DSH)
*
       DZ  = DZ  + DY(K)*D1
       DDZ = DDZ + DDY(K)*D1*D1
    1 Continue
*
      DZ  = 3.D0*DZ
      DDZ = 3.D0*Dsqrt(DDZ)
*
*
      End
