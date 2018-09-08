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
      Subroutine DTAFC(DX,DY,DDY,KD,DGAMMA,DREC,DZ,DDZ)
*
* Purpose :  calculate the average force constant
* Date    :  February 18, 2011
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DX(*)   = energy in meV
*    DY(*)   = normalized phonon DOS; Sum DY = 1
*    DDY(*)  = errors^2 of DOS
*    KD      = number of data points
*    DGAMMA  = nuclear transition energy in keV
*    DREC    = recoil energy in meV
*
* export variables :
*    DZ      = average force constant in N/m
*    DDZ     = error of average force constant in N/m
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*:   DHQUER  =  (Planck constant)/2*pi    in units of eV*s
*:   DCLIC   =  speed of light            in units of m/s
*:   DELCH   =  elementary charge         in units of C
*
      PARAMETER(DHQUER = 6.582118890D-16,
     +          DCLIC  = 2.99792458D+08,
     +          DELCH  = 1.602176462D-19)
*
*
      DIMENSION  DX(*),DY(*),DDY(*)
*
*
      DZ  = 0.D0
      DDZ = 0.D0
      If(KD.le.0 .or. DGAMMA.le.0.D0 .or. DREC.le.0.D0) Return
*
      Do 1  K = 1,KD
       If(DX(K).le.0.D0) GoTo 1
       D1  = DX(K)*DX(K)
       DZ  = DZ  + DY(K)*D1
       DDZ = DDZ + DDY(K)*D1*D1
    1 Continue
*
      DFF = 5.D2*DELCH*(DGAMMA/(DHQUER*DCLIC))**2/DREC
      DZ  = DFF*DZ
      DDZ = DFF*Dsqrt(DDZ)
*
*
      End
