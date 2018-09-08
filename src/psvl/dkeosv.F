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
      Function DKEOSV(DV,DKP,MTYPE)
*
* purpose :  calculate bulk modulus from volume using
*             various equations of state;
* date    :  April 2, 2006
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DV        = volume divided by volume at p=0
*    MTYPE     = type of EoS to be used
*                  0 :  third-order Birch-Murnaghan
*                  1 :  logarithmic
*                  2 :  Vinet
*    DKP       = pressure derivative of bulk modulus at p=0
*
* export variables :
*    DKEOS     = bulk modulus divided by bulk modulus at p=0
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Parameter(DH = 1.D-6)
*
      DKEOSV = 1.D0
      If(DV.LE.0.D0 .OR. DV.GE.1.D0 .OR. DKP.LE.0.D0) Return
*
      DP0 = DPEOS(DV,DKP,MTYPE)
      DP1 = DPEOS(DV*(1.D0+DH),DKP,MTYPE)
*
      DKEOSV = (DP0 - DP1)/DH
*
*
      End
