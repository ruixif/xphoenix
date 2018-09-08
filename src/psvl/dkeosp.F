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
      Function DKEOSP(DP,DKP,MTYPE)
*
* purpose :  calculate bulk modulus from pressure using
*             various equations of state;
* date    :  June 24, 2007
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DP        = pressure divided by bulk modulus at zero pressure
*    MTYPE     = type of EoS to be used
*                  0 :  third-order Birch-Murnaghan
*                  1 :  logarithmic
*                  2 :  Vinet
*    DKP       = pressure derivative of bulk modulus at zero pressure
*
* export variables :
*    DKEOSP    = bulk modulus divided by bulk modulus at zero pressure
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Parameter(DH = 1.D-6)
*
      DKEOSP = 1.D0
      If(DP.LE.0.D0 .OR. DKP.LE.0.D0) Return
*
      DV0 = DVEOS(DP,DKP,MTYPE)
      DV1 = DVEOS(DP*(1.D0+DH),DKP,MTYPE)
*
      DKEOSP = DV0*DP*DH/(DV0 - DV1)
*
*
      End
