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
      FUNCTION DPEOS(DV,DKP,MTYPE)
*
* purpose :  calculate pressure from volume using the
*             various equations of state;
* date    :  December 2, 2004
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DV        = volume divided by volume at zero pressure
*    MTYPE     = type of EoS to be used
*                  0 :  third-order Birch-Murnaghan
*                  1 :  logarithmic
*                  2 :  Vinet
*    DKP       = pressure derivative of bulk modulus
*
* export variables :
*    DPEOS     = pressure divided by bulk modulus
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      PARAMETER(D13 = 1.D0/3.D0, D23 = 2.D0/3.D0, D53 = 5.D0/3.D0)
*
      DPEOS = 0.D0
      IF(DV.LE.0.D0 .OR. DV.GT.1.D0 .OR. DKP.LE.0.D0) RETURN
*
      IF(MTYPE.EQ.0) THEN
       DJ    = 1.D0/DV**D23 - 1.D0
       DPEOS = 1.5D0*DJ/DV**D53*(1.D0 - 0.75D0*DJ*(4.D0-DKP))
      ENDIF
*
      IF(MTYPE.EQ.1) THEN
       DJ    = -DLOG(DV)
       DPEOS = DJ/DV*(1.D0 + (0.5D0*DKP-1.D0)*DJ)
      ENDIF
*
      IF(MTYPE.EQ.2) THEN
       DJ    = 1.D0 - DV**D13
       DPEOS = 3.D0*DJ/DV**D23 * DEXP(1.5D0*(DKP-1.D0)*DJ)
      ENDIF
*
*
      END
