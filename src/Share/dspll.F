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
      FUNCTION DSPLL(DX,DX1,DY1,DX2,DY2)
*
* Purpose :  linear interpolation or extrapolation at DX
* Date    :  March 25, 2001
* Author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      DE = DX2 - DX1
*
      IF(DE.EQ.0.D0) THEN
       DSPLL = 0.5D0*(DY1 + DY2)
      ELSE
       DSPLL = DY1 + (DX - DX1)*(DY2 - DY1)/DE
      ENDIF
*
*
      END
