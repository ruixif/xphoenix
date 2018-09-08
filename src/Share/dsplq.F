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
      FUNCTION DSPLQ(DX,DX1,DY1,DX2,DY2,DX3,DY3)
*
* Purpose :  quadratic interpolation or extrapolation at DX
* Date    :  January 7, 1995
* Author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*:  calculate coefficients
*
      DE1 = DX2 - DX1
      DE2 = DX3 - DX2
*
      IF(DE1.EQ.0.D0 .AND. DE2.EQ.0.D0) THEN
       DSPLQ = (DY1+DY2+DY3)/3.D0
      ELSEIF(DE1.EQ.0.D0) THEN
*
        DYA = 0.5D0*(DY1+DY2)
        DSPLQ = DYA + (DX-DX1)/(DX3-DX1)*(DY3-DYA)
       ELSEIF(DE2.EQ.0.D0) THEN
*
         DYA = 0.5D0*(DY2+DY3)
         DSPLQ = DY1 + (DX-DX1)/(DX3-DX1)*(DYA-DY1)
        ELSEIF(DE1.EQ.-DE2) THEN
*
         DYA = 0.5D0*(DY1+DY3)
         DSPLQ = DY1 + (DX-DX1)/DE1*(DY2-DYA)
*
        ELSE
*
         DEE = DE1 + DE2
         DC  = DY2
         DB  = ( DE1*(DY3-DY2)/DE2 + DE2*(DY2-DY1)/DE1 )/DEE
         DA  = (     (DY3-DY2)/DE2 -     (DY2-DY1)/DE1 )/DEE
*
         DEX   = DX - DX2
         DSPLQ = DC + DEX*(DB + DEX*DA)
      ENDIF
*
*
      END
