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
      FUNCTION DFLTR(MODE,DX,N)
*
* purpose :  provide value for a filter function with
*            f(0) = 1, f(1) = 0, f(x>1) = 0
* date    :  January 5, 1995
* author  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DFLTR = 0.D0
      IF(DX.LT.0.D0 .OR. DX.GE.1.D0) RETURN
*
*:  simple polynomials, order N
*
      IF(MODE.EQ.0) THEN
       IF(N.LT.1) RETURN
       DFLTR = 1.D0 - DX**N
      ENDIF
*
*
      END
