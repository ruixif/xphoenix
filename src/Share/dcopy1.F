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
      SUBROUTINE DCOPY1(DA1,DA2,N)
*
* Purpose :  copy one dimensional arrays.
* Date    :  December 6, 1997
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DA1(*)   = array to be copied
*    N        = number of elements to be copied
*
* export variables :
*    DA2(*)   = copy of input array
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION  DA1(*),DA2(*)
*
*
      IF(N.LE.0) RETURN
*
      DO 1  I = 1,N
       DA2(I)  = DA1(I)
    1 CONTINUE
*
*
      END
