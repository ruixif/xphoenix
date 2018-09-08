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
*  Free Software Foundation.
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
      Subroutine DCOPY2(DM1,DM2,ND,NR)
*
* Purpose :  copy a matrix.
* Date    :  April 23, 1998
* Author  :  Wolfgang Sturhahn
*
*
* import variables :
*    DM1(*,*) = matrix to be copied
*    ND       = size of the used matrix arrays
*    NR       = rank of the matrices (NR <= NDIM)
*
* export variables :
*    DM2(*,*) = copy of input matrix
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION  DM1(ND,ND),DM2(ND,ND)
*
*
*:  quiet exits
*
      IF(NR.LE.0)  RETURN
      IF(NR.GT.ND) RETURN
*
*:  copy
*
      DO 1  K = 1,NR
       DO 2  I = 1,NR
        DM2(I,K) = DM1(I,K)
    2  CONTINUE
    1 CONTINUE
*
*
      END
