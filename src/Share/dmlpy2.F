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
      Subroutine DMLPY2(DM1,DM2,DM3,ND,NR)
*
* purpose :  Multiply two matrices
*               DM3 = DM1*DM2
* date    :  December 22, 1995
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    ND       = size of the used matrix arrays
*    NR       = rank of the matrices (NR <= NDIM)
*    DM1,DM2  = matrices to be multiplied
*
* export variables :
*    DM3      = product of input matrices
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
*
      DIMENSION  DM1(ND,ND),DM2(ND,ND),DM3(ND,ND)
*
*:  quiet exits
*
      IF(NR.LE.0)  RETURN
      IF(NR.GT.ND) RETURN
*
*:  multiply
*
      DO 1  K = 1,NR
       DO 2  I = 1,NR
        DM3(I,K) = 0.D0
        DO 3  J = 1,NR
         DM3(I,K) = DM3(I,K) + DM1(I,J)*DM2(J,K)
    3   CONTINUE
    2  CONTINUE
    1 CONTINUE
*
*
      END
