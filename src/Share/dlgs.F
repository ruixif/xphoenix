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
      SUBROUTINE DLGS(NDIM,DA,DB,NC,NS,DDET)
*
* purpose :  solve a system of linear equations for
*            several constant columns simultaneously
*            by the method of Gauss-Seidel
* date    :  27. february 1992
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NDIM     = dimension of matrix
*    NC       = nr. of lines/columns used for the calc.
*    NS       = nr. of constant columns to be used
*    DA(*,*)  = coefficient matrix
*    DB(*,*)  = matrix with the constants being columns
*
* export variables :
*    DDET     = determinant of the matrix DA in the
*                range given by NC
*    DA(*,*)  = matrix DA converted to triangular form
*    DB(*,*)  = matrix with columns being the solutions
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      DIMENSION DA(NDIM,NDIM),DB(NDIM,NDIM)
*
*:  test array subscripts
*
      IF(NC.LE.0.OR.NS.LE.0)  RETURN
      NCC = MIN0(NC,NDIM)
      NSS = MIN0(NS,NDIM)
*
*:  identify pivot to start with
*
      DDET  = 1.0
      DAMAX = DABS(DA(1,1))
      LP    = 1
      DO 11  I = 1, NCC
       IF(DABS(DA(I,1)).GT.DAMAX) THEN
        DAMAX = DABS(DA(I,1))
        LP    = I
       ENDIF
   11 CONTINUE
*
      JP1 = 1
      DO 12  J = 1, NCC
       IP = LP
       DT = DA(IP,J)
*
*:  column is zero
*
       IF(DT.EQ.0.0) RETURN
*
       IF(IP.NE.J) THEN
        DDET = -DDET*DT
       ELSE
        DDET =  DDET*DT
       ENDIF
*
       IF(J.LT.NCC) THEN
        DA(IP,J) = DA(J,J)
        JP1      = J + 1
        LP       = JP1
        DAMAX    = 0.0
*
        DO 13  K = JP1, NCC
         DH       = DA(IP,K)/DT
         DA(IP,K) = DA(J,K)
         DA(J,K)  = DH
*
         DO 14  I = JP1, NCC
          DA(I,K) = DA(I,K) - DA(I,J)*DH
          IF(K.LE.JP1.AND.DABS(DA(I,K)).GT.DAMAX) THEN
           DAMAX = DABS(DA(I,K))
           LP    = I
          ENDIF
   14    CONTINUE
   13   CONTINUE
       ENDIF
*
       DO 15  K = 1, NSS
        DH       = DB(IP,K)/DT
        DB(IP,K) = DB(J,K)
        DB(J,K)  = DH
        IF(J.LT.NCC) THEN
         DO 16  I = JP1, NCC
          DB(I,K) = DB(I,K) - DA(I,J)*DH
   16    CONTINUE
        ENDIF
   15  CONTINUE
*
   12 CONTINUE
*
      IF(NCC.GT.1) THEN
       DO 17  I = 2, NCC
        IP = NCC - I + 1
        LP = IP + 1
        DO 18  J = 1, NSS
         DO 19  K = LP, NCC
          DB(IP,J) = DB(IP,J) - DA(IP,K)*DB(K,J)
   19    CONTINUE
   18   CONTINUE
   17  CONTINUE
      ENDIF
*
*
      END
