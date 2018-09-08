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
      SUBROUTINE EFFT(EX,N,ETW,NS,INV)
*
*
* PURPOSE :  Fast Fourier Transformation
*            Sande-Tukey radix 2 algorithm
*            original source code taken from
*              P.Bloomfield, Fourier Analysis
*              of Time Series: an Introduction,
*              John Wiley & Sons, New York, p.75
*            Introduction of work space yielded
*            three times faster execution
* DATE    :  29. July 1992
* PERSON  :  Wolfgang Sturhahn
*
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      PARAMETER( PMAX = 32 )
*
      DIMENSION EX(0:*),ETW(0:*),NS(*),
     +          DUR(PMAX-1),DUI(PMAX-1),EU(PMAX-1)
*
      DATA BFIRST/.TRUE./, NOLD/-1/, INVOLD/0/
      SAVE BFIRST,NOLD,INVOLD,DUR,DUI,EU
*
*:  test on index bounds
*
      IF(N.LE.0 .OR. N.GT.PMAX) GOTO 999
*
      N0 = 2**N
*
*:  first call or call with new N
*
      IF(NOLD.LT.N .OR. INVOLD.NE.INV .OR. BFIRST) THEN
       DUR(1) = 0.D0
       DUI(1) = 1.D0
       IF(INV.EQ.0) THEN
        EU(1)  = (0.D0,-1.D0)
       ELSE
        EU(1)  = (0.D0,1.D0)
       ENDIF
       DO 1  I = 2,PMAX-1
        DUR(I) = DSQRT((1.D0 + DUR(I-1))*0.5D0)
        DUI(I) = 0.5D0*DUI(I-1)/DUR(I)
        IF(INV.EQ.0) THEN
         EU(I)  = DCMPLX(DUR(I),-DUI(I))
        ELSE
         EU(I)  = DCMPLX(DUR(I),DUI(I))
        ENDIF
    1  CONTINUE
*
*:  calculate twiddle factors
*
       K0 = 1
       ETW(0) = (1.D0,0.D0)
       DO 13  I = N-1,1,-1
        DO 14  K = 0,K0-1
         ETW(K+K0) = ETW(K)*EU(I)
   14   CONTINUE
        K0 = K0 + K0
   13  CONTINUE
*
*:  calculate descrambling table
*
       K0 = 1
       DO 23  I = 0,N-1
        NS(K0) = N0/(K0+K0)
        DO 24  K = 1,K0-1
         NS(K+K0) = NS(K) + NS(K0)
   24   CONTINUE
        K0 = K0 + K0
   23  CONTINUE
*
       BFIRST = .FALSE.
       NOLD   = N
       INVOLD = INV
      ENDIF
*
*:  perform fourier transformation
*
      I1 = N0/2
      I3 = 1
      DO 2  I4 = 0,N-1
       DO 3  K = 0,I1-1
        KS = ISHFT(K,I4)
        L = K
        DO 5  J = 1,I3
         L1     = L + I1
         EZ     = EX(L) + EX(L1)
         EX(L1) = ETW(KS)*(EX(L) - EX(L1))
         EX(L)  = EZ
         L      = L1 + I1
    5   CONTINUE
    3  CONTINUE
       I3 = I3 + I3
       I1 = I1/2
    2 CONTINUE
*
*:  descramble data
*
      DO 6  J = 1,N0-2
       IF(NS(J).LT.J) GOTO 6
       EZ        = EX(J)
       EX(J)     = EX(NS(J))
       EX(NS(J)) = EZ
    6 CONTINUE
*
*:  normalize data
*
      DUM = 1.D0/DSQRT(DFLOAT(N0))
      DO 7  J = 0,N0-1
       EX(J) = DUM*EX(J)
    7 CONTINUE
*
      RETURN
*
*:  error exit
*
  999 CONTINUE
      INV = -1
*
*
      END
