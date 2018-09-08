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
       FUNCTION DTH(N,DX,DBETA,DP,DRRX,DRRY,KRD,BREXT)
*
* purpose :  define theory function for fit procedure
* date    :  December 30, 1997
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    N       =   0 return function value
*               >0 return 1. derivative of function with respect
*                  to fit parameter DP(N)
*               -1 return function value without inelastic background
*               -2 return function value without elastic peak
*    DX      = x-value
*    DBETA   = inverse temperature
*    DP(1)   = scaling factor of elastic peak
*    DP(2)   = ln2*(2/FWHM)**DP(7)
*    DP(3)   = position of elastic peak
*    DP(4)   = scaling factor of inelastic background
*    DP(5)   = not used
*    DP(6)   = asymmetry coefficient
*    DP(7)   = shape coefficient
*    DRRX(*) = x-value, resolution function
*    DRRY(*) = y-value, resolution function
*    KRD     = number of data points in res. file
*    BREXT   = if .FALSE. generate resolution function internally,
*                otherwise use table for external resolution function
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      PARAMETER( DWCV = 0.7213475D0, DISPI = 0.5641895835477563)
*
      DIMENSION  DP(*),DRRX(*),DRRY(*)
*
*
      DTH = 0.0
      DXX = DX - DP(3)
*
*:  define background function and derivative
*
      IF(DXX.NE.0.D0) THEN
       DH = DBETA*DXX
       IF(DH.GT.25.D0) THEN
        DBGF  = DH
        DDBGF = DBETA
       ELSEIF(DH.LT.-25.D0) THEN
         DBGF  = 0.D0
         DDBGF = 0.D0
        ELSE
         DH1   = DEXP(-DH)
         DBGF  = DH/(1.D0 - DH1)
         DDBGF = (1.D0 - DH1*(1.D0+DH))/(1.D0-DH1)**2
       ENDIF
      ELSE
       DBGF  = 1.D0
       DDBGF = 0.5D0
      ENDIF
*
      IF(N.EQ.-2) THEN
       DTH = DP(1)*DP(4)*DBGF
       RETURN
      ENDIF
*
      IF(N.EQ.4) THEN
       DTH = DP(1)*DBGF
       RETURN
      ENDIF
*
      IF(N.EQ.5) THEN
       DTH = 0.D0
       RETURN
      ENDIF
*
      IF(BREXT) THEN
*
*:  x-value for lookup table
*
       DXL = DXX*DWCV*DP(2)
*
*:  find index for this x-value
*
       IXL = IDNINT((DXL - DRRX(1))/(DRRX(2) - DRRX(1))) + 1
*
*:  determine y-value
*
       IF(IXL.LE.0 .OR. IXL.GT.KRD) THEN
        DYL = 0.D0
       ELSEIF(IXL.EQ.1 .OR. IXL.EQ.KRD) THEN
         DYL = DRRY(IXL)
        ELSE
         DYL = DSPLQ(DXL,DRRX(IXL-1),DRRY(IXL-1),
     +                   DRRX(IXL),  DRRY(IXL),
     +                   DRRX(IXL+1),DRRY(IXL+1) )
       ENDIF
*
       IF(N.EQ.-1) THEN
        DTH = DP(1)*DYL
        RETURN
       ENDIF
*
       IF(N.EQ.0) THEN
        DTH = DP(1)*(DYL + DP(4)*DBGF)
        RETURN
       ENDIF
*
       IF(N.EQ.1) THEN
        DTH = DYL + DP(4)*DBGF
        RETURN
       ENDIF
*
       IF(N.EQ.2 .OR. N.EQ.3) THEN
*
*:  determine derivative
*
        IF(IXL.LE.0 .OR. IXL.GT.KRD) THEN
         DDYL = 0.D0
        ELSEIF(IXL.EQ.1) THEN
          DDYL = (DRRY(2) - DRRY(1))/(DRRX(2) - DRRX(1))
         ELSEIF(IXL.EQ.KRD) THEN
           DDYL = (DRRY(KRD) - DRRY(KRD-1))/(DRRX(2) - DRRX(1))
          ELSE
           DDYL = 0.5D0*(DRRY(IXL+1) - DRRY(IXL-1))/(DRRX(2) - DRRX(1))
        ENDIF
*        
        IF(N.EQ.2) THEN
         DTH = DP(1)*DDYL*DXX*DWCV
        ELSE
         DTH = -DP(1)*(DDYL*DP(2)*DWCV + DP(4)*DDBGF)
        ENDIF
*
        RETURN
       ENDIF
*
      ELSE
*
*:  define asymmetric, modified Gaussian.
*
*:  calculate normalized first moment of resolution function
*:  shift x-scale to eliminate first moment
*
       IF(DP(6).NE.1.D0) THEN
        DIP7 = 1.D0/DP(7)
        DM1  = (1.D0-DP(6))/(1.D0+DP(6)) * (4.D0/DP(2))**DIP7 *
     +            DISPI/DINVGM(DIP7+0.5D0)
        DXX  = DXX + DM1
       ENDIF
*
       IF(DXX.GE.0.D0) THEN
        DP2 = DP(2)*(0.5D0*DP(6)+0.5D0)**DP(7)
       ELSE
        DP2 = DP(2)*(0.5D0/DP(6)+0.5D0)**DP(7)
       ENDIF
*
       IF(N.EQ.-1) THEN
        D1  = DP2*DABS(DXX)**DP(7)
        IF(D1.GT.25.) THEN
         DTH = 0.D0
        ELSE
         DTH = DP(1)*DEXP(-D1)
        ENDIF
        RETURN
       ENDIF
*
       IF(N.EQ.0) THEN
        D1  = DP2*DABS(DXX)**DP(7)
        IF(D1.GT.25.) THEN
         DTH = DP(1)*DP(4)*DBGF
        ELSE
         DTH = DP(1)*(DEXP(-D1) + DP(4)*DBGF)
        ENDIF
        RETURN
       ENDIF
*
       IF(N.EQ.1) THEN
        D1  = DP2*DABS(DXX)**DP(7)
        IF(D1.GT.25.) Then
         DTH = DP(4)*DBGF
        Else
         DTH = DEXP(-D1) + DP(4)*DBGF
        Endif
        RETURN
       ENDIF
*
       IF(N.EQ.2) THEN
        D1  = DP2*DABS(DXX)**DP(7)
        IF(D1.GT.25.) RETURN
        DTH = -D1/DP2*DP(1)*DEXP(-D1)
        RETURN
       ENDIF
*
       IF(N.EQ.3) THEN
        D1  = DP2*DABS(DXX)**DP(7)
        IF(D1.GT.25.) Then
         DTH = -DP(1)*DP(4)*DDBGF
        Else
         IF(DXX.NE.0.D0) THEN
          DTH = DP(1)*DP2*DP(7)*DEXP(-D1)*
     +          DSIGN(DABS(DXX)**(DP(7)-1.D0),DXX) -
     +           DP(1)*DP(4)*DDBGF
         ELSE
          DTH = -DP(1)*DP(4)*DDBGF
         ENDIF
        Endif
        RETURN
       ENDIF
*
      ENDIF
*
*
      END
