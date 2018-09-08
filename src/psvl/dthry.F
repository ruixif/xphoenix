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
       Function DTHRY(NTYPE,DX,DP,N)
*
* purpose :  define theory function for fit procedure
* date    :  March 25, 2006
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NTYPE   = type of theory function:
*               0 = for Debye sound velocity extrapolation
*                    dp(1) - (x/dp(2))^dp(3)
*    N       =   0 return function value
*               >0 return 1. derivative of function with respect
*                  to fit parameter DP(N)
*               -1 return total number of parameters needed
*    DX      = x-value
*    DP(*)   = parameter values
*
* export variables :
*    DTHRY   = if N=0  function value
*              if N>0  1. derivative of function with respect
*                       to fit parameter DP(N)
*              if N=-1 total number of parameters needed
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Dimension  DP(*)
*
*
      DTHRY = 0.0
*
*:  define function for Debye sound velocity extrapolation
*
      If(NTYPE.EQ.0) Then
       If(N.EQ.-1) Then
        DTHRY = 3.D0
       ElseIf(N.EQ.0) Then
         If(DX.le.0.D0) Then
          DTHRY = DP(1)
         Else
          DTHRY = DP(1) - (DX/DP(2))**DP(3)
         Endif
        Else
         If(N.EQ.1) DTHRY = 1.D0
         If(DX.le.0.D0) Then
          If(N.EQ.2) DTHRY = 0.D0
          If(N.EQ.3) DTHRY = 0.D0
         Else
          If(N.EQ.2) DTHRY = DP(3)/DP(2)*(DX/DP(2))**DP(3)
          If(N.EQ.3) DTHRY = -Dlog(DX/DP(2))*(DX/DP(2))**DP(3)
         Endif
       Endif
      Endif
*
*
      END
