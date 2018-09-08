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
      SUBROUTINE XBASE(XIN,XOUT)
*
* purpose :  remove leading directory name and extension
* date    :  April 2, 2012
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    XIN   = file name with directory and extension
*
* export variables :
*    XOUT  = file base name, leading directories and trailing
*             extension removed
*
*
#include<implici.h>
*
      CHARACTER*(*) XIN,XOUT
*
      LIN  = Len(XIN)
      LOUT = Len(XOUT)
      Call XFILL(XOUT,1,LOUT,' ')
      L = Min0(LIN,LOUT)
*
*:  nothing to remove
*
      If(Index(XIN,'/').eq.0 .and. Index(XIN,'.').eq.0) Then
       XOUT(1:L) = XIN(1:L)
       Return
      Endif
*
*:  position of the '.' and the '/'
*
      LDOT = 0
      Do 1  J = LIN,1,-1
       If(XIN(J:J).eq.'.') Then
        LDOT = J
        GoTo 101
       Endif
    1 Continue
  101 Continue
*
      LSLASH = 0
      Do 2  J = LIN,1,-1
       If(XIN(J:J).eq.'/') Then
        LSLASH = J
        GoTo 102
       Endif
    2 Continue
  102 Continue
*
*:  remove extension, dirname
*
      If(LDOT.gt.1 .and. LDOT.gt.LSLASH+1) Then
       XOUT(1:LDOT-LSLASH-1) = XIN(LSLASH+1:LDOT-1)
       Return
      Endif
*
      If(LDOT.eq.0 .and. LSLASH.lt.L) Then
       XOUT(1:L-LSLASH) = XIN(LSLASH+1:L)
      Endif
*
      If(LDOT.eq.LSLASH+1) Then
       XOUT(1:L-LSLASH) = XIN(LSLASH+1:L)
      Endif
*
*
      END
