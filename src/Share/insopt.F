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
      Subroutine INSOPT(BRUSE,BGRAPH,BPRT,XDIR,X)
*
* Purpose :  inspect line options of called executable
* Created :  August 11, 2009
* Author  :  Wolfgang Sturhahn
*
*
* import variables :  none
*
* export variables :  
*    BRUSE  = set to .TRUE.  if "--REUSE"   detected
*    BGRAPH = set to .FALSE. if "--NOGRAPH" detected
*    BPRT   = set to .TRUE.  if "--VERBOSE" detected
*    XDIR   = directory name where Phoenix executable resides
*    X      = last parameter without leading '--'
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) X,XDIR
      Character*300 XP
*
*:  defaults
*
      BRUSE  = .false.
      BGRAPH = .true.
      BPRT   = .false.
*
*:  nr. of parms
*
      KPAR = IARGC()
*
*:  test parms
*
      Do  K=1,KPAR
       Call GETARG(K,XP)
       If(XP(1:2).eq.'--') Then
        If(XP(3:6).eq.'dir=') Then
         L = Len(XDIR)
         Call XFILL(XDIR,1,L,' ')
         L = Min0(L,Len(XP))
         If(L.gt.6)  XDIR(1:L-6) = XP(7:L)
        Else
         Call XTOUP(XP)
         If(Index(XP,'REUSE')  .ne.0)  BRUSE  = .true.
         If(Index(XP,'NOGRAPH').ne.0)  BGRAPH = .false.
         If(Index(XP,'VERBOSE').ne.0)  BPRT   = .true.
        Endif
       Else
        L = LEN(X)
        Call XFILL(X,1,L,' ')
        L = Min0(L,Len(XP))
        If(L.ne.0) X(1:L) = XP(1:L)
       Endif
      EndDo
*
*
      End
