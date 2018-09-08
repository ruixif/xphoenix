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
       Subroutine XINTPR(N,ND,XESC,XL,XS,NSUB)
*
* purpose :  simple command interpreter,
*             substitutes strings by
*             predefined values
* date    :  March 22, 1996
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    N        = nr. of lines of internal file XL
*    XESC     = escape sequence used for identification
*                of command lines and for substitution
*    XL(*)    = string array, regarded as internal file
*
* export variables :
*    ND       = nr. of lines after command lines stripped
*    XL(*)    = escaped strings are substituted
*    NSUB     = nr. of symbolic variables
*    XS(J)    = symbolic variable name, J odd
*                value of variable, J even
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<implici.h>
*
      Character*(*) XL(*),XESC,XS(*)
      Character*100 X(2),XV(2),XH
*
      External  LENX
*
*
      LI = Len(XL(1))
      LE = Len(XESC)
      LS = Len(XS(1))
      LT = Min0(100,LS)
*
      BT = LT.lt.LS
*
*:  max. length of symbolic names
*
      NSL = Min0(32,LI,LT)
*
*:  loop over all lines
*:  create a table containing symbolic names and values
*
      NSUB = 0
      Do  I = 1,N
       XH = XL(I)
       Call XADJ(XH,' ')
       If(XH(1:LE).ne.XESC) Cycle
*
*:  parse line
*
       XH = XL(I)
       Call XPARSE(XH,':=',2,X)
*
*:  remove leading Esc.char
*
       Call XADJ(X(1),XESC)
*
*:  remove leading blanks
*
       Call XADJ(X(1),' ')
       Call XADJ(X(2),' ')
*
*:  'blank' is not permitted as symbolic name
*
       If(X(1)(1:1).eq.' ') Cycle
*
*:  remove [..] from variable name
*
       L1 = Index(X(1),'[')
       L2 = Index(X(1),']')
       If(L1*L2.ne.0 .and. L2.gt.L1) Then
        X(1)(L1:LI-L2+L1-1) = X(1)(L2+1:LI)
        Call XFILL(X(1),LI-L2+L1,LI,' ')
        Call XADJ(X(1),' ')
       Endif
*
*:  look for variable name in subst. table
*:  if found, update entry
*
       BNEW = .true.
       Do  J = 1,NSUB
        JO = 2*J-1
        JE = 2*J
        If(X(1)(1:NSL).ne.XS(JO)(2:NSL+1)) Cycle
*
        Call XPARSE(X(2),' ',2,XV)
        XS(JE)(1:LT) = XV(1)(1:LT)
        If(BT) Call XFILL(XS(JE),LT+1,LS,' ')
        BNEW = .false.
        Exit
       EndDo
*
*:  create new entry
*
       If(BNEW) Then
        NSUB = NSUB + 1
        JO   = 2*NSUB-1
        JE   = 2*NSUB
        XS(JO)(1:NSL+LE) = XESC//X(1)(1:NSL)
        If(NSL+LE.lt.LS) Call XFILL(XS(JO),NSL+LE+1,LS,' ')
        Call XPARSE(X(2),' ',2,XV)
        XS(JE)(1:LT) = XV(1)(1:LT)
        If(BT) Call XFILL(XS(JE),LT+1,LS,' ')
       Endif
*
      EndDo
*
*:  in case we didn't find substitution directives
*
      If(NSUB.eq.0) Then
       ND = N
       Return
      Endif
*
*:  remove substitution directives from XL
*
      Call XSTRIP(N,ND,XESC,XL)
*
*:  check each line for occurence of symbolic names
*:  if found, substition will happen,
*:   chars trailing symbolic name may be lost
*
      Do  J = 1,NSUB
       JO  = 2*J-1
       JE  = 2*J
       LSN = LENX(XS(JO),' ')
       LVE = LENX(XS(JE),' ')
       Do  I = 1,ND
*
*:  replace multiple occurences per line
*
        Do  M = 1,10
         LFO = Index(XL(I),XS(JO)(1:LSN)//' ')
         If(LFO.eq.0) Exit
*
*:  replace symbolic name with value
*:  the length of defined chars in XL might change
*:  stop substitution if max.length would be exceeded
*
         LDC = LENX(XL(I),' ')
         If(LDC+LVE-LSN.gt.LI) Exit
*
         XH(1:LI-LFO-LSN+1) = XL(I)(LFO+LSN:LI)
         XL(I) = XL(I)(1:LFO-1)//XS(JE)(1:LVE)//XH(1:LI-LFO-LSN+1)
*
*:  pad with blanks
*
         If(LVE.lt.LSN) Call XFILL(XL(I),LI-LSN+LVE+1,LI,' ')
*
        EndDo
       EndDo
      EndDo
*
*
      End
