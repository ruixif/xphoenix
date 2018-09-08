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
      Subroutine FILEXP
*
* purpose :  analyze file names and expand if a range
*            of files is specified
* date    :  August 21, 2005
* author  :  Wolfgang Sturhahn
*
*
* import variables :
*    NCL         = total number of input files
*    XINTYP      = data type of files containing measured data
*    XMES(*)     = file names + column specifiers, measured data
*
* export variables :
*    NCL         = total number of input files (updated)
*    XMES(*)     = file names + column specifiers, measured data
*
*---+----1----+----2----+----3----+----4----+----5----+----6----+----7->
*
#include<phoenix.h>
#include<padd.h>
*
      Character*72 XP(3),XMES9(PDS),XH,XFORM
*
*
*:  test if scan number was defined
*
      Call XADJ(XMES(1),' ')
      L   = Lenx(XMES(1),' ')
      LB0 = Index(' ',XMES(1))
      If(Index(XMES(1)(LB0:L),'#').eq.0) Then
       BSCAN = .false.
      Else
       BSCAN = .true.
      Endif
*
*:  input files without scan number
*
      If(.not.BSCAN) Then
       II = 0
       Do  I = 1,NCL
        XH = XMES(I)
        Call XPARSE(XH,' ',2,XP)
        L1 = Index(XP(1),'<')
        L2 = Index(XP(1),'>')
        If(L1.gt.0 .and. L2.gt.0 .and. L1.lt.L2-3) Then
         LD = Index(XP(1)(L1+1:L2-1),'-')
         If(LD.gt.1 .and. LD.lt.L2-L1-1) Then
          N1 = -1
          N2 = -1
          Read(XP(1)(L1+1:L1+LD-1),*,ERR=120,END=120) N1
  120     Read(XP(1)(L1+LD+1:L2-1),*,ERR=121,END=121) N2
  121     Continue
*
          LF = Max0(LD-1,L2-1-L1-LD)
          Write(XH,*) LF
          Call XADJ(XH,' ')
          XFORM = '(I'//XH(1:70)
          Call Xmerge(XFORM,'.'//XH)
          Call Xmerge(XFORM,')')
          LF = Lenx(XFORM,' ')
*
          If(N1.ge.0 .and. N2.ge.0) Then
           LM = LENX(XMES(I),' ')
           If(N2.ge.N1) Then
            NN1 = N1
            NN2 = N2
           Else
            NN1 = N2
            NN2 = N1
           Endif
           Do  N = NN1,NN2
            II = II + 1
            WRITE(XH,XFORM) N
            Call XADJ(XH,' ')
            LN = LENX(XH,' ')
            XMES9(II) = XP(1)(1:L1-1)//XH(1:LN)//XMES(I)(L2+1:LM)
           EndDo
          Else
           II = II + 1
           XMES9(II) = XMES(I)
          Endif
         Else
          II = II + 1
          XMES9(II) = XMES(I)
         Endif
        Else
         II = II + 1
         XMES9(II) = XMES(I)
        Endif
       EndDo
*
       NCL = Min0(PDS-14,II)
       Do  I = 1,NCL
        XMES(I) = XMES9(I)
       EndDo
      Endif
*
*:  input files with scan number
*
      If(BSCAN) Then
       II = 0
       Do  I = 1,NCL
        XH = XMES(I)
        Call XPARSE(XH,'#',2,XP)
        If(XP(2)(1:1).ne.' ') Then
         LP = LENX(XP(2),' ')
         LD = Index(XP(2)(1:LP),'-')
         LE = Index(XP(2)(1:LP),' ')
         If(LE.eq.0) Then
          LE = LP
         Else
          LE = LE - 1
         Endif
         If(LD.gt.1 .and. LD.lt.LE) Then
          N1 = -1
          N2 = -1
          Read(XP(2)(1:LD-1),*,ERR=122,END=122) N1
  122     Read(XP(2)(LD+1:LE),*,ERR=123,END=123) N2
  123     Continue
          If(N1.ge.0 .and. N2.ge.0) Then
           L1 = LENX(XP(1),' ')
           If(N2.ge.N1) Then
            NN1 = N1
            NN2 = N2
           Else
            NN1 = N2
            NN2 = N1
           Endif
           Do  N = NN1,NN2
            II = II + 1
            Write(XH,*) N
            Call XADJ(XH,' ')
            LN = LENX(XH,' ')
            XMES9(II) = XP(1)(1:L1)//' #'//XH(1:LN)//
     +                         XP(2)(LE+1:LP)
           EndDo
          Else
           II = II + 1
           XMES9(II) = XMES(I)
          Endif
         Else
          II = II + 1
          XMES9(II) = XMES(I)
         Endif
        Else
         II = II + 1
         XMES9(II) = XMES(I)
        Endif
       EndDo
*
       NCL = Min0(PDS-14,II)
       Do  I = 1,NCL
        XMES(I) = XMES9(I)
       EndDo
      Endif
*
*
      End
